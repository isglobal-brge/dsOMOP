# Numeric computations remain private until the existing final publication gate.
.numberSourceUnit <- function(ctx,node) {
  d<-node$derived;r<-node$reducer;fn<-r$fn %||% ""
  if(identical(d$kind,"number_derive")) {
    u<-d$unit
    return(if(identical(d$reducer$fn,"cv"))"1" else if(identical(d$reducer$fn,"slope"))paste0(u,"/days") else u)
  }
  if(fn %in% c("binary","any","all","abnormal_high","abnormal_low","cv")||identical(d$kind,"died"))return("1")
  if(fn %in% c("count","n_distinct","era_count")||(d$kind %||% "") %in% c("number_count","n_observation_periods"))return("count")
  if(fn %in% c("days_since","days_until","duration_sum","covered_days","supply_days","drug_duration","gap_max","gap_mean","era_days","first_era_days")||(d$kind %||% "") %in% c("observed_days","days_to_death","prior_obs","followup","obs_duration"))return("days")
  if(fn=="time_since")return(if(identical(r$unit,"month"))"months" else "days")
  if((d$kind %||% "") %in% c("time_between","age_at"))return(if(d$unit=="annual_years")"years" else d$unit)
  if(identical(node$date_spec$precision,"relative_days"))return("days")
  source<-node$source %||% d$source
  ev<-ctx$ir$events[[source$event %||% ""]]
  u<-if(!is.null(ev$unit))paste0("unit:",ev$unit$target) else if((ev$table %||% "") %in% c("measurement","observation"))"unresolved_source" else "1"
  if(!is.null(ev$cost))u<-if(!is.null(ev$cost$currency))paste0("currency:",ev$cost$currency) else "unresolved_source"
  if(identical(d$kind,"number_change")) {
    if(d$method=="percent_change")return("percent")
    if(d$method=="ratio")return("1")
    if(d$method=="time_to_threshold")return(d$time_unit %||% "days")
    if(d$method=="slope")return(paste0(u,"/",d$time_unit %||% "days"))
  }
  if(fn=="slope")paste0(u,"/days") else u
}
.numberOffsetAllowed <- function(node) {
  fn<-node$reducer$fn %||% node$derived$reducer$fn %||% ""
  !fn %in% c("sum","count","n_distinct","sd","cv","slope","duration_sum","covered_days","supply_days","drug_duration","gap_max","gap_mean","era_count","era_days","first_era_days","days_since","days_until","time_since") &&
    !(node$derived$kind %||% "") %in% c("number_count","number_change","time_between","age_at","observed_days","obs_duration","prior_obs","followup")
}
.numberAnalyticColumn <- function(c) c$type %in% c("integer","bigint","numeric") && c$role %in% c("measure","value") && !isTRUE(attr(c,"concept")) && !isTRUE(attr(c,"number_structural"))
.numberRecord <- function(record,node,ctx,name,cons) {
  spec<-node$number_spec
  if(is.null(spec)&&is.null(ctx$ir$options$number_spec)&&!(node$derived$kind %||% "") %in% .NUMBER_KINDS)return(record)
  for(col in names(record$columns)) {
    c<-record$columns[[col]]
    if(!.numberAnalyticColumn(c)) {
      if(!is.null(spec)) .numberReject("Explicit number_spec requires a numeric analytic source; Date/factor/concept IDs are not numbers.","number_type_unsupported")
      next
    }
    unit<-.numberSourceUnit(ctx,node)
    attr(c,"number_origin")<-list(unit=unit,spec=spec,source=node$source,derived=node$derived,reducer=node$reducer,
      available=isTRUE(record$available),offset_allowed=.numberOffsetAllowed(node))
    temporal_reducers<-c("days_since","days_until","time_since","duration_sum","covered_days","supply_days","drug_duration","gap_max","gap_mean","era_days","first_era_days","slope")
    temporal_derived<-c("number_change","time_between","age_at","observed_days","days_to_death","prior_obs","followup","obs_duration","age")
    if((node$derived$kind %||% "") %in% temporal_derived||(node$reducer$fn %||% "") %in% temporal_reducers)attr(c,"privacy_class")<-2L
    record$columns[[col]]<-c
  }
  evaluate<-record$evaluate
  numeric_columns<-names(record$columns)[vapply(record$columns,.numberAnalyticColumn,logical(1))]
  record$evaluate<-function() {
    frame<-evaluate()
    for(col in numeric_columns) {
      if(record$columns[[col]]$type=="bigint")frame[[col]]<-bit64::as.integer64(frame[[col]])
      if(identical(node$reducer$fn,"cv"))frame[[col]]<-frame[[col]]/100
    }
    frame
  }
  record
}
.numberRangeValue <- function(x,low,high) {
  attr(x,"number_ranges")<-list(low=low,high=high)
  class(x)<-c("dsomop_number_value",class(x));x
}
#' @export
`[.dsomop_number_value` <- function(x,i,...) {
  r<-attr(x,"number_ranges");out<-NextMethod("[")
  absent <- missing(i)
  if(!is.null(r))attr(out,"number_ranges")<-lapply(r,function(v)if(absent)v else v[i])
  out
}
.numberUnwrap <- function(x) {
  if(inherits(x,"dsomop_number_value"))class(x)<-setdiff(class(x),"dsomop_number_value")
  attr(x,"number_ranges")<-NULL;x
}
.numberRangeSelection <- function(rows,keys,r,ordering,anchors) {
  rows$.number_index<-seq_len(nrow(rows))
  if((r$missing %||% "skip")=="skip")rows<-rows[!is.na(rows$v),,drop=FALSE]
  rows$v<-rows$.number_index
  .reduceApply(r$fn,rows,keys,r,ordering,anchors,"numeric")
}
.numberEventRecord <- function(ctx,node,name,cons) {
  d<-node$derived;src<-.reduceSource(ctx,d$source)
  res<-.predicateResolve(src$catalogue,d$value,"numeric value")
  if(!res$type %in% c("integer","bigint","numeric")||isTRUE(res$concept)||res$column %in% c("event_id","person_id","visit_id","unit_concept_id","operator_concept_id")) .numberReject("Select a registered numeric analytic role, never an identifier or concept.","number_source_invalid")
  rows_mode<-d$kind=="number_rows"
  type<-if(rows_mode)res$type else if(d$kind=="number_count"||d$method=="time_to_threshold"&&(d$time_unit %||% "days")=="days")"integer" else "numeric"
  columns<-setNames(list(.fragmentColumn(type,TRUE,"measure")),name)
  if(!src$available) {
    rec<-.reduceUnavailable(ctx,cons,name,columns);rec$event_grain<-rows_mode;rec$event_source<-d$source
    if(rows_mode)rec$evaluate<-function() {
      out<-data.frame(row_key=character(),event_id=character(),source_ns=character(),start_date=as.Date(character()))
      out[[name]]<-.reduceTypedNA(type,0L);out
    }
    return(rec)
  }
  extra<-list(v=list(expr=paste0("e.",res$column),type=res$type))
  paired<-rows_mode&&d$value %in% c("value","value_as_number")&&all(c("range_low","range_high") %in% names(src$fragment$columns))
  if(paired)for(k in c("range_low","range_high"))extra[[k]]<-list(expr=paste0("e.",k),type="numeric")
  stream<-.reduceStream(ctx,cons,src,d$window,.reduceName(ctx,cons,"s",name),extra=extra,dated=d$kind=="number_change")
  ordering<-.reduceSourceOrdering(src)
  evaluate<-function() {
    roster<-.reduceRosterFrame(ctx,cons$roster);keys<-roster$cohort_row_id
    got<-.reduceStreamRows(ctx,cons,stream,d$window,"numeric source")
    rows<-got$rows;if(res$type=="bigint")rows$v<-bit64::as.integer64(rows$v)
    rows$v<-.numberClean(rows$v)
    rows<-rows[.reduceOrderRows(rows,c("row_key",intersect(ordering,names(rows)))),,drop=FALSE]
    if(rows_mode) {
      out<-rows[intersect(c("row_key","event_id","source_ns","start_date","end_date"),names(rows))]
      out[[name]]<-if(paired).numberRangeValue(rows$v,rows$range_low,rows$range_high) else rows$v
      return(out)
    }
    groups<-.reduceGroups(rows$row_key,keys)
    origins<-NULL
    if(identical(d$method,"time_to_threshold")&&!(is.list(d$reference)&&identical(d$reference$kind,"first_observation")))origins<-.dateExact(.dateSourceValues(ctx,cons,d$reference,.dateEffective()))
    v<-vapply(seq_along(groups),function(i) {
      if(!got$bounds$defined[i])return(NA_real_)
      r<-rows[groups[[i]],,drop=FALSE]
      if(d$kind=="number_count")return(sum(if(d$count_kind=="missing")is.na(r$v) else !is.na(r$v)))
      .numberChangeValues(r,d,if(!is.null(origins))origins[i] else NULL)
    },numeric(1))
    out<-data.frame(row_key=keys);out[[name]]<-.reduceCoerce(v,type);out
  }
  list(stream=stream,reducer=NULL,columns=columns,evaluate=evaluate,available=TRUE,
    declared_empty_value=if(d$kind=="number_count")0L else NA,event_grain=rows_mode,event_source=d$source)
}
.numberDerivedRecord <- function(ctx,node,name,cons) {
  d<-node$derived
  if((d$kind %||% "") %in% c("number_rows","number_count","number_change"))return(.numberEventRecord(ctx,node,name,cons))
  if(d$kind=="number_column") .numberReject("number_column is valid only inside output number_columns.","number_dependency_invalid")
  inputs<-d$inputs
  # Dependencies are ordinary declarations, never published-symbol lookups.
  records<-lapply(inputs,function(i) {
    n<-.numberDependencyName(name,i$alias);v<-i$variable;attr(v,"name")<-n
    rec<-.compileVariable(ctx,v,cons$roster,cons)
    if(length(rec$columns)!=1L) .numberReject("Each input needs one scalar; select one nth/quantile explicitly.","number_dependency_invalid")
    rec
  })
  event<-vapply(records,function(r)isTRUE(r$event_grain),logical(1))
  if(any(event)&&!all(event)&&any(!vapply(inputs[!event],function(i)isTRUE(i$variable$broadcast),logical(1)))) .numberReject("Scalar-to-event inputs require explicit broadcast.","number_dependency_invalid")
  event_mode<-any(event)
  if(event_mode) {
    ids<-vapply(records[event],function(r).jcsCanonical(r$event_source),"")
    if(length(unique(ids))!=1L) .numberReject("Event operands require identical source/event identity and normalisation.","number_dependency_invalid")
  } else if(!is.null(d$reducer)) .numberReject("Only event operands can be reduced after an expression.","number_dependency_invalid")
  empty<-setNames(lapply(records,function(r).reduceTypedNA(r$columns[[1]]$type,0L)),vapply(inputs,function(i)i$alias,""))
  probe<-.numberExpression(d$exprs,as.data.frame(empty),d$result)
  expression_type<-if(inherits(probe,"integer64"))"bigint" else if(is.integer(probe))"integer" else "numeric"
  r<-d$reducer;type<-if(is.null(r))expression_type else .reduceValueType(r$fn,expression_type)
  out_names<-if(is.null(r))name else .reduceColumnNames(name,r)
  columns<-setNames(lapply(out_names,function(n).fragmentColumn(type,TRUE,"measure")),out_names)
  privacy<-max(c(3L,unlist(lapply(records,function(rec) attr(rec$columns[[1]],"privacy_class")))))
  if(any(vapply(records,function(rec)identical(attr(rec$columns[[1]],"privacy_class"),2L),logical(1))))privacy<-2L
  for(n in names(columns))attr(columns[[n]],"privacy_class")<-privacy
  available<-all(vapply(records,function(r)isTRUE(r$available),logical(1)))
  evaluate<-function() {
    vals<-lapply(records,function(r)r$evaluate());base<-vals[[if(event_mode)which(event)[1L] else 1L]]
    keys<-if(event_mode)intersect(c("row_key","source_ns","event_id","start_date"),names(base)) else "row_key"
    identity<-function(x)do.call(paste,c(lapply(x[keys],as.character),sep="\r"))
    ids<-identity(base);frame<-data.frame(row.names=seq_len(nrow(base)))
    for(j in seq_along(inputs)) {
      broadcast<-event_mode&&!event[j]
      at<-if(broadcast)match(base$row_key,vals[[j]]$row_key) else match(ids,identity(vals[[j]]))
      if(anyNA(at)||!broadcast&&(length(at)!=nrow(vals[[j]])||anyDuplicated(at))||broadcast&&anyDuplicated(vals[[j]]$row_key)) .numberReject("Numeric operands lack a one-to-one row identity.","number_dependency_invalid")
      rec<-records[[j]];col<-names(rec$columns)[1L];meta<-attr(rec$columns[[1]],"number_origin")
      x<-vals[[j]][[col]][at];s<-inputs[[j]]$variable$number_spec %||% list()
      x<-.numberPresent(.numberUnwrap(x),s,unit=meta$unit %||% .numberSourceUnit(ctx,inputs[[j]]$variable),ranges=attr(x,"number_ranges"),available=rec$available,offset_allowed=meta$offset_allowed %||% TRUE,canonical=inputs[[j]]$stage=="canonical")
      if(!is.numeric(x)||is.factor(x)||inherits(x,"Date")) .numberReject("Presented expression operands must remain numeric; select a numeric sibling.","number_type_unsupported")
      frame[[inputs[[j]]$alias]]<-x
    }
    value<-.numberExpression(d$exprs,frame,d$result)
    if(!available)value[]<-NA
    if(is.null(r)) {out<-base[keys];out[[name]]<-value;return(out)}
    rows<-base;rows$v<-value
    bounds<-.reduceBoundsR(ctx,cons,r$window,"numeric expression window")
    rows<-.reduceWindowFilter(rows,bounds)
    roster<-.reduceRosterFrame(ctx,cons$roster);out<-data.frame(row_key=roster$cohort_row_id)
    anchor<-if(r$fn=="closest_value").reduceAnchorDates(ctx,cons,.reduceAnchor(r)) else NULL
    v<-.reduceApply(r$fn,rows,roster$cohort_row_id,r,c("start_date","source_ns","event_id"),anchor,expression_type)
    for(j in seq_along(columns)) {x<-.reduceCoerce(v[[j]],type);if(r$fn=="cv")x<-x/100;x[!bounds$defined|!available]<-NA;out[[names(columns)[j]]]<-x}
    out
  }
  list(stream=NULL,reducer=NULL,columns=columns,evaluate=evaluate,available=available,declared_empty_value=NA,
    event_grain=event_mode&&is.null(r),event_source=if(event_mode)records[[which(event)[1L]]]$event_source else NULL)
}
.numberMatchEventRows <- function(rows,values) {
  keys<-intersect(c("row_key","event_id","source_ns"),intersect(names(rows),names(values)))
  if(!all(c("row_key","event_id") %in% keys)) .numberReject("Event numeric columns require existing source-qualified event identity.","number_dependency_invalid")
  key<-function(x)do.call(paste,c(lapply(x[keys],as.character),sep="\r"))
  ids<-key(values)
  if(anyDuplicated(ids)) .numberReject("Numeric event inputs require one-to-one row identity.","number_dependency_invalid")
  match(key(rows),ids)
}

# Forks add source normalization but never rename a declared output source key.
.numberLogicalSource <- function(ctx,name) {
  if(!grepl("^num[0-9a-f]{24}$",name))return(name)
  ev<-ctx$ir$events[[name]]
  candidates<-setdiff(names(ctx$ir$events),grep("^num[0-9a-f]{24}$",names(ctx$ir$events),value=TRUE))
  same<-Filter(function(n) {
    key<-paste0("num",substr(as.character(openssl::sha256(charToRaw(.jcsCanonical(list(source=n,event=ev))))),1L,24L))
    identical(key,name)
  },candidates)
  if(length(same)==1L)same[[1L]] else name
}

.numberOutputCovariates <- function(ctx,node,inputs,co) {
  if(node$format %in% c("wide","long","intervals","joined"))return(co)
  vars<-setdiff(.irOutputVariables(ctx$ir,.compileNodeName(node,"output")),.dateOutputVariables(ctx,node))
  vars<-vars[vapply(ctx$ir$variables[vars],function(v)!is.null(v$number_spec)||(v$derived$kind %||% "") %in% .NUMBER_KINDS,logical(1))]
  if(!length(vars))return(co)
  if(node$format %in% c("sparse","temporal_covariates","dictionary","membership")) .numberReject("Use a typed wide companion for numeric variable columns in this format.","number_type_unsupported")
  for(v in vars)if((ctx$ir$variables[[v]]$derived$kind %||% "") %in% c("number_rows","number_derive")&&is.null(ctx$ir$variables[[v]]$derived$reducer)) {
    # Scalar derives are checked by their compiled dependency grain below.
    if(identical(ctx$ir$variables[[v]]$derived$kind,"number_rows")) .numberReject("Event rows need an explicit reducer for roster-level covariates.","number_dependency_invalid")
  }
  .dateOutputCovariates(ctx,node,inputs,co,vars)
}
