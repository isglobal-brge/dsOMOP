# Native date sources are evaluated on private typed materialisations; no SQL
# date constructor narrows the 0001..9999 domain and no patient rows leave here.
.dateSourceValues <- function(ctx,cons,source,s) {
  roster<-.reduceRosterFrame(ctx,cons$roster);n<-nrow(roster)
  if(is.list(source)&&identical(source$kind,"date_components")) {
    absent <- rep(FALSE,n)
    operands<-setNames(lapply(c("year","month","day"),function(k)source[[k]]),c("year","month","day"))
    cols<-unique(vapply(Filter(function(x)!is.null(x)&&identical(x$kind,"column"),operands),function(x)x$name,""))
    if(is.null(source$event)) {
      # Person columns use the registered catalogue and the same class-1 guard.
      have<-.reduceTableColumns(ctx,"person")
      for(col in cols) {
        row<-have[match(col,tolower(have$column_name)),,drop=FALSE]
        if(!col %in% tolower(have$column_name) || (.assignmentBlockedColumn(col, have) || col %in% .identifierColumns() || col %in% .PERSON_KEY_COLS())) .dateReject("Component source is not a permitted registered integer column.","date_source_invalid")
      }
      for(col in cols) {
        row<-have[match(col,tolower(have$column_name)),,drop=FALSE]
        if(!grepl("int",row$db_datatype,ignore.case=TRUE)) .dateReject("Component source columns must have integer database type.","date_source_invalid")
      }
      p<-.reducePersonFrame(ctx,cons,extra_columns=cols)
      data<-p[match(as.character(roster$cohort_row_id),as.character(p$row_key)),,drop=FALSE]
    } else {
      src<-.reduceSource(ctx,source$event)
      extra<-list()
      for(col in cols) {
        res<-tryCatch(.predicateResolve(src$catalogue,col,"date components"),error=function(e) .dateReject("Component source is not a permitted registered integer column.","date_source_invalid"))
        if(!res$type %in% c("integer","bigint")) .dateReject("Component columns must have integer type.","date_source_invalid")
        extra[[col]]<-list(expr=paste0("e.",res$column),type=res$type)
      }
      if(!src$available) return(list(lo=as.Date(rep(NA_character_,n)),hi=as.Date(rep(NA_character_,n))))
      stream<-.reduceStream(ctx,cons,src,source$search,.reduceName(ctx,cons,"dc",.dateSourceKey(source)),extra=extra)
      rows<-.reduceStreamRows(ctx,cons,stream,source$search,"date components")$rows
      ordering<-.reduceSourceOrdering(src);rows<-rows[.reduceOrderRows(rows,c("row_key",intersect(ordering,names(rows)))),,drop=FALSE]
      if(identical(source$scope,"episode")) {
        if(is.null(cons$spec$op_start)||is.null(cons$spec$op_end)||!all(c(cons$spec$op_start,cons$spec$op_end) %in% names(roster))) .dateReject("Episode date components require an episode roster.","date_source_invalid")
        at<-match(as.character(rows$row_key),as.character(roster$cohort_row_id))
        ok<-!is.na(rows$start_date)&rows$start_date>=roster[[cons$spec$op_start]][at]&rows$start_date<=roster[[cons$spec$op_end]][at]
        rows<-rows[which(ok),,drop=FALSE]
      }
      groups<-.reduceGroups(rows$row_key,roster$cohort_row_id)
      at<-vapply(groups,function(ix) { if(!length(ix)) return(NA_integer_);k<-switch(source$which,first=1L,last=length(ix),nth=source$n);if(k>length(ix))NA_integer_ else ix[k]},1L)
      data<-rows[at,,drop=FALSE];absent<-is.na(at)
    }
    values<-lapply(operands,function(x) {
      if(is.null(x)) return(rep(NA_integer_,n))
      if(x$kind=="integer") return(rep(x$value,n))
      v<-data[[x$name]]
      if(!is.numeric(v)&&!inherits(v,"integer64")) .dateReject("Component columns must have integer type.","date_source_invalid")
      as.numeric(v)
    })
    values$year[absent]<-NA
    return(.dateComponents(values$year,values$month,values$day,s))
  }
  if(is.list(source)&&!is.null(source$date)) {
    if(!.dateISO(source$date)) .dateReject("Invalid fixed date source.","date_source_invalid")
    d<-rep(as.Date(source$date),n)
  } else {
    # Missing builtin anchors are missing values, not a reason to lose a person.
    if(is.character(source)&&! .ANCHOR_BUILTINS[[source]] %in% names(roster)) d<-as.Date(rep(NA_character_,n)) else d<-.reduceAnchorDates(ctx,cons,source,"date source")
  }
  d[!is.na(d)&(d<as.Date("0001-01-01")|d>as.Date("9999-12-31"))]<-NA
  if(s$future=="na") d[!is.na(d)&d>as.Date(s$as_of)]<-NA
  list(lo=d,hi=d)
}
.dateBirthSource <- function() list(kind="date_components",year=list(kind="column",name="year_of_birth"),month=list(kind="column",name="month_of_birth"),day=list(kind="column",name="day_of_birth"),which="first",n=1L,scope="person")
.dateExact <- function(x) { d<-x$lo;d[is.na(x$hi)|d!=x$hi]<-NA;d }
.dateResultType <- function(s,d) {
  if(d$kind %in% c("time_between","age_at")) return(if(isTRUE(d$bands))"text" else if(d$unit %in% c("weeks","decimal_years"))"numeric" else "integer")
  probe<-list(lo=as.Date("2024-02-29"),hi=as.Date("2024-02-29"))
  val<-.datePresent(probe,s,d$part,as.Date("1970-01-01"))
  if(inherits(val,"Date"))"date" else if(is.factor(val))"text" else if(is.integer(val))"integer" else "numeric"
}
.reduceDateVariable <- function(ctx,node,name,cons) {
  d<-node$derived;s<-node$date_spec %||% .dateEffective(NULL,ctx$ir$options$date_spec,d$kind,d$part)
  type<-.dateResultType(s,d);column<-.fragmentColumn(type,TRUE,"measure")
  attr(column,"date_spec")<-s;attr(column,"privacy_class")<-2L
  attr(column,"date_operation")<-d[intersect(names(d),c("kind","part","unit","bands","age_breaks"))]
  evaluate<-function() {
    roster<-.reduceRosterFrame(ctx,cons$roster)
    if(d$kind %in% c("calendar_date","date_part")) {
      source<-.dateSourceValues(ctx,cons,d$reference,s)
      anchor<-if(identical(s$precision,"relative_days")) .dateExact(.dateSourceValues(ctx,cons,s$anchor,s)) else NULL
      value<-.datePresent(source,s,d$part,anchor)
    } else {
      a<-.dateSourceValues(ctx,cons,if(d$kind=="age_at") .dateBirthSource() else d$from,s)
      b<-.dateSourceValues(ctx,cons,if(d$kind=="age_at") d$reference else d$to,s)
      from<-.dateExact(a);to<-.dateExact(b)
      if(d$kind=="age_at"&&d$unit=="annual_years") {
        value<-.dateY(to)-.dateY(a$lo);value[!is.na(to)&!is.na(a$lo)&to<a$lo]<-NA_integer_
      } else value<-.dateElapsed(from,to,d$unit,isTRUE(d$absolute),d$rounding %||% "none")
      if(d$kind=="age_at") {
        value[!is.na(value)&value<0]<-NA
        if(isTRUE(d$bands)) {
          breaks<-as.integer(unlist(d$age_breaks));labels<-c(paste0(head(breaks,-1),"-",tail(breaks,-1)-1L),paste0(tail(breaks,1),"+"))
          keys<-as.character(cut(value,c(breaks,Inf),right=FALSE,labels=labels))
          value<-.dateFactor(keys,labels,s)
        }
      }
    }
    out<-data.frame(row_key=roster$cohort_row_id);out[[name]]<-value;out
  }
  list(stream=NULL,reducer=NULL,declared_empty_value=NA,columns=setNames(list(column),name),evaluate=evaluate,available=TRUE)
}

.datePresentRecord <- function(record,node,ctx,name,cons) {
  if(is.null(node$date_spec)||(node$derived$kind %||% "") %in% .DATE_KINDS) return(record)
  if(!all(vapply(record$columns,function(c) identical(c$type,"date"),logical(1)))) .dateReject("date_spec requires a date-valued source; numeric measures are not timestamps.")
  s<-node$date_spec;old<-record$evaluate
  record$evaluate<-function() {
    out<-old();anchor<-if(identical(s$precision,"relative_days")) .dateExact(.dateSourceValues(ctx,cons,s$anchor,s)) else NULL
    for(col in names(record$columns)) out[[col]]<-.datePresent(list(lo=out[[col]],hi=out[[col]]),s,anchor=anchor)
    out
  }
  for(col in names(record$columns)) {
    record$columns[[col]]$type<-.dateResultType(s,list(kind="calendar_date"))
    attr(record$columns[[col]],"date_spec")<-s;attr(record$columns[[col]],"privacy_class")<-2L
  }
  record
}

.dateSourceKey <- function(source) substr(as.character(openssl::sha256(charToRaw(.jcsCanonical(source)))),1L,16L)
.dateComponentSources <- function(x) {
  if(!is.list(x)) return(list())
  if(identical(x$kind,"date_components")) return(list(x))
  unlist(lapply(x,.dateComponentSources),recursive=FALSE)
}
.datePersonColumns <- function(ir) {
  sources<-Filter(function(x)is.null(x$event),.dateComponentSources(ir))
  unique(unlist(lapply(sources,function(x) unlist(lapply(x[c("year","month","day")],function(v)if(identical(v$kind,"column"))v$name))),use.names=FALSE))
}

.dateApplyColumns <- function(frame,columns,s,index=NULL,role_anchor=NULL) {
  if(is.null(s)) return(list(frame=frame,columns=columns))
  s<-.dateEffective(NULL,s)
  dates<-names(columns)[vapply(columns,function(x)identical(x$type,"date")&&is.null(attr(x,"date_spec")),logical(1))]
  # Paired displayed interval endpoints are invalid together; this is after
  # eligibility, interval arithmetic and censoring have already finished.
  starts<-dates[vapply(columns[dates],function(x)identical(x$role,"start"),logical(1))]
  ends<-dates[vapply(columns[dates],function(x)identical(x$role,"end"),logical(1))]
  if(length(starts)==1L&&length(ends)==1L) {
    bad<-!is.na(frame[[starts]])&!is.na(frame[[ends]])&frame[[ends]]<frame[[starts]]
    frame[[starts]][bad]<-NA;frame[[ends]][bad]<-NA
  }
  for(col in dates) {
    d<-frame[[col]];if(!inherits(d,"Date")) d<-as.Date(d)
    if(s$future=="na") d[!is.na(d)&d>as.Date(s$as_of)]<-NA
    anchor<-NULL
    if(identical(s$precision,"relative_days")) {
      if(!is.null(role_anchor)) anchor<-role_anchor
      else if(is.list(s$anchor)&&!is.null(s$anchor$date)) anchor<-as.Date(s$anchor$date)
      else if(identical(s$anchor,"index")) anchor<-index
      else .dateReject("Output date roles need an index or fixed anchor; use a date helper for an event anchor.")
      if(is.null(anchor)) anchor<-as.Date(rep(NA_character_,length(d)))
    }
    frame[[col]]<-.datePresent(list(lo=d,hi=d),s,anchor=anchor)
    columns[[col]]$type<-.dateResultType(s,list(kind="calendar_date"))
    attr(columns[[col]],"date_spec")<-s;attr(columns[[col]],"privacy_class")<-2L
  }
  list(frame=frame,columns=columns)
}

.dateOutputCovariates <- function(ctx,node,inputs,co) {
  fmt<-node$format
  if(fmt %in% c("wide","long")) return(co)
  vars<-.irOutputVariables(ctx$ir,.compileNodeName(node,"output"))
  vars<-vars[vapply(ctx$ir$variables[vars],function(v)!is.null(v$date_spec)||(v$derived$kind %||% "") %in% .DATE_KINDS,logical(1))]
  if(!length(vars)) return(co)
  if(fmt %in% c("sparse","temporal_covariates","dictionary")) .dateReject(paste0("Format '",fmt,"' has no roster date-variable slot; use a wide typed companion output and join it for analysis."))
  pop<-inputs$population;name<-.compileNodeName(node,"output")
  cons<-.reduceConsumer(ctx,name,pop$roster,pop$coverage,node$grain)
  for(v in vars) {
    rec<-.compileVariable(ctx,.irNamedNode(ctx$ir,"variable",v),pop$roster,cons);values<-rec$evaluate()
    for(cn in names(co$components)) {
      part<-co$components[[cn]];frame<-part$frame
      if("cohort_row_id" %in% names(frame)) {
        projected<-values
        at<-match(as.character(frame$cohort_row_id),as.character(values$row_key))
      } else if("person_id" %in% names(frame)) {
        roster<-.reduceRosterFrame(ctx,cons$roster)
        projected<-.outputIdentity(roster)
        ix<-match(as.character(roster$cohort_row_id),as.character(values$row_key))
        for(col in names(rec$columns)) projected[[col]]<-values[[col]][ix]
        projected<-.reduceCollapse(projected,node$collapse %||% "first_episode")
        at<-match(as.character(frame$person_id),as.character(projected$person_id))
      } else next
      for(col in names(rec$columns)) {
        if(col %in% names(frame)) .dateReject("Date covariate collides with an output column.","date_parts_names")
        frame[[col]]<-projected[[col]][at];part$fragment$columns[[col]]<-rec$columns[[col]]
      }
      part$frame<-frame;co$components[[cn]]<-part
    }
  }
  co
}

.dateRoleAnchor <- function(ctx, cons, frame, spec) {
  if(is.null(spec) || !identical(spec$precision,"relative_days")) return(NULL)
  spec <- .dateEffective(NULL,spec)
  dates <- .dateExact(.dateSourceValues(ctx,cons,spec$anchor,spec))
  roster <- .reduceRosterFrame(ctx,cons$roster)
  if("cohort_row_id" %in% names(frame)) return(dates[match(as.character(frame$cohort_row_id),as.character(roster$cohort_row_id))])
  if("person_id" %in% names(frame) && !anyDuplicated(roster$person_id)) return(dates[match(as.character(frame$person_id),as.character(roster$person_id))])
  .dateReject("Relative presentation requires an unambiguous roster anchor.","date_source_invalid")
}
