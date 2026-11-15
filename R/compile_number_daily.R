# A-31: private two-stage reduction on the existing consumer roster.
.numberDailyRecord <- function(ctx,node,name,cons) {
  d<-node$derived;r<-d$reducer
  .numberReducerCheck(r)
  .numberChoice(d$within_day,c("mean","max","first"))
  .numberChoice(d$missing,c("skip","propagate"))
  if(!(d$input$derived$kind %||% "") %in% c("number_rows","number_derive") || !is.null(d$input$derived$reducer))
    .numberReject("/derived/input: use numeric event rows or an unreduced same-event derive.","number_daily_input")
  # Apply the outer selection to every same-event leaf before expression
  # evaluation, so ineligible rows cannot trigger arithmetic or alter collapse.
  select<-function(v) {
    if(identical(v$derived$kind,"number_rows"))v$derived$window<-r$window
    if(!is.null(v$derived$inputs))v$derived$inputs<-lapply(v$derived$inputs,function(i){i$variable<-select(i$variable);i})
    v
  }
  units<-function(v) {
    if(identical(v$derived$kind,"number_rows") && .numberPresentedUnit(ctx$ir,v) %in% c("unresolved_source","mixed_coordinates","categorical"))
      .numberReject("/derived/input: normalize each event quantity before declaring a common coordinate.","number_unit_conflict")
    if(!is.null(v$derived$inputs))lapply(v$derived$inputs,function(i)units(i$variable))
    invisible(NULL)
  }
  units(d$input)
  operand<-select(d$input);attr(operand,"name")<-.numberDependencyName(name,"daily")
  rec<-.compileVariable(ctx,operand,cons$roster,cons)
  if(!isTRUE(rec$event_grain)||length(rec$columns)!=1L)
    .numberReject("/derived/input: daily collapse requires one aligned event stream.","number_daily_input")
  unit<-.numberPresentedUnit(ctx$ir,d$input)
  if(unit %in% c("unresolved_source","mixed_coordinates","categorical"))
    .numberReject("/derived/input: declare a compatible normalized numeric unit before daily collapse.","number_unit_conflict")
  source<-.reduceSource(ctx,rec$event_source)
  ev<-ctx$ir$events[[rec$event_source$event]]
  # Explicit expressions declare a common quantity. Ordinary event projections
  # require a single concept; do not infer equivalence from equal unit labels.
  if(!startsWith(unit,"coordinate:") && !.numberDailySingleConcept(ctx$ir,ev))
    .numberReject("/derived/input: multiple concepts need an explicit common-quantity numeric coordinate; concept-days remain separate.","number_daily_quantity")
  meta<-.numberColumnMeta(rec$columns[[1L]])
  present<-function(x) .numberPresent(.numberUnwrap(x),d$input$number_spec %||% list(),
    unit=meta$unit %||% .numberSourceUnit(ctx,d$input),available=rec$available,
    ranges=attr(x,"number_ranges"),offset_allowed=meta$offset_allowed %||% TRUE)
  proto<-present(.numberMaterialize(.reduceTypedNA(rec$columns[[1L]]$type,0L),rec$columns[[1L]]$type))
  if(!is.numeric(proto)||is.factor(proto))
    .numberReject("/derived/input: retain a numeric sibling before collapse.","number_type_unsupported")
  input_type<-if(inherits(proto,"integer64"))"bigint" else if(is.integer(proto))"integer" else "numeric"
  daily_type<-if(d$within_day=="mean")"numeric" else input_type
  type<-.reduceValueType(r$fn,daily_type)
  columns<-setNames(lapply(.reduceColumnNames(name,r),function(n).fragmentColumn(type,TRUE,"measure")),.reduceColumnNames(name,r))
  for(n in names(columns))attr(columns[[n]],"privacy_class")<-2L
  evaluate<-function() {
    roster<-.reduceRosterFrame(ctx,cons$roster);keys<-roster$cohort_row_id
    bounds<-.reduceBoundsR(ctx,cons,r$window,"daily input window")
    if(!is.null(cons$time_window))bounds<-.reduceIntersectBounds(bounds,.reduceBoundsR(ctx,cons,cons$time_window))
    rows<-rec$evaluate()
    rows<-.reduceWindowFilter(rows,bounds)
    rows<-rows[!is.na(rows$start_date),,drop=FALSE]
    rows$v<-present(rows[[names(rec$columns)[1L]]])
    ordering<-intersect(.reduceSourceOrdering(source),names(rows))
    rows<-rows[.reduceOrderRows(rows,c("row_key",ordering)),,drop=FALSE]
    group_cols<-c("row_key","source_ns","concept_id","start_date")
    if(!all(group_cols %in% names(rows))&&nrow(rows))
      .numberReject("/derived/input: daily input lacks canonical concept/date identity.","number_daily_input")
    # Length-delimited canonical JSON prevents key/delimiter collisions. No
    # patient-derived identifier, group count or coordinate leaves this closure.
    ids<-vapply(seq_len(nrow(rows)),function(i).jcsCanonical(c(lapply(rows[i,group_cols,drop=FALSE],as.character),list(coordinate=unit))),"")
    unique_ids<-unique(ids)
    groups<-lapply(unique_ids,function(id)which(ids==id))
    daily<-data.frame(row_key=keys[FALSE],source_ns=character(),event_id=character(),start_date=as.Date(character()))
    value<-.reduceTypedNA(daily_type,length(groups))
    for(i in seq_along(groups)) {
      ix<-groups[[i]];x<-rows$v[ix]
      daily[i,]<-list(rows$row_key[ix[1L]],rows$source_ns[ix[1L]],
        as.character(openssl::sha256(charToRaw(unique_ids[i]))),rows$start_date[ix[1L]])
      if(d$missing=="propagate"&&anyNA(x)||all(is.na(x)))next
      x<-x[!is.na(x)]
      value[i]<-switch(d$within_day,mean=mean(.numberDouble(x)),max=max(x),first=x[1L])
    }
    daily$v<-value
    # Deliberately no ranges, visit key or original event ID on the collapsed
    # stream. Roster association preserves genuine person/episode lineage.
    anchors<-if(r$fn=="closest_value").reduceAnchorDates(ctx,cons,.reduceAnchor(r)) else NULL
    values<-.reduceApply(r$fn,daily,keys,r,c("start_date","source_ns","event_id"),anchors,daily_type)
    out<-data.frame(row_key=keys)
    for(j in seq_along(columns)) {
      x<-.reduceCoerce(values[[j]],type);if(r$fn=="cv")x<-x/100
      x[!bounds$defined|!rec$available]<-NA
      out[[names(columns)[j]]]<-x
    }
    out
  }
  list(stream=NULL,reducer=NULL,columns=columns,evaluate=evaluate,
    available=rec$available,declared_empty_value=NA,event_grain=FALSE)
}

.numberDailySingleConcept <- function(plan,event) {
  cs<-event$concept_set
  if(is.character(cs)&&length(cs)==1L)cs<-plan$concept_sets[[cs]]
  if(is.list(cs)&&!is.null(cs$ref))cs<-plan$concept_sets[[cs$ref]]
  if(is.list(cs)) {
    if(isTRUE(cs$include_descendants)||isTRUE(cs$include_mapped)||length(cs$exclude))return(FALSE)
    cs<-cs$ids %||% cs$concepts
  }
  length(unlist(cs,use.names=FALSE))==1L
}
