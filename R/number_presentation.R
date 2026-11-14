# Output-level presentation sees the canonical frame after all selection,
# coverage, joins and date calculations. It executes once, before sealing.
.numberColumnMeta <- function(column) {
  meta<-attr(column,"number_origin")
  if(is.null(meta))meta<-list(unit="1",available=TRUE,offset_allowed=TRUE)
  meta
}
.numberResolvedColumn <- function(value,column,s,meta) {
  if(is.null(s))return(list(value=value,column=column))
  attr(column,"number_origin")<-meta
  canonical<-value;canonical_column<-column
  effective<-.numberEffective(s)
  ranges<-attr(value,"number_ranges")
  presented<-.numberPresent(.numberUnwrap(value),effective,unit=meta$unit,ranges=ranges,
    available=meta$available,offset_allowed=meta$offset_allowed,trace=TRUE)
  value<-presented$value
  result_unit<-meta$unit
  for(step in effective$pipeline) {
    if(step$kind=="fixed_unit")result_unit<-step$to
    if(step$kind=="expression")result_unit<-step$unit %||% .numberExpressionUnit(step$exprs,step$result,result_unit)
    if(step$kind %in% c("present","missing","flag"))result_unit<-"1"
  }
  contract<-list(unit=result_unit,source_unit=meta$unit,coordinates=meta$coordinates,pipeline=.numberWireFinite(effective$pipeline),stages=presented$stages,quantitative_poolable=!grepl("unresolved_source|mixed_coordinates",result_unit),source=meta$source,
    reducer=meta$reducer,derived=meta$derived,input_type=column$type,
    result_type=if(is.factor(value))"ordered_factor" else if(inherits(value,"integer64"))"integer64" else if(is.integer(value))"integer" else "numeric",
    privacy_class=attr(column,"privacy_class") %||% 3L,
    availability="unavailable stays NA; available absent value is missing",
    levels=if(is.factor(value))as.list(levels(value)) else NULL)
  # Availability and selected ranges are private; the contract describes policy.
  column$type<-if(is.factor(value))"text" else if(inherits(value,"integer64"))"bigint" else if(is.integer(value))"integer" else "numeric"
  attr(column,"number_contract")<-contract
  list(value=value,column=column,canonical=canonical,canonical_column=canonical_column)
}
.numberOutputColumns <- function(frame,columns,node,component,components,recipe=NULL,canonical=NULL,canonical_columns=NULL) {
  original<-canonical %||% frame;original_columns<-canonical_columns %||% columns
  map<-node$number_columns %||% list()
  if(length(map)&&node$format %in% c("sparse","temporal_covariates","membership","dictionary")) .numberReject("Use a typed wide/long/dense person_period frame for numeric column copies.","number_type_unsupported")
  evaluate<-function(v,as_input=FALSE,stage="presented",depth=0L) {
    if(depth>32L||isTRUE(v$broadcast)||!is.null(v$derived$reducer)) .numberReject("Output formulas preserve rows: no reduction/broadcast/cyclic dependencies.","number_dependency_invalid")
    d<-v$derived
    if(identical(d$kind,"number_column")) {
      if(is.null(d$component)&&length(components)!=1L) .numberReject("Name a patient frame component explicitly.","number_dependency_invalid")
      if(!is.null(d$component)&&!d$component %in% components) .numberReject("Unknown output component.","number_dependency_invalid")
      if(!is.null(d$component)&&d$component!=component)return(NULL)
      n<-d$column;c<-original_columns[[n]]
      if(is.null(c)||!.numberEligibleColumn(c,n,node)) .numberReject("Bind an original numeric analytic key, never identifiers, event codes, weights, Dates or factors.","number_source_invalid")
      meta<-.numberColumnMeta(c);spec<-v$number_spec %||% meta$spec
      if(!as_input)spec<-.numberEffective(spec,recipe) else spec<-.numberEffective(spec)
      if(any(vapply(spec$pipeline,function(p)p$kind=="event_unit",logical(1)))) .numberReject("Column copies cannot repeat source-unit normalization.","number_unit_conflict")
      x<-original[[n]]
      if(as_input&&stage=="canonical")spec$pipeline<-Filter(function(p)p$kind=="fixed_unit",spec$pipeline)
      if(!length(spec$pipeline))spec$pipeline<-list(list(kind="transform",method="identity"))
      return(.numberResolvedColumn(x,c,spec,meta))
    }
    if(!identical(d$kind,"number_derive")) .numberReject("number_columns accepts column copies or formulas whose leaves are copies.","number_dependency_invalid")
    operands<-lapply(d$inputs,function(i)evaluate(i$variable,TRUE,i$stage,depth+1L))
    absent<-vapply(operands,is.null,logical(1))
    if(all(absent))return(NULL)
    if(any(absent)) .numberReject("Formula inputs must belong to one component.","number_dependency_invalid")
    if(any(vapply(operands,function(x)!is.numeric(x$value)||is.factor(x$value)||inherits(x$value,"Date"),logical(1)))) .numberReject("Presented operands must remain numeric; use a numeric sibling.","number_type_unsupported")
    values<-setNames(lapply(operands,function(x)x$value),vapply(d$inputs,function(i)i$alias,""))
    value<-.numberExpression(d$exprs,as.data.frame(values),d$result)
    c<-.fragmentColumn(if(is.integer(value))"integer" else "numeric",TRUE,"measure")
    if(any(vapply(operands,function(x)identical(attr(x$column,"privacy_class"),2L),logical(1))))attr(c,"privacy_class")<-2L
    meta<-list(unit=d$unit,available=Reduce(`&`,lapply(operands,function(x).numberColumnMeta(x$column)$available)),offset_allowed=TRUE,derived=d)
    spec<-.numberEffective(v$number_spec,if(!as_input)recipe else NULL)
    if(as_input&&stage=="canonical") {
      spec$pipeline<-Filter(function(p)p$kind=="fixed_unit",spec$pipeline)
      if(!length(spec$pipeline))spec$pipeline<-list(list(kind="transform",method="identity"))
    }
    .numberResolvedColumn(value,c,spec,meta)
  }
  # Validate additions against the entire canonical schema, including names
  # excluded from a final projection. Never bind another addition by name.
  if(length(intersect(names(map),names(columns)))) .numberReject("Numeric additions must have new unique column names.","number_dependency_invalid")
  additions<-lapply(map,evaluate)
  for(n in names(columns)) {
    c<-columns[[n]]
    if(!is.null(attr(c,"number_contract")))next
    if(!.numberEligibleColumn(c,n,node)||node$format=="survival"&&is.null(attr(c,"number_origin")))next
    meta<-.numberColumnMeta(c);s<-meta$spec
    if(is.null(s)&&is.null(recipe)&&is.null(attr(c,"number_origin")))next
    # Source maps are applied on event streams, never on scalar/generated slots.
    default<-recipe
    if(is.null(s)&&!is.null(default$unit)&&default$unit$kind=="event_unit")default$unit<-NULL
    if(is.null(s)&&!is.null(default$unit)&&default$unit$kind=="fixed_unit"&&!identical(default$unit$from,meta$unit))default$unit<-NULL
    effective<-.numberEffective(s,default)
    result<-.numberResolvedColumn(frame[[n]],c,effective,meta)
    if(node$format %in% c("sparse","temporal_covariates") && is.factor(result$value)) .numberReject("Matrix cells must remain numeric; use a typed wide/long companion for bins/categories.","number_type_unsupported")
    frame[[n]]<-result$value;columns[[n]]<-result$column
  }
  for(n in names(additions))if(!is.null(additions[[n]])) {
    frame[[n]]<-additions[[n]]$value;columns[[n]]<-additions[[n]]$column
    original[[n]]<-additions[[n]]$canonical;original_columns[[n]]<-additions[[n]]$canonical_column
  }
  list(frame=frame,columns=columns,canonical=original,canonical_columns=original_columns)
}
.numberCapture <- function(frame,columns) {
  contracts<-attr(frame,"omop_concepts") %||% list();numbers<-attr(frame,"omop_numbers") %||% list()
  for(n in intersect(names(columns),names(frame))) {
    c<-attr(columns[[n]],"number_contract")
    if(is.null(c))next
    numbers[[n]]<-c
    if(!is.factor(frame[[n]]))next
    labels<-levels(frame[[n]]);ids<-paste0("N",seq_along(labels))
    records<-setNames(lapply(seq_along(labels),function(i)list(kind="number",id=ids[i],label=labels[i],number=c,order=i)),ids)
    contracts[[n]]<-list(kind="number",number=c,ids=ids[match(as.character(frame[[n]]),labels)],records=records,declared=ids,resolved=.numberResolveRecords(unname(records),c))
  }
  if(length(contracts))attr(frame,"omop_concepts")<-contracts
  if(length(numbers))attr(frame,"omop_numbers")<-numbers
  frame
}
.numberGenerated <- function(column,unit,privacy=3L,available=TRUE) {
  attr(column,"number_origin")<-list(unit=unit,available=available,offset_allowed=FALSE)
  attr(column,"privacy_class")<-privacy;column
}
.numberEligibleColumn <- function(column,name,node) {
  structural<-c("cohort_definition_id",switch(node$format,
    long=c("event_number","visit_occurrence_id","event_id","unit_concept_id","operator_concept_id"),
    sparse=,temporal_covariates=c("covariate_id","analysis_id","time_id","start_day","end_day"),
    person_period=c("period_id","bin_id","time_id"),
    survival=c("event","event_code","event_id","status","stratum_id","interval_number","transition_id","from_state","to_state","weight","sampling_weight","from","to","trans","state_visit_number","event_number","outcome_event_number"),character()))
  .numberAnalyticColumn(column)&&!name %in% structural
}

.numberOutputOrigins <- function(ctx,node,columns) {
  if(!.numberHas(ctx$ir))return(columns)
  sources<-.outputSources(node)
  units<-vapply(sources,function(s).numberSourceUnit(ctx,list(source=s)),"")
  common<-if(length(unique(units))==1L)units[[1L]] else "unresolved_source"
  durations<-switch(node$format,
    long=c("start_days","end_days","days_from_index","days_since_previous","duration_days"),
    intervals=c("start_days_from_index","end_days_from_index","duration_days"),
    survival=c("followup_days","follow_up_days","Tstart","Tstop","time","time_at_risk","entry_days_from_index","exit_days_from_index","event_days_from_index","interval_start_days","interval_end_days"),character())
  for(n in names(columns)) {
    c<-columns[[n]]
    if(!is.null(attr(c,"number_origin"))||!is.null(attr(c,"number_contract")))next
    if(n %in% durations || node$format=="long"&&startsWith(n,"days_from_")&&n %in% paste0("days_from_",names(node$options$anchors)))columns[[n]]<-.numberGenerated(c,"days",2L)
    else if(node$format=="long"&&n %in% c("value","value_as_number","range_low","range_high"))columns[[n]]<-.numberGenerated(c,common)
  }
  columns
}
.numberMatrixOrigin <- function(ctx,node,column,reg) {
  if(!.numberHas(ctx$ir))return(column)
  sources<-.outputSources(node)
  coordinates<-lapply(seq_len(nrow(reg)),function(i) {
    a<-reg$analysis[[i]]
    unit<-switch(a,binary="1",count="count",exposed_days="days",.numberSourceUnit(ctx,list(source=sources[[reg$source_index[[i]]]])))
    list(covariate_id=reg$covariate_id[[i]],source=reg$source[[i]],concept_id=reg$concept_id[[i]],analysis=a,unit=unit)
  })
  units<-unique(vapply(coordinates,function(c)c$unit,""))
  attr(column,"number_origin")<-list(unit=if(length(units)==1L)units else "mixed_coordinates",coordinates=coordinates,available=TRUE,offset_allowed=FALSE)
  column
}

# Bind copies to declared fragment schemas before output rows are fetched or
# joined. The later evaluator uses the same keys and checks the actual types.
.numberPreflightColumns <- function(node,schemas) {
  map<-node$number_columns %||% list()
  if(!length(map))return(invisible(NULL))
  if(node$format %in% c("sparse","temporal_covariates","membership","dictionary"))
    .numberReject("Use a typed wide/long/dense person_period frame for numeric column copies.","number_type_unsupported")
  if(any(names(map) %in% unique(unlist(lapply(schemas,names)))))
    .numberReject("Numeric additions must have new unique column names.","number_dependency_invalid")
  selected<-unlist(node$columns,use.names=FALSE)
  allowed<-unique(c(unlist(lapply(schemas,names)),unlist(lapply(schemas,function(s)attr(s,"number_variable_names"))),names(map)))
  if(length(setdiff(selected,allowed)))
    .numberReject("Select declared original columns or named numeric additions.","number_source_invalid")
  bind<-function(v,depth=0L) {
    if(depth>32L||isTRUE(v$broadcast)||!is.null(v$derived$reducer))
      .numberReject("Output formulas preserve rows: no reduction/broadcast/cyclic dependencies.","number_dependency_invalid")
    d<-v$derived
    if(identical(d$kind,"number_column")) {
      comp<-d$component
      if(is.null(comp)) {
        if(length(schemas)!=1L).numberReject("Name a patient frame component explicitly.","number_dependency_invalid")
        comp<-names(schemas)[1L]
      }
      if(!comp %in% names(schemas)).numberReject("Unknown output component.","number_dependency_invalid")
      column<-schemas[[comp]][[d$column]]
      if(is.null(column)||!.numberEligibleColumn(column,d$column,node))
        .numberReject("Bind an original numeric analytic key, never identifiers, event codes, weights, Dates or factors.","number_source_invalid")
      return(comp)
    }
    if(!identical(d$kind,"number_derive"))
      .numberReject("number_columns accepts column copies or formulas whose leaves are copies.","number_dependency_invalid")
    components<-unique(vapply(d$inputs,function(i)bind(i$variable,depth+1L),""))
    if(length(components)!=1L).numberReject("Formula inputs must belong to one component.","number_dependency_invalid")
    components
  }
  invisible(lapply(map,bind))
}
.numberVariableSchema <- function(ctx,node,name,inputs,cons) {
  columns<-list()
  for(v in .irOutputVariables(ctx$ir,name)) {
    rec<-.compileVariable(ctx,.irNamedNode(ctx$ir,"variable",v),inputs$population$roster,cons)
    columns<-c(columns,rec$columns)
  }
  attr(columns,"number_variable_names")<-.irOutputVariables(ctx$ir,name)
  columns
}
.numberPreflightOutput <- function(ctx,node,name,inputs) {
  if(!length(node$number_columns))return(invisible(NULL))
  if(node$format %in% c("sparse","temporal_covariates","membership","dictionary"))return(.numberPreflightColumns(node,list()))
  # Panel identities include vocabulary expansion, and joined identities come
  # from their declared input fragments; those builders check their inventory.
  if(node$format %in% c("person_period","joined","wide"))return(invisible(NULL))
  cons<-.reduceConsumer(ctx,name,inputs$population$roster,inputs$population$coverage,node$grain %||% "episode")
  vars<-.numberVariableSchema(ctx,node,name,inputs,cons)
  numeric<-function(keys)setNames(lapply(keys,function(k).fragmentColumn("numeric",TRUE,"measure")),keys)
  opts<-node$options %||% list()
  if(node$format=="survival") {
    fmt<-opts$format %||% "survival";schemas<-list();tars<-.outputTarList(node,name)
    for(t in names(tars)) {
      family<-.survivalFamily(fmt,opts,t,name,length(opts$tars %||% list())>0L)
      for(part in setdiff(names(family),"transition_ref"))schemas[[family[[part]]]]<-c(.survivalColumns(part,cons,fmt),vars)
    }
  } else {
    columns<-c(.outputIdentityColumns(cons),vars)
    if(node$format=="intervals")columns<-c(columns,numeric(c("start_days_from_index","end_days_from_index")))
    if(node$format=="long") {
      derived<-unlist(opts$columns_derived)
      if(isTRUE(opts$event_log))columns<-c(columns,numeric(c("start_days","end_days","value"))) else {
        for(ref in .outputSources(node)) {
          src<-.reduceSource(ctx,ref)
          for(key in .longOtherColumns(src)) {
            col<-src$fragment$columns[[key]]
            if(isTRUE(src$catalogue$columns[[key]]$concept))attr(col,"concept")<-TRUE
            columns[[key]]<-col
          }
        }
        columns<-c(columns,numeric(intersect(derived,c("value","days_from_index","days_since_previous","duration_days"))))
        if("days_from_anchor" %in% derived)columns<-c(columns,numeric(paste0("days_from_",names(opts$anchors))))
        if(length(opts$relations))columns<-c(columns,numeric(paste0("days_to_",names(opts$relations))))
      }
    }
    attr(columns,"number_variable_names")<-attr(vars,"number_variable_names")
    schemas<-setNames(list(columns),name)
  }
  .numberPreflightColumns(node,schemas)
}
