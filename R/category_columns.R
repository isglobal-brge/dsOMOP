.categoryColumnDependencies <- function(map) {
  out<-character()
  walk<-function(x) {
    if(!is.list(x))return(NULL)
    if(identical(x$kind,"category_column"))out<<-union(out,x$column)
    lapply(x,walk);NULL
  }
  walk(map);out
}
.categoryOutputColumns <- function(ctx,frame,columns,node,component,components) {
  map<-node$category_columns
  if(!length(map))return(list(frame=frame,columns=columns))
  if(length(intersect(names(map),names(columns)))) .categoryReject("Category additions must have new unique names.")
  original<-frame;original_columns<-columns
  evaluate<-function(v,depth=0L) {
    if(depth>32L||isTRUE(v$broadcast)||!is.null(v$derived$reducer)) .categoryReject("Category copies preserve rows and cannot reduce or broadcast.")
    d<-v$derived
    if(identical(d$kind,"category_column")) {
      if(is.null(d$component)&&length(components)!=1L) .categoryReject("Name the output component explicitly.")
      if(!is.null(d$component)&&!d$component %in% components) .categoryReject("Unknown output component.")
      if(!is.null(d$component)&&d$component!=component)return(NULL)
      c<-original_columns[[d$column]]
      if(is.null(c)) .categoryReject("Bind an original stable column key; sibling/self references are not admitted.")
      eligible<-isTRUE(attr(c,"category_result"))||.conceptColumn(c)||!is.null(attr(c,"category_domain"))||!is.null(attr(c,"number_contract")$levels)||.categoryFlag(list(),c)||!is.null(attr(c,"date_spec"))
      if(!eligible) .categoryReject("Bind a declared category or semantic flag, not a structural ID or ordinary quantity.","category_type_unsupported")
      attr(c,"category_spec")<-.categoryEffective(v$category_spec,attr(c,"category_spec"))
      return(list(value=original[[d$column]],column=c))
    }
    if(!identical(d$kind,"category_indicators")) .categoryReject("Use category_column copies or identity indicators over them.")
    operand<-evaluate(d$variable,depth+1L)
    if(is.null(operand))return(NULL)
    local<-list2env(as.list(ctx,all.names=TRUE),parent=parent.env(ctx));local$ir<-ctx$ir;local$ir$options$category_spec<-NULL
    col<-operand$column
    fallback<-if(.categoryFlag(list(),col))list(binary=list(kind="category_binary",as="factor",labels=c("no","yes"))) else list(order=list(kind="category_order",metadata="producer"))
    attr(col,"category_spec")<-.categoryEffective(attr(col,"category_spec"),fallback)
    spec<-attr(col,"category_spec")
    if(.categoryFlag(list(),col)) {spec$binary$as<-"factor";attr(col,"category_spec")<-spec}
    if(identical(spec$rollup$multiple,"all")) {
      owner<-attr(col,"concept_owner")
      value<-.categorySetIndicator(ctx,operand$value,spec,d$key,d$missing,if(length(owner))ctx$ir$variables[[owner]]$labels else NULL)
    } else {
      tmp<-data.frame(value=operand$value)
      presented<-.conceptPresentOnce(local,tmp,list(value=col))
      contract<-attr(presented,"omop_concepts")$value
      if(is.null(contract)||!identical(contract$kind,"category")) .categoryReject("Column indicators require a typed category identity.")
      alias<-.categoryNullableStrings(contract$resolved$aliases)
      key<-if(d$key %in% names(alias))alias[[d$key]] else d$key
      if(grepl("^special:",key)&&!key %in% contract$resolved$keys) .categoryReject("Declare the sentinel before testing it.")
      value<-as.integer(unname(alias[contract$ids])==key)
    }
    available<-attr(col,"category_available") %||% attr(col,"number_origin")$available %||% TRUE
    defined<-attr(operand$value,"category_defined") %||% rep(TRUE,length(value))
    available<-available & defined
    if(d$missing=="zero")value[is.na(value)&available]<-0L
    value[!available]<-NA_integer_
    out<-.fragmentColumn("integer",TRUE,"measure");attr(out,"semantic_flag")<-TRUE
    attr(out,"privacy_class")<-attr(col,"privacy_class");attr(out,"category_available")<-available
    attr(out,"category_spec")<-v$category_spec
    attr(out,"category_input_contract")<-.categoryInputContract(ctx,d$variable)
    list(value=value,column=out)
  }
  additions<-lapply(map,evaluate)
  for(name in names(additions))if(!is.null(additions[[name]])) {
    frame[[name]]<-additions[[name]]$value;columns[[name]]<-additions[[name]]$column
  }
  list(frame=frame,columns=columns)
}

.categoryOutputCovariates <- function(ctx,node,inputs,co) {
  if(node$format %in% c("wide","long","intervals","joined"))return(co)
  vars<-.irOutputVariables(ctx$ir,.compileNodeName(node,"output"))
  if(!length(vars))return(co)
  existing<-unique(unlist(lapply(co$components,function(part)names(part$fragment$columns)),use.names=FALSE))
  vars<-setdiff(vars,existing)
  if(!length(vars))return(co)
  pop<-inputs$population
  cons<-.reduceConsumer(ctx,.compileNodeName(node,"output"),pop$roster,pop$coverage,node$grain)
  vars<-Filter(function(v) {
    decl<-.irNamedNode(ctx$ir,"variable",v)
    rec<-.compileVariable(ctx,decl,pop$roster,cons)
    if(all(names(rec$columns) %in% existing))return(FALSE)
    any(vapply(rec$columns,function(c).conceptColumn(c)||!is.null(attr(c,"category_domain"))||.categoryFlag(decl,c),logical(1))) ||
      (decl$derived$kind %||% "") %in% c("repeated","category_reduce","category_indicators")
  },vars)
  if(!length(vars))return(co)
  if(node$format %in% c("sparse","temporal_covariates","dictionary")) .categoryReject("Use a typed wide/long companion for categorical variables; matrix cells retain numeric analyses.","category_type_unsupported")
  .dateOutputCovariates(ctx,node,inputs,co,vars)
}
