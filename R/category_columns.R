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
      eligible<-.conceptColumn(c)||!is.null(attr(c,"category_domain"))||!is.null(attr(c,"number_contract")$levels)||.categoryFlag(list(),c)||!is.null(attr(c,"date_spec"))
      if(!eligible) .categoryReject("Bind a declared category or semantic flag, not a structural ID or ordinary quantity.","category_type_unsupported")
      attr(c,"category_spec")<-.categoryEffective(v$category_spec,attr(c,"category_spec"))
      return(list(value=original[[d$column]],column=c))
    }
    if(!identical(d$kind,"category_indicators")) .categoryReject("Use category_column copies or identity indicators over them.")
    operand<-evaluate(d$variable,depth+1L)
    if(is.null(operand))return(NULL)
    local<-list2env(as.list(ctx,all.names=TRUE),parent=parent.env(ctx));local$ir<-ctx$ir;local$ir$options$category_spec<-NULL
    col<-operand$column
    attr(col,"category_spec")<-.categoryEffective(attr(col,"category_spec"),list(order=list(kind="category_order",metadata="producer")))
    tmp<-data.frame(value=operand$value)
    presented<-.conceptPresent(local,tmp,list(value=col))
    contract<-attr(presented,"omop_concepts")$value
    if(is.null(contract)||!identical(contract$kind,"category")) .categoryReject("Column indicators require a typed category identity.")
    alias<-.categoryNullableStrings(contract$resolved$aliases)
    key<-if(d$key %in% names(alias))alias[[d$key]] else d$key
    if(grepl("^special:",key)&&!key %in% contract$resolved$keys) .categoryReject("Declare the sentinel before testing it.")
    value<-as.integer(unname(alias[contract$ids])==key)
    available<-attr(col,"category_available") %||% attr(col,"number_origin")$available %||% TRUE
    defined<-attr(operand$value,"category_defined") %||% rep(TRUE,length(value))
    available<-available & defined
    if(d$missing=="zero")value[is.na(value)&available]<-0L
    value[!available]<-NA_integer_
    out<-.fragmentColumn("integer",TRUE,"measure");attr(out,"semantic_flag")<-TRUE
    attr(out,"privacy_class")<-attr(col,"privacy_class");attr(out,"category_available")<-available
    attr(out,"category_spec")<-v$category_spec
    list(value=value,column=out)
  }
  additions<-lapply(map,evaluate)
  for(name in names(additions))if(!is.null(additions[[name]])) {
    frame[[name]]<-additions[[name]]$value;columns[[name]]<-additions[[name]]$column
  }
  list(frame=frame,columns=columns)
}
