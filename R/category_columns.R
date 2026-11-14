.categoryColumnDependencies <- function(map) {
  out<-character()
  walk<-function(x) {
    if(!is.list(x))return(NULL)
    if(identical(x$kind,"category_column"))out<<-union(out,x$column)
    lapply(x,walk);NULL
  }
  walk(map);out
}
.categoryOutputColumns <- function(frame,columns,node,component,components) {
  map<-node$category_columns
  if(!length(map))return(list(frame=frame,columns=columns))
  if(length(intersect(names(map),names(columns)))) .categoryReject("Category additions must have new unique names.")
  original<-frame;original_columns<-columns
  for(name in names(map)) {
    v<-map[[name]];d<-v$derived
    if(!identical(d$kind,"category_column")||isTRUE(v$broadcast)) .categoryReject("category_columns needs row-preserving original-column copies.")
    if(is.null(d$component)&&length(components)!=1L) .categoryReject("Name the output component explicitly.")
    if(!is.null(d$component)&&!d$component %in% components) .categoryReject("Unknown output component.")
    if(!is.null(d$component)&&d$component!=component)next
    c<-original_columns[[d$column]]
    if(is.null(c)) .categoryReject("Bind an original stable column key; sibling/self references are not admitted.")
    eligible<-.conceptColumn(c)||!is.null(attr(c,"category_domain"))||!is.null(attr(c,"number_contract")$levels)||isTRUE(attr(c,"semantic_flag"))||!is.null(attr(c,"date_spec"))
    if(!eligible) .categoryReject("Bind a declared category or semantic flag, not a structural ID or ordinary quantity.","category_type_unsupported")
    frame[[name]]<-original[[d$column]]
    attr(c,"category_spec")<-.categoryEffective(v$category_spec,attr(c,"category_spec"))
    columns[[name]]<-c
  }
  list(frame=frame,columns=columns)
}
