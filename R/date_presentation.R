# Date metadata is a distinct variant of the existing controlled A-27 route.
.dateCapture <- function(frame,columns) {
  contracts<-attr(frame,"omop_concepts") %||% list()
  for(col in intersect(names(frame),names(columns))) {
    c<-attr(frame[[col]],"omop_date_contract")
    if(is.null(c)) {
      s<-attr(columns[[col]],"date_spec")
      if(is.null(s)||!is.factor(frame[[col]])) next
      labels<-levels(frame[[col]]);keys<-labels;map<-unlist(s$labels)
      if(length(map)) keys[labels %in% map]<-names(map)[match(labels[labels %in% map],map)]
      c<-list(spec=s,levels=keys,labels=labels,order=seq_along(keys))
    }
    # Subsetting data frames can retain vector attributes; reconstruct keys from
    # the actual labels so collapse/join never leaves row-misaligned identity.
    ids<-c$levels[match(as.character(frame[[col]]),c$labels)]
    s<-c$spec
    identity<-s;identity$labels<-NULL;identity$level_range<-NULL
    identity$operation<-attr(columns[[col]],"date_operation")
    # Lexical built-in keys are chronology-safe. Custom and fixed parts use
    # their public calendar order, never patient insertion order.
    ranks<-if(!is.null(s$precision)&&s$precision!="custom") match(c$levels,sort(c$levels)) else c$order
    records<-setNames(lapply(seq_along(c$levels),function(i)list(kind="date",id=c$levels[i],label=c$labels[i],calendar=identity,order=ranks[i])),c$levels)
    resolved<-.dateResolveRecords(unname(records),identity)
    declared<-c$declared %||% setdiff(c$levels,unique(ids[!is.na(ids)]))
    contracts[[col]]<-list(kind="date",calendar=identity,ids=ids,records=records,declared=declared,resolved=resolved)
  }
  if(length(contracts)) attr(frame,"omop_concepts")<-contracts
  frame
}

#' @export
`[.dsomop_date_factor` <- function(x, ..., drop=FALSE) {
  contract<-attr(x,"omop_date_contract")
  out<-NextMethod("[",drop=FALSE)
  if(!is.null(contract)) {
    contract$keys<-contract$levels[match(as.character(out),contract$labels)]
    attr(out,"omop_date_contract")<-contract
  }
  out
}

.dateOrdered <- function(x, levels) {
  out <- ordered(x, levels = levels)
  class(out) <- c("dsomop_date_factor", class(out))
  out
}
