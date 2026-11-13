# Date metadata is a distinct variant of the existing controlled A-27 route.
.dateCapture <- function(frame,columns) {
  contracts<-attr(frame,"omop_concepts") %||% list()
  for(col in intersect(names(frame),names(columns))) {
    c<-attr(frame[[col]],"omop_date_contract")
    if(is.null(c)) next
    # Subsetting data frames can retain vector attributes; reconstruct keys from
    # the actual labels so collapse/join never leaves row-misaligned identity.
    ids<-c$levels[match(as.character(frame[[col]]),c$labels)]
    s<-c$spec
    identity<-s;identity$labels<-NULL;identity$level_range<-NULL
    # Lexical built-in keys are chronology-safe. Custom and fixed parts use
    # their public calendar order, never patient insertion order.
    ranks<-if(!is.null(s$precision)&&s$precision!="custom") match(c$levels,sort(c$levels)) else c$order
    records<-setNames(lapply(seq_along(c$levels),function(i)list(kind="date",id=c$levels[i],label=c$labels[i],calendar=identity,order=ranks[i])),c$levels)
    resolved<-.dateResolveRecords(unname(records))
    declared<-setdiff(c$levels,unique(ids[!is.na(ids)]))
    contracts[[col]]<-list(kind="date",ids=ids,records=records,declared=declared,resolved=resolved)
  }
  if(length(contracts)) attr(frame,"omop_concepts")<-contracts
  frame
}
