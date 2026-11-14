.numberChangeValues <- function(rows,d,origin=NULL) {
  d<-.numberChangeCheck(d);m<-d$method
  rows<-rows[!is.na(rows$start_date),,drop=FALSE]
  if(d$missing=="skip")rows<-rows[!is.na(.numberClean(rows$v)),,drop=FALSE]
  # Threshold comparisons stay exact; every arithmetic method uses checked
  # binary64 operands before integer64 operators or scalar vapply can run.
  if(m!="time_to_threshold")rows$v<-.numberDouble(rows$v)
  if(!nrow(rows))return(NA_real_)
  if(m %in% c("difference","percent_change","ratio")) {
    if(nrow(rows)<2L)return(NA_real_)
    first<-rows$v[1L];last<-tail(rows$v,1L)
    return(.numberClean(switch(m,difference=last-first,percent_change=if(first==0||is.na(first))NA_real_ else 100*(last-first)/first,ratio=if(first==0||is.na(first))NA_real_ else last/first)))
  }
  if(m=="slope") {
    t<-as.numeric(rows$start_date-min(rows$start_date));if(length(unique(t))<2L)return(NA_real_)
    if(d$time_unit=="weeks")t<-t/7
    t<-t-mean(t);v<-rows$v-mean(rows$v)
    return(.numberClean(sum(t*v)/sum(t*t)))
  }
  if(identical(d$reference,"first")||is.list(d$reference)&&identical(d$reference$kind,"first_observation"))origin<-rows$start_date[1L]
  if(is.null(origin)||is.na(origin))return(NA_real_)
  hits<-which(rows$start_date>=origin & .numberCompare(rows$v,d$op,d$threshold))
  if(!length(hits))return(NA_real_)
  elapsed<-as.integer(rows$start_date[hits[1L]]-origin)
  if(d$time_unit=="weeks")elapsed/7 else elapsed
}
