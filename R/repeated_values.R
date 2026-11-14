# Pure selection contracts; selected dates and keys remain private.
.repeatedTarget <- function(anchor,point) {
  offset<-point$offset;unit<-point$unit
  if(unit %in% c("days","weeks"))out<-anchor+offset*if(unit=="weeks")7 else 1
  else {
    y<-.dateY(anchor);m<-.dateM(anchor);d<-.dateD(anchor)
    if(unit=="months") {
      serial<-y*12+m-1+offset;y<-floor(serial/12);m<-serial%%12+1
      d<-pmin(d,.dateD(.dateMonthEnd(y,m)))
    } else {
      y<-y+offset;leap<-m==2L&d==29L&!(y%%4==0&(y%%100!=0|y%%400==0))
      m[which(leap)]<-3L;d[which(leap)]<-1L
    }
    out<-.dateMake(y,m,d)
  }
  out[out<as.Date("0001-01-01")|out>as.Date("9999-12-31")]<-NA
  out
}
.repeatedSelect <- function(rows,target,select) {
  if(!nrow(rows)||is.na(target))return(NA_integer_)
  ordering<-.reduceOrderRows(rows,c("start_date","source_ns","event_id"))
  if(select=="last")return(tail(ordering,1L))
  if(select=="first")return(ordering[1L])
  distance<-abs(as.numeric(rows$start_date-target))
  ordering[which.min(distance[ordering])]
}

.categoryValue <- function(value,defined) {
  attr(value,"category_defined")<-defined
  class(value)<-unique(c("dsomop_category_value",class(value)));value
}
#' @export
`[.dsomop_category_value` <- function(x,i,...) {
  mask<-attr(x,"category_defined");missing_mask<-attr(x,"category_canonical_missing");consumed<-attr(x,"category_consumed");out<-NextMethod("[")
  class(out)<-class(x)
  if(!is.null(consumed)) {
    if(!missing(i))consumed$ids<-consumed$ids[i]
    attr(out,"category_consumed")<-consumed
  }
  attr(out,"category_defined")<-if(missing(i))mask else mask[i]
  if(!is.null(missing_mask))attr(out,"category_canonical_missing")<-if(missing(i))missing_mask else missing_mask[i]
  out
}
