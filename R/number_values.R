# Portable private numeric evaluator. No backend ROUND/transcendental semantics.
.numberDouble <- function(x) {
  if(inherits(x,"integer64")) {
    y<-suppressWarnings(as.double(x));back<-suppressWarnings(bit64::as.integer64(y))
    if(any(!is.na(x)&!((back==x) %in% TRUE))) .numberReject("This binary64 operation cannot retain exact integers; retain raw/count identity representation.","number_type_unsupported")
    return(y)
  }
  as.double(x)
}
.numberClean <- function(x) {
  if(inherits(x,"integer64"))return(x)
  x[!is.finite(x)]<-NA
  x[which(x==0)]<-0L
  x
}
# Exact rational input / decimal step, nearest integer ties to even, followed
# by the platform correctly-rounded decimal-to-binary64 conversion. All large
# arithmetic uses the existing pure-R integer implementation, no new dependency.
.numberRoundOne <- function(x,method,digits) {
  if(is.na(x)||!is.finite(x))return(NA_real_)
  if(x==0)return(0)
  a<-abs(x);dy<-.dsomopDpDyadic(a)
  numerator<-.dsomopBigShiftLeft(dy$odd,max(0L,dy$exponent))
  denominator<-.dsomopBigShiftLeft(1,max(0L,-dy$exponent))
  ten<-function(k).dsomopBigInteger(paste0("1",strrep("0",k)))
  compare_power<-function(k) {
    if(k>=0).dsomopBigCompare(numerator,.dsomopBigMultiply(denominator,ten(k))) else
      .dsomopBigCompare(.dsomopBigMultiply(numerator,ten(-k)),denominator)
  }
  if(method=="significant") {
    decade<-floor(log10(a)) # provisional only; exact comparisons settle boundaries
    while(compare_power(decade)<0)decade<-decade-1L
    while(compare_power(decade+1L)>=0)decade<-decade+1L
    digits<-digits-1L-decade
  }
  if(digits>=0)numerator<-.dsomopBigMultiply(numerator,ten(digits)) else denominator<-.dsomopBigMultiply(denominator,ten(-digits))
  q<-.dsomopBigDivide(numerator,denominator)
  remainder<-.dsomopBigSubtract(numerator,.dsomopBigMultiply(q,denominator))
  cmp<-.dsomopBigCompare(.dsomopBigMultiply(remainder,2),denominator)
  if(cmp>0||cmp==0&&.dsomopBigIsOdd(q))q<-.dsomopBigAdd(q,1)
  out<-suppressWarnings(as.numeric(paste0(if(x<0)"-" else "",.dsomopBigDecimal(q),"e",-digits)))
  if(!is.finite(out))NA_real_ else if(out==0)0 else out
}
.numberRoundValues <- function(x,s) {
  if(s$method=="none")return(x)
  x<-.numberDouble(x)
  switch(s$method,floor=floor(x),ceiling=ceiling(x),trunc=trunc(x),
    vapply(x,.numberRoundOne,numeric(1),method=s$method,digits=s$digits))
}
.numberTransformValues <- function(x,s) {
  if(s$method=="identity")return(x)
  x<-.numberDouble(x)
  .numberClean(suppressWarnings(switch(s$method,
    log=log(x),log10=log10(x),log2=log2(x),log1p=log1p(x),sqrt=sqrt(x),
    abs=abs(x),exp=exp(x),expm1=expm1(x),affine=s$factor*x+s$offset,
    standardize=(x-s$center)/s$scale,clip=pmin(pmax(x,s$lower),s$upper))))
}
.numberBoundary <- function(x) {
  if(is.infinite(x))return(if(x<0)"-Inf" else "Inf")
  if(x==0)return("0")
  .jcsNumber(x)
}
.numberBinLabels <- function(s) {
  if(!is.null(s$labels))return(unlist(s$labels,use.names=FALSE))
  b<-unlist(s$breaks);n<-length(b)-1L
  vapply(seq_len(n),function(i)paste0(if(s$closed=="left"&&is.finite(b[i]))"[" else "(",.numberBoundary(b[i]),",",.numberBoundary(b[i+1L]),if(s$closed=="right"&&is.finite(b[i+1L]))"]" else ")"),"")
}
.numberCompare <- function(x,op,t) {
  if(inherits(x,"integer64")) return(.verbInteger64Compare(x,switch(op,lt="<",le="<=",gt=">",ge=">=",eq="==",ne="!="),t))
  switch(op,lt=x<t,le=x<=t,gt=x>t,ge=x>=t,eq=x==t,ne=x!=t)
}
.numberExpression <- function(exprs,frame,result=NULL) {
  result<-.numberExpressionShape(exprs,result,names(frame))
  assignments<-.mutateValidate(exprs,frame,"numeric expression")
  selected<-assignments[[match(result,vapply(assignments,function(a)a$name,""))]]
  if(!selected$type %in% c("numeric","integer")) .numberReject("Expression result must be numeric; use native bins or flags for classification.","number_expression_invalid")
  types<-vapply(frame,.verbColumnType,"")
  for(a in assignments) {
    if(a$type=="numeric")for(n in names(frame))if(inherits(frame[[n]],"integer64")) .numberDouble(frame[[n]])
    frame[[a$name]]<-.mutateEval(a$expr,frame,types);types[[a$name]]<-a$type
  }
  frame[[result]]
}
.numberPresent <- function(x,s,unit="1",ranges=NULL,available=TRUE,offset_allowed=TRUE,canonical=FALSE) {
  if(!is.numeric(x)||inherits(x,c("Date","POSIXt","factor"))) .numberReject("Numeric presentation requires a registered numeric analytic value.","number_type_unsupported")
  x<-.numberClean(x);steps<-.numberEffective(s)$pipeline
  for(p in steps) {
    k<-p$kind
    if(canonical&&!k %in% c("event_unit","fixed_unit"))break
    if(k=="event_unit")next # source phase has already applied this map
    if(k=="fixed_unit") {
      if(unit!=p$from||!offset_allowed&&p$offset!=0) .numberReject("Fixed conversion needs matching declared units and zero offsets on sums/differences/durations.","number_unit_conflict")
      x<-.numberClean(.numberDouble(x)*p$factor+p$offset)
      if(!is.null(ranges))ranges<-lapply(ranges,function(v)v*p$factor+p$offset)
      unit<-p$to
    } else if(k=="transform") {
      x<-.numberTransformValues(x,p)
      if(p$method!="identity")ranges<-NULL
    } else if(k=="round")x<-.numberClean(.numberRoundValues(x,p))
    else if(k=="expression") {x<-.numberExpression(p$exprs,data.frame(x=x),p$result);ranges<-NULL;unit<-p$unit %||% "coordinate:expression"}
    else if(k=="bins") {
      b<-unlist(p$breaks);labels<-.numberBinLabels(p)
      at<-rep(NA_integer_,length(x))
      for(i in seq_along(labels)) {
        hit<-.numberCompare(x,if(p$closed=="left")"ge" else "gt",b[i]) & .numberCompare(x,if(p$closed=="left")"lt" else "le",b[i+1L])
        at[which(hit)]<-i
      }
      x<-ordered(labels[at],levels=labels)
    } else if(k=="category") {
      labels<-unlist(p$labels %||% p$levels)
      if(p$range=="bins")x<-ordered(unlist(p$category_map)[as.integer(x)],levels=labels)
      else {
        if(p$range=="reference") {
          if(is.null(ranges)) .numberReject("Reference categories need selected paired ranges; select an observation or declare thresholds.","number_source_invalid")
          lo<-ranges$low;hi<-ranges$high
        } else {lo<-unlist(p$thresholds)[1L];hi<-unlist(p$thresholds)[2L]}
        at<-ifelse(x<lo,1L,ifelse(x>hi,3L,2L))
        bad<-is.na(x)|!is.finite(lo)|!is.finite(hi)|lo>hi
        at[which(bad)]<-NA_integer_;x<-ordered(labels[at],levels=labels)
      }
    } else if(k=="flag") {
      t<-unlist(p$thresholds)
      hit<-if(p$op %in% c("inside","outside")) {
        inside<-.numberCompare(x,if(p$closed=="left")"ge" else "gt",t[1]) & .numberCompare(x,if(p$closed=="left")"lt" else "le",t[2])
        if(p$op=="outside")!inside else inside
      } else .numberCompare(x,p$op,t)
      x<-as.integer(hit)
    } else if(k=="present")x<-as.integer(!is.na(x))
    else if(k=="missing")x<-as.integer(is.na(x))
  }
  if(length(available)==1L&&!isTRUE(available))x[]<-NA else if(length(available)>1L)x[!available %in% TRUE]<-NA
  x
}
.numberChangeValues <- function(rows,d,origin=NULL) {
  d<-.numberChangeCheck(d);m<-d$method
  rows<-rows[!is.na(rows$start_date),,drop=FALSE]
  if(d$missing=="skip")rows<-rows[is.finite(rows$v),,drop=FALSE]
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
