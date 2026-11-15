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
# Scalar rounding and expression evaluation are execution-only operations.
# Resolve them when consuming values; empty prototypes use the same vector
# operations without loading patient-row evaluators into the client.
.numberRoundValues <- function(x,s) {
  if(s$method=="none")return(x)
  x<-.numberDouble(x)
  switch(s$method,floor=floor(x),ceiling=ceiling(x),trunc=trunc(x),
    vapply(x,function(value)get(".numberRoundOne",mode="function")(value,s$method,s$digits),numeric(1)))
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
  if(inherits(x,"integer64")) {
    symbol<-switch(op,lt="<",le="<=",gt=">",ge=">=",eq="==",ne="!=")
    if(length(t)==1L)return(if(is.na(t))rep(NA,length(x)) else get(".verbInteger64Compare",mode="function")(x,symbol,t))
    if(length(t)!=length(x)) .numberReject("Paired numeric bounds require identical row identity.","number_dependency_invalid")
    return(vapply(seq_along(t),function(i)if(is.na(t[i]))NA else get(".verbInteger64Compare",mode="function")(x[i],symbol,t[i]),logical(1)))
  }
  switch(op,lt=x<t,le=x<=t,gt=x>t,ge=x>=t,eq=x==t,ne=x!=t)
}
.numberPresent <- function(x,s,unit="1",ranges=NULL,available=TRUE,offset_allowed=TRUE,canonical=FALSE,trace=FALSE) {
  if(!is.numeric(x)||inherits(x,c("Date","POSIXt","factor"))) .numberReject("Numeric presentation requires a registered numeric analytic value.","number_type_unsupported")
  x<-.numberClean(x);steps<-.numberEffective(s)$pipeline
  stages<-list()
  type<-function(v)if(is.factor(v))"ordered_factor" else if(inherits(v,"integer64"))"integer64" else if(is.integer(v))"integer" else "numeric"
  for(p in steps) {
    before<-type(x);before_unit<-unit
    k<-p$kind
    if(canonical&&!k %in% c("event_unit","fixed_unit"))break
    if(k=="event_unit") {
      stages[[length(stages)+1L]]<-list(kind=k,input_type=before,result_type=before,unit=unit,phase="source before predicates/reduction")
      next # source phase has already applied this map
    }
    if(k=="fixed_unit") {
      if(unit!=p$from||!offset_allowed&&p$offset!=0) .numberReject("Fixed conversion needs matching declared units and zero offsets on sums/differences/durations.","number_unit_conflict")
      x<-.numberClean(.numberDouble(x)*p$factor+p$offset)
      if(!is.null(ranges))ranges<-lapply(ranges,function(v)v*p$factor+p$offset)
      unit<-p$to
    } else if(k=="transform") {
      x<-.numberTransformValues(x,p)
      if(p$method!="identity")ranges<-NULL
    } else if(k=="round")x<-.numberClean(.numberRoundValues(x,p))
    else if(k=="expression") {x<-get(".numberExpression",mode="function")(p$exprs,data.frame(x=x),p$result);ranges<-NULL;unit<-p$unit %||% .numberExpressionUnit(p$exprs,p$result,unit)}
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
        at<-ifelse(.numberCompare(x,"lt",lo),1L,ifelse(.numberCompare(x,"gt",hi),3L,2L))
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
    if(k %in% c("flag","present","missing"))unit<-"1"
    stages[[length(stages)+1L]]<-list(kind=k,input_type=before,result_type=type(x),source_unit=before_unit,unit=unit,phase="private scalar before sealing")
  }
  if(length(available)==1L&&!isTRUE(available))x[]<-NA else if(length(available)>1L)x[!available %in% TRUE]<-NA
  if(trace)list(value=x,stages=stages) else x
}
