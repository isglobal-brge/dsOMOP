# Portable private numeric evaluator. No backend ROUND/transcendental semantics.
.numberDouble <- function(x) {
  if(inherits(x,"integer64")) {
    y<-suppressWarnings(as.double(x));back<-suppressWarnings(bit64::as.integer64(y))
    if(any(!is.na(x)&!((back==x) %in% TRUE))) .numberReject("This binary64 operation cannot retain exact integers; retain raw/count identity representation.","number_type_unsupported")
    return(y)
  }
  as.double(x)
}
# Validate the physical materialization boundary before any cast. BIGINT may
# arrive as decimal text; fractional doubles must never be truncated into it.
.numberMaterialize <- function(x,type) {
  if(type=="bigint") {
    if(inherits(x,"integer64"))return(x)
    valid<-if(is.character(x))is.na(x)|grepl("^-?[0-9]+$",x) else
      is.numeric(x)&(is.na(x)|is.finite(x)&x==trunc(x))
    if(!all(valid)) .numberReject("Runtime numeric value disagrees with its declared integer type; retain a numeric representation.","number_type_unsupported")
    y<-suppressWarnings(bit64::as.integer64(x))
    if(any(!is.na(x)&is.na(y))||is.numeric(x)&&any(!is.na(x)&suppressWarnings(as.double(y))!=x))
      .numberReject("Runtime integer cannot be materialized exactly; retain raw/count identity representation.","number_type_unsupported")
    return(y)
  }
  if(!is.numeric(x)||is.factor(x)||inherits(x,"Date")) .numberReject("Runtime value disagrees with its declared numeric type.","number_type_unsupported")
  if(type=="numeric")return(if(inherits(x,"integer64")) .numberDouble(x) else x)
  if(type=="integer"&&!inherits(x,"integer64")&&any(!is.na(x)&(!is.finite(x)|x!=trunc(x))))
    .numberReject("Runtime numeric value disagrees with its declared integer type; retain a numeric representation.","number_type_unsupported")
  x
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
  if(inherits(x,"integer64")) {
    symbol<-switch(op,lt="<",le="<=",gt=">",ge=">=",eq="==",ne="!=")
    if(length(t)==1L)return(if(is.na(t))rep(NA,length(x)) else .verbInteger64Compare(x,symbol,t))
    if(length(t)!=length(x)) .numberReject("Paired numeric bounds require identical row identity.","number_dependency_invalid")
    return(vapply(seq_along(t),function(i)if(is.na(t[i]))NA else .verbInteger64Compare(x[i],symbol,t[i]),logical(1)))
  }
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
    else if(k=="expression") {x<-.numberExpression(p$exprs,data.frame(x=x),p$result);ranges<-NULL;unit<-p$unit %||% .numberExpressionUnit(p$exprs,p$result,unit)}
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
