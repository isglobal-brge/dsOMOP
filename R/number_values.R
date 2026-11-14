# Execution-only materialization, exact scalar rounding and row expressions.
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
.numberExpression <- function(exprs,frame,result=NULL,type_only=FALSE) {
  result<-.numberExpressionShape(exprs,result,names(frame))
  assignments<-.mutateValidate(exprs,frame,"numeric expression")
  selected<-assignments[[match(result,vapply(assignments,function(a)a$name,""))]]
  if(!selected$type %in% c("numeric","integer")) .numberReject("Expression result must be numeric; use native bins or flags for classification.","number_expression_invalid")
  # The grammar's integer domain is signed64, regardless of empty/observed
  # values or whether an intermediate happens to fit R's smaller integers.
  type<-if(selected$type=="integer")"bigint" else "numeric"
  if(type_only)return(type)
  types<-vapply(frame,.verbColumnType,"")
  for(a in assignments) {
    frame[[a$name]]<-.mutateEvalChecked(a$expr,frame,types,.numberDouble);types[[a$name]]<-a$type
  }
  .numberMaterialize(frame[[result]],type)
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
