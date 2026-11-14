# A-29. Closed numeric declarations shared by the client and server.
.NUMBER_KINDS <- c("number_rows", "number_count", "number_change", "number_derive", "number_column")
.NUMBER_GRIDS <- list(count_1_2_5=c(-Inf,0,1,2,5,10,20,50,100,Inf),
  proportion_quarters=c(-Inf,0,.25,.5,.75,1,Inf), signed_decades=c(-Inf,-100,-10,-1,0,1,10,100,Inf))
.numberReject <- function(message, reason="number_spec_invalid") {
  stop(structure(list(message=paste0(reason, ": ", message), call=NULL,
    code="validation_error", reason=reason), class=c("omop_number_error","dsomop_plan_rejection","error","condition")))
}
.numberObject <- function(x, fields, required=character()) {
  if(!is.list(x) || length(x) && (is.null(names(x)) || anyDuplicated(names(x))) ||
     length(setdiff(names(x),fields)) || length(setdiff(required,names(x))) ||
     any(vapply(x,is.null,logical(1)))) .numberReject("Use only the documented fields and required operands.")
  invisible(x)
}
.numberChoice <- function(x, choices) {
  if(!is.character(x)||length(x)!=1L||is.na(x)||!x %in% choices) .numberReject("Invalid numeric option; use its documented enum.")
  x
}
.numberFinite <- function(x, n=1L) {
  x<-unlist(x,use.names=FALSE)
  if(!is.numeric(x)||length(x)!=n||any(!is.finite(x))) .numberReject("Numeric operands must be finite public numbers of the required length.")
  x
}
.numberNames <- function(x) {
  if(!is.character(x)||!length(x)||anyNA(x)||any(!grepl("^[A-Za-z][A-Za-z0-9_]{0,63}$",x))||anyDuplicated(x)) .numberReject("Use unique native identifiers.","number_dependency_invalid")
  x
}
.numberLabels <- function(x, n=NULL, unique=TRUE) {
  x<-unlist(x,use.names=FALSE)
  if(!is.character(x)||!length(x)||anyNA(x)||any(!nzchar(x))||any(!validUTF8(x))||
     !is.null(n)&&length(x)!=n||unique&&anyDuplicated(x)) .numberReject("Declare complete nonempty UTF-8 labels in public order.")
  x
}
.numberUnitToken <- function(x) {
  scalar<-"(1|count|proportion|percent|days|weeks|months|years|decimal_years|unit:[1-9][0-9]*|currency:[1-9][0-9]*|coordinate:[A-Za-z][A-Za-z0-9_]{0,63})"
  if(!is.character(x)||length(x)!=1L||is.na(x)||!grepl(paste0("^",scalar,"(/(days|weeks))?$"),x)) .numberReject("Declare a canonical scalar unit or coordinate, optionally per days/weeks.","number_unit_conflict")
  x
}
.numberUnitCheck <- function(u) {
  if(is.character(u)&&identical(u,"source")) return(list(kind="source"))
  .numberObject(u,c("kind","spec","from","to","factor","offset"),"kind")
  kind<-.numberChoice(u$kind,c("source","event_unit","fixed_unit"))
  if(kind=="source") .numberObject(u,"kind","kind")
  if(kind=="event_unit") .numberObject(u,c("kind","spec"),c("kind","spec"))
  if(kind=="fixed_unit") {
    .numberObject(u,c("kind","from","to","factor","offset"),c("kind","from","to","factor"))
    .numberUnitToken(u$from);.numberUnitToken(u$to)
    if(.numberFinite(u$factor)<=0) .numberReject("A unit factor must be positive.","number_unit_conflict")
    u$offset<-.numberFinite(u$offset %||% 0)
  }
  u
}
.numberTransformCheck <- function(t) {
  .numberObject(t,c("method","factor","offset","center","scale","lower","upper"),"method")
  m<-.numberChoice(t$method,c("identity","log","log10","log2","log1p","sqrt","abs","exp","expm1","affine","standardize","clip"))
  fields<-switch(m,affine=c("factor","offset"),standardize=c("center","scale"),clip=c("lower","upper"),character())
  .numberObject(t,c("method",fields),c("method",setdiff(fields,if(m=="affine")"offset" else character())))
  if(m=="affine"&&is.null(t$offset)) t$offset<-0
  for(k in fields) .numberFinite(t[[k]])
  if(m=="standardize"&&t$scale<=0 || m=="clip"&&t$lower>=t$upper) .numberReject("Use positive scale and strictly increasing clip bounds.")
  t
}
.numberRoundCheck <- function(r) {
  .numberObject(r,c("method","digits"),"method")
  m<-.numberChoice(r$method,c("none","decimals","significant","floor","ceiling","trunc"))
  if(m %in% c("decimals","significant")) {
    d<-.numberFinite(r$digits)
    if(d!=trunc(d)||d<if(m=="decimals")-308 else 1||d>if(m=="decimals")308 else 17) .numberReject("digits must be an integer in the documented range.")
  } else if(!is.null(r$digits)) .numberReject("Only decimals/significant accept digits.")
  r
}
.numberBreaks <- function(x) {
  x<-unlist(x,use.names=FALSE)
  if(is.character(x)) {
    if(any(!x %in% c("-Inf","Inf") & is.na(suppressWarnings(as.numeric(x))))) .numberReject("Invalid bin boundary.")
    x<-as.numeric(x)
  }
  if(!is.numeric(x)||length(x)<2L||anyNA(x)||any(diff(x)<=0)||
     any(!is.finite(x[-c(1L,length(x))]))||x[1L]==Inf||tail(x,1L)==-Inf) .numberReject("Breaks must strictly increase, with optional infinite tails only.")
  x
}
.numberSpecCheck <- function(s) {
  if(is.null(s)) return(NULL)
  fields<-c("unit","transform","rounding","representation","breaks","grid","closed","labels","range","thresholds","op","category_map","levels","pipeline")
  .numberObject(s,fields)
  if(!is.null(s$pipeline)) {
    if(length(s)!=1L||!is.list(s$pipeline)||!length(s$pipeline)||!is.null(names(s$pipeline))) .numberReject("pipeline is a nonempty ordered list and cannot mix with shorthand fields.")
    .numberPipelineCheck(s$pipeline)
    return(s)
  }
  if(!is.null(s$unit)) s$unit<-.numberUnitCheck(s$unit)
  if(!is.null(s$transform)) {
    ts<-s$transform
    if(!is.null(ts$method)) ts<-list(ts)
    if(!is.list(ts)||!length(ts)||!is.null(names(ts))) .numberReject("transform needs a nonempty ordered transform list.")
    s$transform<-lapply(ts,.numberTransformCheck)
  }
  if(!is.null(s$rounding)) s$rounding<-.numberRoundCheck(s$rounding)
  enums<-list(representation=c("number","bins","category","flag","present","missing"),closed=c("left","right"),range=c("reference","thresholds","bins"),op=c("lt","le","gt","ge","eq","ne","inside","outside"),grid=names(.NUMBER_GRIDS))
  for(k in intersect(names(s),names(enums))) .numberChoice(s[[k]],enums[[k]])
  if(!is.null(s$breaks)) s$breaks<-as.list(.numberBreaks(s$breaks))
  if(!is.null(s$breaks)&&!is.null(s$grid)) .numberReject("Choose breaks or grid at one level.")
  for(k in intersect(names(s),c("labels","levels","category_map"))) .numberLabels(s[[k]],unique=k!="category_map")
  if(!is.null(s$thresholds)) .numberFinite(s$thresholds,length(s$thresholds))
  if(!is.null(s$thresholds)&&!is.null(s$category_map)) .numberReject("A bin map cannot also declare thresholds.")
  if(!is.null(s$range)&&s$range=="reference"&&!is.null(s$thresholds)) .numberReject("Reference mode cannot declare thresholds.")
  s
}
.numberClassifyCheck <- function(s, previous=NULL) {
  r<-s$representation
  fields<-switch(r,number=character(),bins=c("breaks","grid","closed","labels"),
    category=c("range","thresholds","labels","category_map","levels"),flag=c("op","thresholds","closed"),present=,missing=character())
  .numberObject(s,c("representation",fields),"representation")
  if(r=="bins") {
    if(is.null(s$breaks)==is.null(s$grid)) .numberReject("Bins require exactly one public breaks/grid declaration.")
    b<-if(!is.null(s$grid)) .NUMBER_GRIDS[[s$grid]] else .numberBreaks(s$breaks)
    s$grid<-NULL;s$breaks<-as.list(b);s$closed<-s$closed %||% "left"
    if(!is.null(s$labels)) s$labels<-as.list(.numberLabels(s$labels,length(b)-1L))
  }
  if(r=="category") {
    s$range<-s$range %||% if(!is.null(s$category_map))"bins" else if(!is.null(s$thresholds))"thresholds" else "reference"
    if(s$range=="bins") {
      if(is.null(previous)||previous$kind!="bins"||!is.null(s$thresholds)||!is.null(s$labels)) .numberReject("Bin categories require an immediately preceding bins stage and a complete ordered map.")
      m<-.numberLabels(s$category_map,length(previous$breaks)-1L,FALSE)
      l<-.numberLabels(s$levels %||% c("low","normal","high"))
      ix<-match(m,l)
      if(anyNA(ix)||any(diff(ix)<0)) .numberReject("Category maps must name declared levels in nondecreasing order.")
      s$category_map<-as.list(m);s$levels<-as.list(l)
    } else {
      if(!is.null(s$category_map)||!is.null(s$levels)) .numberReject("category_map/levels apply only to bin categories.")
      if(s$range=="thresholds") {t<-.numberFinite(s$thresholds,2L);if(t[1]>t[2]) .numberReject("Category thresholds must be nondecreasing.");s$thresholds<-as.list(t)}
      if(s$range=="reference"&&!is.null(s$thresholds)) .numberReject("Reference categories do not accept thresholds.")
      s$labels<-as.list(.numberLabels(s$labels %||% c("low","normal","high"),3L))
    }
  }
  if(r=="flag") {
    .numberChoice(s$op,c("lt","le","gt","ge","eq","ne","inside","outside"))
    interval<-s$op %in% c("inside","outside")
    t<-.numberFinite(s$thresholds,if(interval)2L else 1L)
    if(interval) {if(t[1]>=t[2]) .numberReject("Interval thresholds must strictly increase.");s$closed<-s$closed %||% "left"}
    else if(!is.null(s$closed)) .numberReject("Scalar flags do not accept closed.")
    s$thresholds<-as.list(t)
  }
  s
}
.numberPipelineCheck <- function(steps) {
  state<-"numeric";seen_source<-FALSE;seen_fixed<-FALSE;started<-FALSE;out<-list()
  for(s in steps) {
    .numberObject(s,c("kind","spec","from","to","factor","offset","method","center","scale","lower","upper","digits","exprs","result","unit","breaks","grid","closed","labels","range","thresholds","op","category_map","levels"),"kind")
    k<-.numberChoice(s$kind,c("event_unit","fixed_unit","transform","round","expression","bins","category","flag","present","missing"))
    if(state!="numeric"&&!(state=="bins"&&k=="category"&&identical(s$range,"bins"))) .numberReject("Arithmetic requires numeric stages; only a bin category may follow bins.","number_representation_conflict")
    if(k %in% c("event_unit","fixed_unit")) {
      if(started||k=="event_unit"&&(seen_source||seen_fixed)||k=="fixed_unit"&&seen_fixed) .numberReject("Unit steps must come first, once each, source then fixed.","number_unit_conflict")
      s<-.numberUnitCheck(s);if(k=="event_unit")seen_source<-TRUE else seen_fixed<-TRUE
    } else {
      started<-TRUE;t<-s;t$kind<-NULL
      if(k=="transform") s<-c(list(kind=k),.numberTransformCheck(t))
      else if(k=="round") s<-c(list(kind=k),.numberRoundCheck(t))
      else if(k=="expression") {.numberObject(t,c("exprs","result","unit"),"exprs");if(!is.null(t$unit)).numberUnitToken(t$unit);.numberExpressionShape(t$exprs,t$result,"x")}
      else {
        t<-.numberSpecCheck(c(list(representation=k),t))
        t<-.numberClassifyCheck(t,if(length(out))tail(out,1)[[1]] else NULL)
        t$representation<-NULL;s<-c(list(kind=k),t)
        state<-if(k=="bins")"bins" else "terminal"
      }
    }
    out[[length(out)+1L]]<-s
  }
  out
}
.numberEffective <- function(variable=NULL, recipe=NULL) {
  v<-.numberSpecCheck(variable);r<-.numberSpecCheck(recipe)
  if(!is.null(v$pipeline)) return(list(pipeline=.numberPipelineCheck(v$pipeline)))
  if(is.null(v)&&!is.null(r$pipeline)) return(list(pipeline=.numberPipelineCheck(r$pipeline)))
  if(!is.null(r$pipeline)) r<-list()
  raw<-r %||% list()
  for(k in names(v)) raw[[k]]<-v[[k]]
  if(!is.null(v$breaks)) raw$grid<-NULL
  if(!is.null(v$grid)) raw$breaks<-NULL
  if(!is.null(v$thresholds)&&is.null(v$range)) raw$range<-"thresholds"
  if(!is.null(v$category_map)&&is.null(v$range)) raw$range<-"bins"
  rep<-raw$representation %||% "number"
  fields<-switch(rep,number=character(),bins=c("breaks","grid","closed","labels"),category=c("range","thresholds","labels","category_map","levels"),flag=c("op","thresholds","closed"),character())
  applicable<-c("unit","representation",fields,if(!rep %in% c("present","missing"))c("transform","rounding"))
  if(length(setdiff(names(v),applicable))) .numberReject("Explicit fields are inapplicable to the selected representation.")
  raw<-raw[intersect(names(raw),applicable)];steps<-list()
  if(!is.null(raw$unit)&&raw$unit$kind!="source") steps<-list(raw$unit)
  for(t in raw$transform %||% list()) steps[[length(steps)+1L]]<-c(list(kind="transform"),t)
  if(!is.null(raw$rounding)) steps[[length(steps)+1L]]<-c(list(kind="round"),raw$rounding)
  if(rep!="number") {
    s<-.numberClassifyCheck(c(list(representation=rep),raw[intersect(names(raw),fields)]))
    s$representation<-NULL;steps[[length(steps)+1L]]<-c(list(kind=rep),s)
  }
  if(!length(steps)) steps<-list(list(kind="transform",method="identity"))
  list(pipeline=.numberPipelineCheck(steps))
}
.numberExpressionShape <- function(exprs, result=NULL, aliases) {
  pkg<-utils::packageName();if(is.null(pkg))pkg<-"dsOMOP"
  checked<-.jsonSchemaValidate(.jsonSchemaPackaged("mutate-v1",package=pkg),exprs)
  if(!isTRUE(checked$valid)) .numberReject("Use a closed mutate-v1 numeric request, never code.","number_expression_invalid")
  names<-vapply(exprs$assign,function(a)a$name,"")
  if(anyDuplicated(names)||any(names %in% aliases)) .numberReject("Assignments cannot duplicate or shadow inputs.","number_expression_invalid")
  result<-result %||% tail(names,1L)
  if(!result %in% names) .numberReject("result must select a declared assignment.","number_expression_invalid")
  result
}
.numberChangeCheck <- function(d) {
  m<-.numberChoice(d$method,c("difference","percent_change","ratio","slope","time_to_threshold"))
  .numberChoice(d$missing,c("skip","keep"))
  extras<-intersect(names(d),c("reference","time_unit","threshold","op"))
  if(m %in% c("difference","percent_change","ratio")&&length(extras)) .numberReject("Endpoint change methods do not accept origin/time/threshold operands.")
  if(m %in% c("slope","time_to_threshold")) {
    if(d$missing!="skip") .numberReject("Slope and attainment require missing=skip.")
    d$time_unit<-.numberChoice(d$time_unit %||% "days",c("days","weeks"))
    if(m=="slope"&&length(intersect(extras,c("reference","threshold","op")))) .numberReject("Slope does not accept reference/threshold/op.")
    if(m=="time_to_threshold") {
      .numberFinite(d$threshold);d$op<-.numberChoice(d$op %||% "ge",c("ge","gt","le","lt"));d$reference<-d$reference %||% "first"
    }
  }
  d
}
.numberHas <- function(x) {
  if(!is.list(x))return(FALSE)
  if(!is.null(x$number_spec)||!is.null(x$number_columns)||(x$kind %||% "") %in% .NUMBER_KINDS)return(TRUE)
  any(vapply(x,.numberHas,logical(1)))
}
.numberCapabilities <- function(x) {
  ops<-character()
  walk<-function(v) {
    if(!is.list(v))return(invisible(NULL))
    if(!is.null(v$number_spec))ops<<-c(ops,"number.presentation")
    if(!is.null(v$number_columns))ops<<-c(ops,"number.columns")
    k<-v$kind %||% ""
    if(k %in% .NUMBER_KINDS)ops<<-c(ops,switch(k,number_rows="number.rows",number_count="number.count",number_change="number.change",number_derive="number.expression",number_column="number.columns"))
    if(k=="expression")ops<<-c(ops,"number.expression")
    lapply(v,walk);invisible(NULL)
  }
  walk(x);unique(ops)
}
.numberWireFinite <- function(x) {
  if(is.list(x))return(lapply(x,.numberWireFinite))
  if(is.numeric(x)&&length(x)==1L&&!is.finite(x))return(if(x<0)"-Inf" else "Inf")
  x
}
.numberDependencyName <- function(name,alias) paste0("num",substr(as.character(openssl::sha256(charToRaw(paste(name,alias,sep="/")))),1L,24L))
.numberPlanNormalize <- function(plan) {
  if(!.numberHas(plan))return(plan)
  recipe<-plan$options$number_spec
  if(!is.null(recipe)) .numberSpecCheck(recipe)
  normalize<-function(v,defaults=NULL,depth=0L) {
    if(depth>32L).numberReject("Numeric dependencies exceed the existing expression nesting bound.","number_dependency_invalid")
    d<-v$derived
    if(!is.null(d$inputs)) {
      aliases<-vapply(d$inputs,function(i)i$alias,"");.numberNames(aliases)
      d$result<-.numberExpressionShape(d$exprs,d$result,aliases)
      d$inputs<-lapply(d$inputs,function(i){i$variable<-normalize(i$variable,NULL,depth+1L);i})
    }
    if(identical(d$kind,"number_change")) {
      d<-.numberChangeCheck(d)
      if(identical(d$reference,"first"))d$reference<-list(kind="first_observation")
    }
    if(identical(d$kind,"number_rows")&&isTRUE(v$broadcast)) .numberReject("Event rows need explicit reduction for scalar outputs.","number_dependency_invalid")
    if(!is.null(d))v$derived<-d
    spec<-v$number_spec
    # Recipe scalar defaults are resolved at the final analytic slot. Source
    # maps must fork before predicates/reduction, including private operands.
    source<-v$source %||% d$source
    eff<-if(!is.null(spec)||!is.null(defaults)) .numberEffective(spec,defaults) else NULL
    maps<-Filter(function(p)p$kind=="event_unit",eff$pipeline %||% list())
    if(length(maps)) {
      if(is.null(source$event)) {
        if(!is.null(spec)) .numberReject("Source maps require a numeric event unit role.","number_unit_conflict")
      } else {
        ev<-plan$events[[source$event]]
        if(is.null(ev)||!ev$table %in% c("measurement","observation")) .numberReject("Source maps require a registered unit-bearing source.","number_unit_conflict")
        value<-d$value %||% v$reducer$value %||% "value"
        if(!value %in% c("value","value_as_number","range_low","range_high")) .numberReject("A measurement unit map cannot convert unrelated roles.","number_unit_conflict")
        map<-maps[[1L]]$spec
        if(!is.null(ev$unit)&&!identical(.jcsCanonical(ev$unit),.jcsCanonical(map))) .numberReject("Explicit source and numeric unit maps conflict.","number_unit_conflict")
        ev$unit<-map
        key<-paste0("num",substr(as.character(openssl::sha256(charToRaw(.jcsCanonical(ev)))),1L,24L))
        plan$events[[key]]<<-ev;source$event<-key
        if(!is.null(v$source))v$source<-source else v$derived$source<-source
      }
    }
    if(!is.null(spec))v$number_spec<-.numberWireFinite(.numberEffective(spec))
    v
  }
  for(n in names(plan$variables))plan$variables[[n]]<-normalize(plan$variables[[n]],recipe)
  for(o in names(plan$outputs)) {
    cols<-plan$outputs[[o]]$number_columns
    if(!is.null(cols)) {
      .numberNames(names(cols))
      plan$outputs[[o]]$number_columns<-lapply(cols,normalize)
    }
  }
  plan
}
