# A-29. Closed numeric declarations shared by the client and server.
.NUMBER_KINDS <- c("number_rows", "number_count", "number_change", "number_derive", "number_column", "number_daily")
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
    if(d!=trunc(d)||d<(if(m=="decimals")-308 else 1)||d>(if(m=="decimals")308 else 17)) .numberReject("digits must be an integer in the documented range.")
  } else if(!is.null(r$digits)) .numberReject("Only decimals/significant accept digits.")
  r
}
.numberBreaks <- function(x) {
  if(is.list(x)) {
    if(any(!vapply(x,function(v)is.numeric(v)&&length(v)==1L||is.character(v)&&length(v)==1L&&v %in% c("-Inf","Inf"),logical(1)))) .numberReject("Bin boundaries are numeric literals with optional infinite tails.")
    x<-vapply(x,function(v)if(is.character(v))if(v=="-Inf")-Inf else Inf else v,0)
  } else if(is.character(x)) .numberReject("Bin boundaries must be numeric, not text.")
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
      else if(k=="expression") {.numberObject(t,c("exprs","result","unit"),"exprs");if(!is.null(t$unit)).numberUnitToken(t$unit);s$result<-.numberExpressionShape(t$exprs,t$result,"x")}
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
  if(!is.null(v)&&!length(v))v<-NULL
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
  .numberAstCheck(exprs,aliases,result)
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
    if(k %in% .NUMBER_KINDS)ops<<-c(ops,switch(k,number_rows="number.rows",number_count="number.count",number_change="number.change",number_derive="number.expression",number_column="number.columns",number_daily="number.daily"))
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
  producers<-plan$variables
  if(!is.null(recipe)) .numberSpecCheck(recipe)
  normalize<-function(v,defaults=NULL,depth=0L,column_scope=FALSE) {
    if(depth>32L).numberReject("Numeric dependencies exceed the existing expression nesting bound.","number_dependency_invalid")
    d<-v$derived
    if(identical(d$kind,"number_column")&&!column_scope) .numberReject("Column bindings are scoped to one output number_columns map.","number_dependency_invalid")
    if(!is.null(d$inputs)) {
      if(!is.null(d$reducer)) .numberReducerCheck(d$reducer)
      aliases<-vapply(d$inputs,function(i)i$alias,"");.numberNames(aliases)
      d$result<-.numberExpressionShape(d$exprs,d$result,aliases)
      d$inputs<-lapply(d$inputs,function(i){i$variable<-normalize(i$variable,NULL,depth+1L,column_scope);i})
    }
    if(identical(d$kind,"number_daily")) {
      .numberReducerCheck(d$reducer)
      .numberChoice(d$within_day,c("mean","max","first"))
      .numberChoice(d$missing,c("skip","propagate"))
      d$input<-normalize(d$input,NULL,depth+1L,column_scope)
    }
    if(!is.null(d$variable))d$variable<-normalize(d$variable,NULL,depth+1L,column_scope)
    if(identical(d$kind,"repeated")) {
      d$values<-lapply(d$values,function(entry) {entry$variable<-normalize(entry$variable,NULL,depth+1L,column_scope);entry})
      for(entry in d$values) {
        operand<-entry$variable
        while(!is.null(operand$derived$variable))operand<-operand$derived$variable
        source<-operand$source %||% operand$derived$source
        if(!is.null(source)&&!identical(.jcsCanonical(source),.jcsCanonical(d$source))) .categoryReject("Repeated values require one identical candidate stream; use a shared event unit map or separate bundles.","repeated_grain_incompatible")
      }
    }
    if(identical(d$kind,"number_change")) {
      d<-.numberChangeCheck(d)
      if(identical(d$reference,"first"))d$reference<-list(kind="first_observation")
    }
    if(identical(d$kind,"number_rows")&&isTRUE(v$broadcast)) .numberReject("Event rows need explicit reduction for scalar outputs.","number_dependency_invalid")
    if(!is.null(d))v$derived<-d
    spec<-v$number_spec
    if(column_scope && identical(d$kind,"number_column")) {
      if(!is.null(spec)&&(identical(spec$unit$kind,"event_unit")||any(vapply(spec$pipeline %||% list(),function(p)p$kind=="event_unit",logical(1))))) .numberReject("Column copies already have normalized source units; remove the event-unit map.","number_unit_conflict")
      spec<-.numberOverlay(spec,.numberWithoutSourceMap(producers[[d$column]]$number_spec))
      defaults<-.numberWithoutSourceMap(defaults)
    }
    # Recipe scalar defaults are resolved at the final analytic slot. Source
    # maps must fork before predicates/reduction, including private operands.
    source<-v$source %||% d$source
    eff<-if(!is.null(spec)||!is.null(defaults)) .numberEffective(spec,defaults) else NULL
    maps<-Filter(function(p)p$kind=="event_unit",eff$pipeline %||% list())
    if(length(maps)) {
      if(identical(d$kind,"repeated")) .categoryReject("Declare source-unit normalization on the shared repeated event, not the selected scalar.","repeated_grain_incompatible")
      if(is.null(source$event)) {
        if(!is.null(spec)) .numberReject("Source maps require a numeric event unit role.","number_unit_conflict")
      } else {
        ev<-plan$events[[source$event]]
        if(is.null(ev)||!ev$table %in% c("measurement","observation")) {
          if(!is.null(spec)) .numberReject("Source maps require a registered unit-bearing source.","number_unit_conflict")
          return(v)
        }
        value<-d$value %||% v$reducer$value %||% "value"
        if(!value %in% c("value","value_as_number","range_low","range_high")) .numberReject("A measurement unit map cannot convert unrelated roles.","number_unit_conflict")
        map<-maps[[1L]]$spec
        if(!is.null(ev$unit)&&!identical(.jcsCanonical(ev$unit),.jcsCanonical(map))) .numberReject("Explicit source and numeric unit maps conflict.","number_unit_conflict")
        ev$unit<-map
        key<-source$event
        if(!grepl("^num[0-9a-f]{24}$",key))key<-paste0("num",substr(as.character(openssl::sha256(charToRaw(.jcsCanonical(list(source=source$event,event=ev))))),1L,24L))
        if(!is.null(plan$events[[key]])&&!identical(.jcsCanonical(plan$events[[key]]),.jcsCanonical(ev))) .numberReject("Numeric source identity collides with a declared event.","number_dependency_invalid")
        plan$events[[key]]<<-ev;source$event<-key
        if(!is.null(v$source))v$source<-source else v$derived$source<-source
      }
    }
    if(!is.null(spec))v$number_spec<-.numberWireFinite(.numberEffective(spec,defaults))
    v
  }
  for(n in names(plan$variables))plan$variables[[n]]<-normalize(plan$variables[[n]],recipe)
  for(o in names(plan$outputs)) {
    out<-plan$outputs[[o]]
    if(!is.null(recipe)) {
      map_source<-function(src) {
        if(is.null(src$event))return(src)
        pseudo<-normalize(list(source=src,reducer=list(fn="mean")),recipe)
        pseudo$source
      }
      if(!is.null(out$sources))out$sources<-lapply(out$sources,map_source)
      if(!is.null(out$options$sources))out$options$sources<-lapply(out$options$sources,map_source)
      plan$outputs[[o]]<-out
    }
    cols<-plan$outputs[[o]]$number_columns
    if(!is.null(cols)) {
      .numberNames(names(cols))
      plan$outputs[[o]]$number_columns<-lapply(cols,function(v)normalize(v,recipe,column_scope=TRUE))
    }
  }
  plan
}
.numberResolveRecords <- function(records,identity=NULL) {
  records<-unname(records)
  if(is.null(identity)&&length(records))identity<-records[[1L]]$number
  if(length(records)&&any(vapply(records,function(r)!identical(r$kind,"number")||!identical(.jcsCanonical(r$number),.jcsCanonical(identity)),logical(1)))) .numberReject("Numeric representation contracts differ across retained sites.","number_representation_conflict")
  records<-records[!duplicated(vapply(records,function(r)r$id,""))]
  records<-records[order(vapply(records,function(r)r$order,0))]
  ids<-vapply(records,function(r)r$id,"");labels<-vapply(records,function(r)r$label,"")
  list(kind="number",number=identity,ids=ids,labels=labels,levels=labels,decisions=list())
}
.numberValidateResolution <- function(shared,records) {
  expected<-.numberResolveRecords(unname(records))
  for(k in c("kind","number","ids","labels","levels")) if(!identical(.jcsCanonical(.pnv2Plain(as.list(shared[[k]]))),.jcsCanonical(.pnv2Plain(as.list(expected[[k]]))))) .numberReject("Incomplete or incompatible shared numeric domain.","number_representation_conflict")
  invisible(TRUE)
}
.numberValidateShared <- function(local,shared) {
  if(!identical(.jcsCanonical(local %||% list()),.jcsCanonical(shared %||% list()))) .numberReject("Numeric column semantics differ across retained sites.","number_representation_conflict")
  invisible(TRUE)
}
.numberExplain <- function(plan,declared=plan) {
  if(!.numberHas(plan))return(character())
  lines<-c("Numeric presentation: source unit normalization before predicates and reduction; fixed conversion, then declared scalar stages, before sealing.",
    "Precedence: variable/wrapper > recipe > design; pipelines are atomic. Canonical inputs bypass transforms/rounds/categories and outer recipe defaults.",
    "Rounding: nearest exact decimal multiple of the actual binary64 input, ties to even; identity/none preserve exact integer widths.",
    "Public bins/categories retain full ordered levels and polynomial contrasts; quantitative trends require a numeric sibling. Reference categories are relative to each selected observation's paired local ranges.",
    "Nonfinite/domain-invalid rows become typed NA without diagnostics; unavailable sources remain unavailable. Counts retain available empty zero; threshold time is first observed attainment, never interpolation.",
    paste0("Required numeric capabilities: ",paste(.numberCapabilities(plan),collapse=", ")))
  if(!is.null(plan$options$number_spec)) {
    lines<-c(lines,paste0("Recipe numeric defaults (including generated analytic slots): ",.jcsCanonical(.pnv2Plain(plan$options$number_spec))))
    for(step in .numberEffective(NULL,plan$options$number_spec)$pipeline) {
      levels<-.numberDeclaredLevels(step)
      if(length(levels))lines<-c(lines,paste0("  default ",step$kind," -> ordered factor; complete public levels: ",paste(levels,collapse=" | ")))
    }
  }
  for(n in names(plan$variables)) {
    v<-plan$variables[[n]]
    if(is.null(v$number_spec)&&is.null(plan$options$number_spec)&&!(v$derived$kind %||% "") %in% .NUMBER_KINDS)next
    lines<-c(lines,paste0("Numeric variable '",n,"': ",.jcsCanonical(.pnv2Plain(v)),"; source/reducer grain retained; broadcast=",isTRUE(v$broadcast),"; date/class-2 lineage never declassified."))
    explicit<-declared$variables[[n]]$number_spec
    origin<-if(!is.null(explicit))"variable/wrapper" else if(!is.null(declared$options$number_spec))"recipe" else "design"
    lines<-c(lines,paste0("  specification origin: ",origin,"; explicit declaration: ",.jcsCanonical(.pnv2Plain(explicit %||% list()))))
    effective<-.numberEffective(v$number_spec,plan$options$number_spec)
    unit<-.numberDeclaredUnit(plan,v)
    lines<-c(lines,paste0("  canonical source coordinate: ",unit))
    state<-"numeric"
    for(i in seq_along(effective$pipeline)) {
      step<-effective$pipeline[[i]];before<-state
      if(step$kind=="fixed_unit")unit<-step$to
      if(step$kind=="expression")unit<-step$unit %||% .numberExpressionUnit(step$exprs,step$result,unit)
      if(step$kind %in% c("flag","present","missing"))unit<-"1"
      if(step$kind %in% c("bins","category"))state<-"ordered factor (contr.poly)"
      if(step$kind %in% c("flag","present","missing"))state<-"integer 0/1"
      levels<-.numberDeclaredLevels(step)
      lines<-c(lines,paste0("  stage ",i," ",step$kind,": ",before," -> ",state,"; coordinate ",unit,
        if(length(levels))paste0("; complete public levels: ",paste(levels,collapse=" | ")),
        if(step$kind=="event_unit")"; before source predicates and reduction" else "; private scalar presentation before sealing"))
    }
  }
  for(n in names(plan$outputs))if(!is.null(plan$outputs[[n]]$number_columns))lines<-c(lines,paste0("Output '",n,"' original-frame numeric bindings: ",.jcsCanonical(plan$outputs[[n]]$number_columns),"; exact row identity; recipe display defaults inactive inside formulas."))
  lines
}
.numberOverlay <- function(high,low) {
  high<-.numberSpecCheck(high);low<-.numberSpecCheck(low)
  if(!is.null(high$pipeline))return(high)
  if(is.null(high)||!length(high))return(low %||% list())
  if(!is.null(low$pipeline))low<-list()
  if(!is.null(high$representation)) {
    r<-high$representation
    allowed<-c("unit","representation",if(!r %in% c("present","missing"))c("transform","rounding"),
      switch(r,bins=c("breaks","grid","closed","labels"),category=c("range","thresholds","labels","category_map","levels"),flag=c("op","thresholds","closed"),character()))
    low<-low[intersect(names(low),allowed)]
  }
  out<-low %||% list();for(k in names(high))out[[k]]<-high[[k]]
  if(!is.null(high$breaks))out$grid<-NULL
  if(!is.null(high$grid))out$breaks<-NULL
  if(!is.null(high$thresholds)&&is.null(high$range))out$range<-if(identical(out$representation,"category"))"thresholds" else NULL
  if(!is.null(high$category_map)&&is.null(high$range))out$range<-"bins"
  out
}

.numberColumnDependencies <- function(x) {
  found<-character()
  walk<-function(v) {
    if(!is.list(v))return(NULL)
    if(identical(v$kind,"number_column"))found<<-c(found,v$column)
    lapply(v,walk);NULL
  }
  walk(x);unique(found)
}
.numberVariables <- function(x) {
  result<-list()
  walk<-function(v) {
    if(!is.list(v))return(NULL)
    if(!is.null(v$reducer)||!is.null(v$derived)||!is.null(v$raw)||!is.null(v$population_column))result[[length(result)+1L]]<<-v
    if((v$kind %||% "") %in% c("number_derive","number_daily")&&!is.null(v$reducer))result[[length(result)+1L]]<<-list(reducer=v$reducer)
    lapply(v,walk);NULL
  }
  walk(x);result
}

.numberReducerCheck <- function(r) {
  if(!r$fn %in% c("first_value","last_value","nth_value","closest_value","mean","min","max","sum","sd","cv","median","quantile","mode","slope")||!is.null(r$value)||!is.null(r$unit)&&!identical(r$unit,"day")) .numberReject("Expression reducers require an admitted numeric reducer with value and unit omitted.","number_expression_invalid")
  invisible(r)
}

.numberWithoutSourceMap <- function(s) {
  if(is.null(s))return(NULL)
  if(identical(s$unit$kind,"event_unit"))s$unit<-NULL
  if(!is.null(s$pipeline)) {
    s$pipeline<-Filter(function(p)p$kind!="event_unit",s$pipeline)
    if(!length(s$pipeline))s$pipeline<-list(list(kind="transform",method="identity"))
  }
  s
}
.numberExpressionUnit <- function(exprs,result,unit) {
  paste0("coordinate:expr_",substr(as.character(openssl::sha256(charToRaw(.jcsCanonical(list(exprs=exprs,result=result,unit=unit))))),1L,48L))
}

.numberDeclaredLevels <- function(step) {
  if(step$kind=="category")return(unlist(step$levels %||% step$labels,use.names=FALSE))
  if(step$kind!="bins")return(character())
  if(!is.null(step$labels))return(unlist(step$labels,use.names=FALSE))
  b<-.numberBreaks(step$breaks)
  token<-function(x)if(is.infinite(x))if(x<0)"-Inf" else "Inf" else .jcsNumber(x)
  vapply(seq_len(length(b)-1L),function(i)paste0(if(step$closed=="left"&&is.finite(b[i]))"[" else "(",token(b[i]),",",token(b[i+1L]),if(step$closed=="right"&&is.finite(b[i+1L]))"]" else ")"),"")
}

.numberDeclaredUnit <- function(plan,node,value_role=NULL) {
  d<-node$derived;r<-node$reducer;fn<-r$fn %||% ""
  if((d$kind %||% "") %in% c("number_derive","number_daily")) {
    u<-if(d$kind=="number_daily") .numberPresentedUnit(plan,d$input) else d$unit
    return(if(identical(d$reducer$fn,"cv"))"1" else if(identical(d$reducer$fn,"slope"))paste0(u,"/days") else u)
  }
  if(fn %in% c("binary","any","all","abnormal_high","abnormal_low","cv")||identical(d$kind,"died"))return("1")
  if(fn %in% c("count","n_distinct","era_count")||(d$kind %||% "") %in% c("number_count","n_observation_periods"))return("count")
  if(fn %in% c("days_since","days_until","duration_sum","covered_days","supply_days","drug_duration","gap_max","gap_mean","era_days","first_era_days")||(d$kind %||% "") %in% c("observed_days","days_to_death","prior_obs","followup","obs_duration"))return("days")
  if(fn=="time_since")return(if(identical(r$unit,"month"))"months" else "days")
  if(identical(d$kind,"age"))return("years")
  if((d$kind %||% "") %in% c("time_between","age_at"))return(if(d$unit=="annual_years")"years" else d$unit)
  if(identical(node$date_spec$precision,"relative_days"))return("days")
  source<-node$source %||% d$source
  ev<-plan$events[[source$event %||% ""]]
  value<-value_role %||% d$value %||% r$value %||% "value"
  # A source map applies only to its measured value and paired ranges. Supply
  # and unrelated numeric columns retain their registered coordinates.
  measured<-value %in% c("value","range_low","range_high") ||
    is.null(value_role)&&value %in% c(ev$value$column %||% "value_as_number")
  u<-if(value %in% c("supply","days_supply"))"days" else if(measured) {
    if(!is.null(ev$unit))paste0("unit:",ev$unit$target) else if((ev$table %||% "") %in% c("measurement","observation"))"unresolved_source" else "1"
  } else if(value %in% c("value_source","range_low_source","range_high_source"))"unresolved_source" else "1"
  if(measured&&!is.null(ev$cost))u<-if(!is.null(ev$cost$currency))paste0("currency:",ev$cost$currency) else "unresolved_source"
  if(identical(d$kind,"number_change")) {
    if(d$method=="percent_change")return("percent")
    if(d$method=="ratio")return("1")
    if(d$method=="time_to_threshold")return(d$time_unit %||% "days")
    if(d$method=="slope")return(paste0(u,"/",d$time_unit %||% "days"))
  }
  if(fn=="slope")paste0(u,"/days") else u
}

# Order-preserving JSON with exact round-tripping binary64 constants.
.numberJson <- function(x) {
  if (!is.list(x)) {
    if (!is.null(x) && length(x) != 1L) {
      parts <- vapply(seq_along(x), function(i) .numberJson(x[[i]]), "")
      return(paste0("[", paste(parts, collapse = ","), "]"))
    }
    return(.jcsCanonical(x))
  }
  nms <- names(x)
  if (is.null(nms)) {
    return(paste0("[", paste(vapply(x, .numberJson, ""), collapse = ","), "]"))
  }
  if (!length(x)) return("{}")
  if (anyNA(nms) || any(!nzchar(nms)) || anyDuplicated(nms)) {
    stop("JSON object keys must be non-empty and unique.", call. = FALSE)
  }
  parts <- vapply(seq_along(x), function(i) {
    paste0(.jcsCanonical(nms[[i]]), ":", .numberJson(x[[i]]))
  }, "")
  paste0("{", paste(parts, collapse = ","), "}")
}


# Reserved numeric roles are structural in execution and public-plan schemas.
.numberStructuralColumns <- function(format) {
  c("cohort_definition_id",switch(format,
    long=c("event_number","visit_occurrence_id","event_id","unit_concept_id","operator_concept_id"),
    sparse=,temporal_covariates=c("covariate_id","analysis_id","time_id","start_day","end_day"),
    person_period=c("period_id","bin_id","time_id"),
    survival=c("event","event_code","event_id","status","stratum_id","interval_number","transition_id","from_state","to_state","weight","sampling_weight","from","to","trans","state_visit_number","event_number","outcome_event_number"),character()))
}

# Generated output measurements use this common eligibility/unit declaration.
.numberOutputDurations <- function(format) {
  switch(format,
    long=c("start_days","end_days","days_from_index","days_since_previous","duration_days"),
    intervals=c("start_days_from_index","end_days_from_index","duration_days"),
    survival=c("followup_days","follow_up_days","Tstart","Tstop","time","time_at_risk","entry_days_from_index","exit_days_from_index","event_days_from_index","interval_start_days","interval_end_days"),character())
}

# The coordinate after explicit operand stages, without outer display defaults.
.numberPresentedUnit <- function(plan, node) {
  unit <- .numberDeclaredUnit(plan,node)
  for(step in .numberEffective(node$number_spec)$pipeline) {
    if(step$kind=="event_unit") unit<-paste0("unit:",step$spec$target)
    if(step$kind=="fixed_unit") unit<-step$to
    if(step$kind=="expression") unit<-step$unit %||% .numberExpressionUnit(step$exprs,step$result,unit)
    if(step$kind %in% c("flag","present","missing")) unit<-"1"
    if(step$kind %in% c("bins","category")) unit<-"categorical"
  }
  unit
}
