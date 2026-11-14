# Versioned fidelity descriptions for existing analysis routes. No estimator or
# scientific default is changed by these public declarations.
.ohdsiMethodScope <- function(id,adapter) {
  if(identical(adapter,"ohdsi"))return(list(compatibility="local_port",estimand="protected physical result reader",evaluation="exported result; upstream schema/version must be recognized separately",limit="registration does not imply public readability"))
  if(grepl("fe[.]prevalence",id))return(list(estimand="all-history event prevalence among scoped cohort members",evaluation="descriptive",limit="no cohort-relative date predicate; use native bounded features for a timed estimand"))
  if(identical(id,"dsomop:incidence.rate"))return(list(estimand="first target episode persons-with-event over full-window person-days",evaluation="descriptive",limit="recurrent counts separate; no clean-time subtraction; episode-time is not unioned person-time"))
  if(identical(id,"dsomop:cm.effect_estimate"))return(list(estimand="crude arm-only Cox HR or Poisson RR",evaluation="unadjusted outcome fit",limit="propensity diagnostics do not adjust this outcome fit"))
  if(grepl("sccs.*(sccs_result|outcome_rate)",id))return(list(estimand="outcome rate per month",evaluation="descriptive",limit="not the conditional exposed/unexposed SCCS estimator"))
  if(grepl("sccs",id))return(list(estimand="route-declared SCCS preparation or conditional estimator",evaluation="see route settings",limit="person-time partition preparation alone is not a fitted SCCS model"))
  if(id %in% c("dsomop:plp.performance","dsomop:plp.calibration","dsomop:plp.threshold"))return(list(estimand="fit-and-predict on the same data",evaluation="in_sample/resubstitution",limit="not held-out or externally validated performance"))
  if(grepl("drug_util|adherence|pdc|mpr",id))return(list(estimand="declared exposure duration or supply utilization",evaluation="descriptive",limit="start-based washout does not exclude spanning exposure; PDC/MPR is not ingested ingredient dose"))
  if(grepl("survival|kaplan",id))return(list(estimand="route-declared survival summary",evaluation="see protected route",limit="band-derived displayed summaries are not fitted Kaplan-Meier; competing-risk assignment is not Aalen-Johansen"))
  if(grepl("cohort_diag|cohortdx",id))return(list(estimand="declared standard-concept, overlap or temporal summaries",evaluation="descriptive",limit="not complete source-mapping diagnostics or inclusion-rule attrition"))
  if(grepl("achilles|query",id)||adapter %in% c("querylibrary","achilles"))return(list(estimand="curated question/analysis",evaluation="route and backend specific",limit="SQL rendering is not executed-backend acceptance; no full upstream catalogue claim"))
  if(grepl("dqd|quality",id))return(list(estimand="declared schema/profiling/plausibility checks",evaluation="local checks",limit="not a run of DQD; not-run, unsupported, unavailable and suppressed never mean pass"))
  if(grepl("score|charlson|chads|hfrs|dcsi",id))return(list(estimand="route-declared score weights/window",evaluation="local implementation",limit="one-point burden approximation and traced extraction score are distinct; no automatic upstream equivalence"))
  list(estimand="route-declared analysis",evaluation="unresolved",limit="no validated upstream equivalence, negative-control calibration or network-module claim")
}
.ohdsiRouteDeclaration <- function(entry,settings=NULL,protection="unresolved",scope=NULL,combine="union") {
  id<-entry$meta$alias_target %||% entry$name
  scope<-.ohdsiMethodScope(id,entry$meta$adapter %||% "unknown")
  defaults<-setNames(lapply(entry$params,function(p)p$default %||% "unresolved"),vapply(entry$params,function(p)p$name,""))
  settings<-settings %||% defaults
  roles<-list(status="unresolved",arms="ordered scope: target then comparator where the route accepts two cohorts",
    outcome="concept-filter where outcome_concept_id is declared; never a cohort-definition ID")
  public_ids<-function(x) {
    if(is.list(x)) {
      if(is.object(x)||!all(vapply(x,function(v)!is.object(v)&&is.atomic(v)&&length(v)==1L&&(is.numeric(v)||is.character(v)),logical(1))))return(NULL)
      x<-unlist(x,use.names=FALSE)
    }
    if(is.numeric(x)&&!is.object(x)&&all(is.finite(x))&&all(x==trunc(x))&&all(abs(x)<=9007199254740991))return(as.list(format(x,scientific=FALSE,trim=TRUE,digits=22)))
    if(is.character(x)&&length(x)&&!anyNA(x)&&all(grepl("^(0|[1-9][0-9]*)$",x)))return(as.list(x))
    NULL
  }
  ids<-public_ids(scope)
  if(length(ids))roles<-list(status="declared",ordered_cohort_definitions=ids,combine=combine,
    target=ids[[1]],comparator=if(length(ids)>1L)ids[[2]] else "not_applicable",
    outcome_definition=list(kind=if("outcome_concept_id" %in% names(settings))"concept-filter" else "unresolved",
      definition=settings$outcome_concept_id %||% "unresolved"))
  d<-list(schema_version="ohdsi-declaration-1",compatibility=scope$compatibility %||% "local_port",
    method_id=id,method_version="1.0.0",adapter_version="2.10.5",
    settings_digest=paste(openssl::sha256(charToRaw(.jcsCanonical(.pnv2Plain(list(settings=settings,roles=roles)))))),
    upstream=list(package=entry$meta$pack_package %||% "unknown",version=entry$meta$pack_version %||% "unknown",fixture="unresolved",source_definition_hash="unresolved"),
    identity=list(namespace=if(grepl("fe[.]",id))"dsomop.catalogue_concept.v1" else "not_applicable",scope_kind="analysis_settings",definition=settings),
    study_roles=roles,
    temporal_contract=list(settings=settings,closure="route-specific; see method and parameter declarations"),
    era_contract=list(status="not_applicable"),vocabulary_contract=list(cdm_version=entry$meta$cdm_version %||% "unresolved",vocabulary_version="unresolved"),
    estimation_contract=list(estimand=scope$estimand,evaluation=scope$evaluation,limits=scope$limit,
      unit_of_analysis=entry$disclosure$unit %||% "unresolved",protection=protection,
      scientific_settings=settings,contribution_caps="separate protected release contract; not scientific window settings"))
  d$identity$scope_digest<-d$settings_digest
  d
}
