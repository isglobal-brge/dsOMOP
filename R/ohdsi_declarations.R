# A-31 public declarations only. Never inspect a handle, vocabulary or data row.
.ohdsiDeclaration <- function(plan,variable=NULL,key=NULL,digest=NULL) {
  digest<-digest %||% .planNormalizedDigest(plan)$sha256
  public<-function(x)if(is.null(x)||!length(x))"not_applicable" else x
  variable<-variable %||% list()
  d<-variable$derived;r<-variable$reducer %||% d$reducer
  identity<-list(namespace=if(identical(key,"covariate_id"))"dsomop.native_covariate.v2" else "not_applicable",
    scope_kind="plan",scope_digest=digest,local_id_representation="exact integer or canonical decimal text; never a display label",
    definition=public(list(source=variable$source %||% d$source,quantity=d$unit,
      analysis=r,window=r$window %||% d$window,reference=d$reference)))
  list(schema_version="ohdsi-declaration-1",compatibility="native",
    method_id=if(identical(d$kind,"number_daily"))"dsomop.number.daily" else "dsomop.native_recipe.v2",
    method_version="1.0.0",adapter_version="not_applicable",
    settings_digest=as.character(openssl::sha256(charToRaw(.jcsCanonical(.pnv2Plain(variable))))),
    upstream=list(package="not_applicable",version="not_applicable",fixture="unresolved",source_definition_hash="not_applicable"),
    identity=identity,
    study_roles=list(status="unresolved",population_definitions=public(plan$populations),output_definitions=public(plan$outputs)),
    temporal_contract=list(anchor=public(r$window$anchor %||% d$reference),window=public(r$window %||% d$window),
      inclusivity="closed declared day windows",selection=public(r),coverage=public(plan$options$coverage),
      population_policies=public(plan$populations),outputs=public(plan$outputs)),
    era_contract=list(recorded_namespace="omop.cdm.recorded_era",derived_namespace="dsomop.native_era.v2",
      declarations=public(plan$intervals),native_defaults=list(gap=30L,persistence=0L,by="event"),
      split="next_start > previous_end + gap + 1",persistence="native end extension; not tolerated gap",
      days_supply_end="start + days_supply - 1 where declared fallback applies"),
    vocabulary_contract=list(cdm_version="unresolved",vocabulary_version="unresolved",
      definitions=public(plan$concept_sets),events=public(plan$events),
      mapping_direction="valid Maps to source concept_id_1 to selected target concept_id_2; no patient-fact mapping",
      expansion_digest="unresolved"),
    estimation_contract=list(unit_of_analysis="declared output grain",numerator="not_applicable",denominator="not_applicable",
      rate_multiplier="not_applicable",estimator="private declaration; no fitted-model claim",
      protection="existing final component gate on distinct people; downstream endpoint contract applies",
      contribution_caps="not_applicable",approximation="not_applicable"))
}
