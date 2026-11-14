# Release schema contains declarations and types only. Never inspect cells,
# frequencies, occurrence, availability, selected dates, or source strings.
.categoryReleaseSchema <- function(ctx,frame,columns) {
  physical<-attr(frame,"omop_column_names") %||% setNames(names(frame),names(frame))
  numbers<-attr(frame,"omop_numbers") %||% list()
  cols<-lapply(names(frame),function(name) {
    key<-names(physical)[match(name,physical)];if(is.na(key))key<-name
    c<-columns[[key]];owner<-attr(c,"concept_owner");v<-if(length(owner))ctx$ir$variables[[owner]] else NULL
    category<-attr(frame,"omop_concepts")[[key]]
    number<-numbers[[key]]
    value<-frame[[name]]
    list(stable_key=key,name=name,meaning=if(length(owner))paste("Declared variable",owner) else paste("Output role",c$role %||% "unresolved"),
      semantic_type=if(!is.null(category))category$kind %||% "concept" else if(.categoryFlag(v,c))"flag" else c$role %||% "unresolved",
      r_type=if(is.ordered(value))"ordered_factor" else if(is.factor(value))"factor" else if(inherits(value,"Date"))"Date" else if(inherits(value,"integer64"))"integer64" else typeof(value),
      storage_type=typeof(value),unit=number$unit %||% "not_applicable",
      derivation=list(variable=v,number=number,repeated=attr(c,"repeated_contract"),category_input=attr(c,"category_input_contract")),
      privacy_class=attr(c,"privacy_class") %||% if(!is.null(v$date_spec))2L else "unresolved",
      aggregate_policy="Existing endpoint admission and protection; unresolved for unrequested endpoints",
      missing_policy="Ordinary available NA follows the declaration; unavailable and undefined anchors remain NA",
      provenance=list(source="bound plan and admitted release schema"),completeness="reconciled")
  })
  list(columns=cols,row_grain=if("cohort_row_id" %in% names(frame))"episode" else if("person_id" %in% names(frame))"person" else "none",row_keys=names(columns)[vapply(columns,function(c)c$role %in% c("person_key","episode_key","analytic_key"),logical(1))])
}
