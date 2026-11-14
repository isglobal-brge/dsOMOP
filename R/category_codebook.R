# Release schema contains declarations and types only. Never inspect cells,
# frequencies, occurrence, availability, selected dates, or source strings.
.categoryReleaseSchema <- function(ctx,frame,columns) {
  physical<-attr(frame,"omop_column_names") %||% setNames(names(frame),names(frame))
  numbers<-attr(frame,"omop_numbers") %||% list()
  cols<-lapply(names(frame),function(name) {
    key<-names(physical)[match(name,physical)];if(is.na(key))key<-name
    c<-columns[[key]];role<-c$role %||% "unresolved";owner<-attr(c,"concept_owner");v<-if(length(owner))ctx$ir$variables[[owner]] else NULL
    category<-attr(frame,"omop_concepts")[[key]]
    number<-numbers[[key]]
    value<-frame[[name]]
    semantic<-if(!is.null(category))category$kind %||% "concept" else if(.categoryFlag(v,c))"flag" else if(role %in% c("person_key","episode_key","analytic_key"))"key" else if(role %in% c("measure","value"))"numeric" else if(inherits(value,"Date"))"date" else "structural"
    unit<-number$unit %||% attr(c,"number_origin")$unit %||% if(!is.null(v)&&semantic %in% c("flag","numeric")) .categoryDeclaredUnit(ctx$ir,v) else "not_applicable"
    presentation <- .presentationDescribe(value,domains=FALSE)
    c(list(stable_key=key,name=name,meaning=if(length(owner))paste("Declared variable",owner) else paste("Output role",role),
      semantic_type=semantic,
      unit=unit,
      derivation=list(variable=v,number=number,repeated=attr(c,"repeated_contract"),category_input=attr(c,"category_input_contract")),
      privacy_class=attr(c,"privacy_class") %||% if(role %in% c("person_key","episode_key","analytic_key"))1L else if(!is.null(v$date_spec)||role %in% c("start","end"))2L else if((!is.null(c)&&.conceptColumn(c))||.categoryFlag(v,c)||role %in% c("measure","value"))3L else "unresolved",
      aggregate_policy="Existing endpoint admission and protection; unresolved for unrequested endpoints",
      missing_policy="Ordinary available NA follows the declaration; unavailable and undefined anchors remain NA",
      provenance=list(source="bound plan and admitted release schema"),completeness="reconciled"),presentation)
  })
  list(columns=cols,row_grain=if("cohort_row_id" %in% names(frame))"episode" else if("person_id" %in% names(frame))"person" else "none",row_keys=names(columns)[vapply(columns,function(c)c$role %in% c("person_key","episode_key","analytic_key"),logical(1))])
}
