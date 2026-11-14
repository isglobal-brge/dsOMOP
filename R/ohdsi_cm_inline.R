# Actual pinned CohortMethod 5.5.2 export variant. It uses the existing public
# cm_result projection/protection; the additional exported measures stay private.
.ohdsiCmInlineExportColumns <- function() {
  c(analysis_id="integer",target_id="integer",comparator_id="integer",outcome_id="integer",
    rr="number",ci_95_lb="number",ci_95_ub="number",p="number",one_sided_p="number",
    target_subjects="number",comparator_subjects="number",target_days="number",comparator_days="number",
    target_outcomes="number",comparator_outcomes="number",log_rr="number",se_log_rr="number",llr="number",
    calibrated_rr="number",calibrated_ci_95_lb="number",calibrated_ci_95_ub="number",calibrated_p="number",
    calibrated_one_sided_p="number",calibrated_log_rr="number",calibrated_se_log_rr="number",
    target_estimator="text",database_id="text")
}
.ohdsiCmInlineReferences <- function(table) {
  prefix<-sub("cm_result$","",tolower(table))
  setNames(list(c(analysis_id="integer",description="text",definition="text"),
    c(outcome_id="integer",outcome_of_interest="number",true_effect_size="number",target_id="integer",comparator_id="integer")),
    paste0(prefix,c("cm_analysis","cm_target_comparator_outcome")))
}
.ohdsiCmInlineStructure <- function(handle,table,info) {
  expected<-.ohdsiCmInlineExportColumns()
  check<-function(info,expected) {
    if(is.null(info)||!all(c("column_name","data_type") %in% names(info)))return(FALSE)
    names<-tolower(info$column_name);types<-setNames(.ohdsiSchemaType(info$data_type),names)
    if(anyDuplicated(names)||!setequal(names,names(expected)))return(FALSE)
    all(ifelse(expected=="number",types[names(expected)] %in% c("number","integer"),types[names(expected)]==expected))
  }
  if(!check(info,expected))return(NULL)
  refs<-.ohdsiCmInlineReferences(table)
  metadata<-list()
  for(n in names(refs)) {
    info<-tryCatch(.listColumnsRaw(handle,n,.effectiveResultsSchema(handle)),error=function(e)NULL)
    if(!check(info,refs[[n]]))return(NULL)
    metadata[[n]]<-setNames(as.list(tolower(info$data_type)),tolower(info$column_name))
  }
  metadata
}
.ohdsiValidateCmInlineReferences <- function(handle,physical,layout) {
  if(!identical(layout,"cm-inline-export-5.5.2-1"))return(invisible(NULL))
  refs<-.ohdsiCmInlineReferences(physical$table_name)
  keys<-list("analysis_id",c("target_id","comparator_id","outcome_id"))
  checks<-character()
  for(i in seq_along(refs)) {
    table<-.qualifyTable(handle,names(refs)[i],.effectiveResultsSchema(handle))
    k<-keys[[i]]
    invalid<-paste0(k," IS NULL")
    if(identical(handle$target_dialect,"sqlite"))invalid<-c(invalid,paste0("TYPEOF(",k,") <> 'integer'"))
    checks<-c(checks,paste0("SELECT TOP 1 1 AS invalid FROM ",table," WHERE ",paste(invalid,collapse=" OR ")),
      paste0("SELECT TOP 1 1 AS invalid FROM ",table," GROUP BY ",paste(k,collapse=",")," HAVING COUNT(*) > 1"),
      paste0("SELECT TOP 1 1 AS invalid FROM ",physical$qualified_name," r WHERE NOT EXISTS (SELECT 1 FROM ",table," a WHERE ",paste0("a.",k," = r.",k,collapse=" AND "),")"))
  }
  # This reader's single-database identity and ATE interpretation are closed.
  # A new estimator or combined-database dimension needs a versioned adapter.
  checks<-c(checks,paste0("SELECT TOP 1 1 AS invalid FROM ",physical$qualified_name,
    " WHERE database_id IS NULL OR database_id = '' OR target_estimator IS NULL OR target_estimator <> 'ate'"),
    paste0("SELECT MAX(1) AS invalid FROM ",physical$qualified_name," HAVING COUNT(DISTINCT database_id) > 1"))
  bad<-tryCatch(any(vapply(checks,function(sql)nrow(.executeQuery(handle,.sql_translate(sql,handle$target_dialect)))>0L,logical(1))),error=function(e)TRUE)
  if(bad)stop("results_schema_integrity: /results/references; use a unique, associated single-database ATE export with complete inline reference identities.",call.=FALSE)
  invisible(NULL)
}
