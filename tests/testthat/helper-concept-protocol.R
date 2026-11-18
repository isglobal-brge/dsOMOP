# dsOMOPClient 2.10.6 always declares concept_protocol = 1 on plan executions
# and catalog assignments: it reconciles every published concept factor. The
# direct server calls in these tests stand in for that client, so these
# wrappers add the declaration and keep each method's formals. The refusal of
# older clients is tested through the namespaced methods (dsOMOP::...).
.with_concept_protocol <- function(name) {
  real <- getExportedValue("dsOMOP", name)
  wrapper <- function() {
    call <- sys.call()
    call[[1L]] <- call("::", as.name("dsOMOP"), as.name(name))
    if (is.null(call$concept_protocol)) call$concept_protocol <- 1
    eval(call, parent.frame())
  }
  formals(wrapper) <- formals(real)
  wrapper
}
omopPlanExecuteDS <- .with_concept_protocol("omopPlanExecuteDS")
omopDpPlanExecuteDS <- .with_concept_protocol("omopDpPlanExecuteDS")
omopAnalysisRunAssignDS <- .with_concept_protocol("omopAnalysisRunAssignDS")
