#' @export
`[.dsomop_date_factor` <- function(x, ..., drop=FALSE) {
  contract<-attr(x,"omop_date_contract")
  out<-NextMethod("[",drop=FALSE)
  if(!is.null(contract)) {
    contract$keys<-contract$levels[match(as.character(out),contract$labels)]
    attr(out,"omop_date_contract")<-contract
  }
  out
}

.dateOrdered <- function(x, levels) {
  out <- ordered(x, levels = levels)
  class(out) <- c("dsomop_date_factor", class(out))
  out
}
