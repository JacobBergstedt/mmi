#' @include specifications.R models.R
NULL


# AIC measures ----------------------------------------------------------------------
#' @export
AIC.mmi_model <- function(object) {
  setup_compare_tib(object, AIC(object@fit))
}

#' @export
AIC.mmi_lm <- function(object) {
  trans <- object@trans
  if (trans == "identity") val <-  AIC(object@fit)
  else if (trans %in% c("log", "log10")) {
    if (trans == "log10") b <- 1 / log(10)
    else b <- 1
    val <- AIC(object@fit) + 2 * (sum(object@fit$model[[object@response]]) + log(b))
  }
  else stop("Transformation not supported")
  setup_compare_tib(object, val)
}

