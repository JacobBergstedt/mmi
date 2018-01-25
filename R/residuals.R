#' @include specifications.R models.R
NULL

#' @export
residuals.mmi_model <- function(object) {
  # NOTE: should check on the family level that all predictors are the same!!!
  # And clarify in the documentation that it only works for different response variables
  # with the same predictors
  res <- residuals(object@fit)
  setup_resid_tib(object, res)
}

#' @export
residuals.mmi_lm <- function(object) {
  hat <- lm.influence(object@fit, do.coef = FALSE)$hat
  hat <- hat[hat > 0]
  res <- residuals(object@fit)
  ok <- !(is.na(res))
  res <- res[ok]
  rdf <- object@fit$df.residual
  stddev <- sqrt(sum(res^2)/rdf)
  setup_resid_tib(object, res/(sqrt(1 - hat) * stddev))
}


