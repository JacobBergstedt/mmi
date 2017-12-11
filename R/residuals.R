#' @include specifications.R models.R
NULL

#' @export
residuals.mmi_model <- function(object) {
  res <- residuals(object@fit)
  tib <- setup_compare_tib(object, res)
  tib$exp_norm <- qnorm(ppoints(length(res)))[order(order(res))]
  tib
}

residuals.mmi_lm <- function(object) {
  hat <- lm.influence(object@fit, do.coef = FALSE)$hat
  hat <- hat[hat > 0]
  res <- residuals(object@fit)
  ok <- !(is.na(res))
  res <- res[ok]
  rdf <- object@fit$df.residual
  stddev <- sqrt(sum(res^2)/rdf)
  tib <- setup_compare_tib(object, res/(sqrt(1 - hat) * stddev))
  tib$exp_norm <- qnorm(ppoints(length(res)))[order(order(res))]
  tib
}


