#' @include specifications.R models.R
NULL

#  Confidence methods ------------------------------------------------------------------
#' Generic method to compute confidence regions for mmi model and family objects.
#'
#' For \code{\linkS4class{mmi_lm}} objects a confidence interval is constructed using the
#' t-distribution assumption for the linear parameter over the standard error. The standard
#' error is estimated using sandwich estimation. For the other models the confidence intervals
#' are computed using the profile likelihood. A tidy tibble will be setup with the information.
#' For a \code{\linkS4class{fam}} object confidence intervals will be computed for each model
#' element using the individual methods, and the tibbles will then be binded together in a
#' map-reduce pattern.
#' @param object Either a mmi model object or a mmi family object.
#' @param level The confidence level.
#' @export
confidence <- function(object, level, ...) {
  UseMethod("confidence")
}

#' @export
confidence.mmi_model <- function(object, level) {
  inv_trans <- inv(object@trans)
  est <- coef(object)
  confs <- warn(confint(object@fit, parm = object@var_labels[, "levels"], level = level,
                        quiet = TRUE),
                object,
                "estimation of confidence intervals")
  confs <- inv_trans(confs)
  confs <- simplify_col_sel(confs)
  tibble(response = object@response,
         variables = object@var_labels[, "variable"],
         levels = object@var_labels[, "levels"],
         est = inv_trans(est),
         lower = confs$lower,
         higher = confs$higher,
         trans = object@trans)
}

#' @describeIn confidence
#' Computes the confidence interval using the t-distribution assumption and sets up a
#' tidy tibble with the information. Uses the sandwich estimated standard errors.
#' @export
confidence.mmi_lm <- function(object, level) {
  inv_trans <- inv(object@trans)
  mu <- coef(object)
  alpha <- (1 - level) / 2
  p_thresh <- qt(1 - alpha, object@fit$df.residual)
  tibble(response = object@response,
         variables = object@var_labels[, "variable"],
         levels = object@var_labels[, "levels"],
         est = inv_trans(mu),
         lower = inv_trans(mu - p_thresh * object@se),
         higher = inv_trans(mu + p_thresh * object@se),
         trans = object@trans)
}

