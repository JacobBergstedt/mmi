#' @include specifications.R models.R
NULL

#  Confidence methods ------------------------------------------------------------------
#' Generic method to compute confidence regions for mmi model and family objects.
#'
#' For \code{\linkS4class{mmi_lm}} objects a confidence interval for treatment terms
#' is constructed using the t-distribution assumption
#' for the linear parameter over the standard error. The standard
#' error is estimated using sandwich estimation. For the other models the confidence intervals
#' are computed using the profile likelihood. A tibble will be setup with the information.
#' For a \code{\linkS4class{fam}} object confidence intervals will be computed for each model
#' element using the individual methods, and the tibbles will then be binded together in a
#' map-reduce pattern.
#' @param object Either a mmi model object or a mmi family object.
#' @param level The confidence level.
#' @export
confidence <- function(object, level, ...) {
  UseMethod("confidence")
}

#' @describeIn confidence
#' Confidence method for the superclass \code{\linkS4class{mmi_model}}. Uses confint.
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
         variable = object@var_labels[, "variable"],
         levels = object@var_labels[, "levels"],
         est = inv_trans(est),
         lower = confs$lower,
         higher = confs$higher,
         trans = object@trans,
         model_id = object@id)
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
         variable = object@var_labels[, "variable"],
         levels = object@var_labels[, "levels"],
         est = inv_trans(mu),
         lower = inv_trans(mu - p_thresh * object@se),
         higher = inv_trans(mu + p_thresh * object@se),
         trans = object@trans,
         model_id = object@id)
}

# FCR function ----------------------------------------------------------------------
#' @export
select_confidence <- function(fam, test_tib, crit = "FDR", thresh = 0.05, level = 0.95) {
  if (!class(fam) == "fam") stop("fam parameter must be a fam object")
  if (!crit %in% names(test_tib)) stop("crit not a name in test_tib")
  if (is.null(names(fam))) stop("This function currently only works for named families")
  ntot <- nrow(test_tib)
  test_tib <- test_tib[test_tib[[crit]] < thresh, c("response", "variable", "model_id")]
  nhits <- nrow(test_tib)
  fcr_alpha <- (1 - level) * nhits / ntot
  confidence(fam[names(fam) %in% test_tib$model_id], level = 1 - fcr_alpha)
  right_join(test_tib)
}
