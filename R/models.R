#' @include specifications.R
NULL

# mmi_model classes -------------------------------------------------------
.make_model <- setClass("mmi_model",
                        slots = c(trt_levels = "character",
                                  formula = "formula",
                                  null_formula = "formula"))

#' S4 class for representing a linear model specification and a fit to that specification.
#'
#' Inherits from \code{\linkS4class{spec_lm}}. An \code{\link[stats]{lm}} fit and some
#' technicalities needed from the fit are added.
#'
#' @slot fit The fitted \code{\link[stats]{lm}} object.
#' @slot se The standard error of the treatment (levels) estimated using
#' \code{\link[sandwich]{vcovHC}}
#' @slot trt_levels The names of the treatment variables given by \code{\link[stats]{lm}}
.make_lm <- setClass("mmi_lm",
                   slots = c(fit = "lm", se = "numeric"),
                   contains = c("spec_lm", "mmi_model"))

#' S4 class for representing a linear mixed model specification and a fit to that specification.
#'
#' Inherits from \code{\linkS4class{spec_lmm}}. Vehicle that contains a
#' \code{\linkS4class{lmerMod}} fit to a model specification and some additional information.
#' @slot fit The \code{\linkS4class{lmerMod}} fit to the contained model specification.
#' @slot trt_levels The names of the treatment variables given by \code{\linkS4class{lmerMod}}
.make_lmm <- setClass("mmi_lmm",
                      slots = c(fit = "lmerMod"),
                      contains = c("spec_lmm", "mmi_model"))

#' S4 class for representing a logstic regression specification and a fit to that specification.
#'
#' Inherits from \code{\linkS4class{spec_logreg}}. A \code{\link[stats]{glm}} binomial
#' log-link fit is added along with some additional information.
#'
#' @slot fit The fitted \code{\link[stats]{glm}} object.
#' @slot trt_levels The names of the treatment variables given by \code{\link[stats]{glm}}
.make_logreg <- setClass("mmi_logreg",
                         slots = c(fit = "glm"),
                         contains = c("spec_logreg", "mmi_model"))

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

#' @describeIn confidence
#' Computes the confidence interval using the t-distribution assumption and sets up a
#' tidy tibble with the information. Uses the sandwich estimated standard errors.
#' @export
confidence.mmi_lm <- function(object, level) {
  inv_trans <- inv(object@trans)
  mu <- coef(object@fit)[object@trt_levels]
  alpha <- (1 - level) / 2
  p_thresh <- qt(1 - alpha, object@fit$df.residual)
  tibble(response = object@response,
         treatment = object@treatment,
         est = inv_trans(mu),
         lower = inv_trans(mu - p_thresh * object@se),
         higher = inv_trans(mu + p_thresh * object@se))
}

#' @describeIn confidence
#' Computes the confidence interval using profile likelihood (the confint method)
#' and sets up a tidy tibble with the information.
#' @export
confidence.mmi_logreg <- function(object, level) {
  inv_trans <- inv(object@trans)
  est <- coef(object@fit)[object@trt_levels]
  confs <- confint(object@fit, parm = object@trt_levels, level = level)
  confs <- inv_trans(confs)
  if (is.vector(confs)) {
    lower <- confs[1]
    higher <- confs[2]
  } else {
    lower <- confs[, 1]
    higher <- confs[, 2]
  }
  tibble(response = object@response,
         treatment = object@treatment,
         treatment_levels = object@trt_levels,
         est = inv_trans(est),
         lower = lower,
         higher = higher)
}

#' @describeIn confidence
#' Computes the confidence interval using profile likelihood (the confint method)
#' and sets up a tidy tibble with the information.
#' @export
confidence.mmi_lmm <- function(object, level) {
  inv_trans <- inv(object@trans)
  est <- inv_trans(fixef(object@fit)[object@trt_levels])
  confs <- warn(confint(object@fit, parm = object@trt_levels, level = level, quiet = TRUE),
                object,
                "estimation of confidence intervals")
  confs[object@trt_levels, ] <- inv_trans(confs[object@trt_levels, ])
  confs <- simplify_col_sel(confs)
  tibble(response = object@response,
         treatment = object@treatment,
         treatment_levels = object@trt_levels,
         est = est,
         lower = confs$lower,
         higher = confs$higher)
}

#  S3 generic method test --------------------------------------------------------------
#' S3 generic method to do hypothesis tests in the mmi model and family objects.
#'
#' Generic method to do tests for the mmi model and family objects. For the models the appropriate
#' tests will be done depending on model type. Currently, a t-test using sandwich estimated standard errors
#' is done for \code{\linkS4class{mmi_lm}} and an \code{\link{lrt}} is done for all other model objects.
#' The information will be stored in a tidy tibble.
#' For a \code{\linkS4class{fam}} object, a test will be performed for each element in the family
#' and the information will be binded together in a tidy way using a map-reduce pattern.
#' The FDR-adjusted significance level is also calculated and added to the tibble.
#' @param object An mmi model object.
#' @export
test <- function(object, ...) UseMethod("test")

#' @describeIn test
#' Performs a t-test with the sandwiched-estimated standard errors and sets up a tidy tibble
#' with the information.
#' @export
test.mmi_lm <- function(object) {
  est <- coef(object@fit)[object@trt_levels]
  t <- est / object@se
  p <- 2 * pt(abs(t), object@fit$df.residual, lower.tail = FALSE)
  setup_test_tib(object, p)
}

#' @describeIn test
#' Performs an lrt and setups a tidy tibble with the information.
#' @export
test.mmi_lmm <- function(object) {
  null <- warn(lmer(object@null_formula, object@fit@frame),
               object,
               "fitting of null model")
  p <- KRmodcomp(object@fit, null)$stats$p.value
  setup_test_tib(object, p)
}

#' @describeIn test
#' Performs an lrt and setups a tidy tibble with the information.
#' @export
test.mmi_logreg <- function(object) {
  ll_null <- logLik(glm(object@null_formula, object@fit$model, family = "binomial"))
  setup_test_tib(object, p_lrt(ll_null, logLik(object@fit)))
}

# S3 method lrt        ---------------------------------------------------------------
#' Generic method for likelihood ratio tests.
#'
#' A method is implemented for all the mmi model classes. A method is also implemented for
#' the family class. In that case the method is basically a loop over all models stored in
#' the family and then the method for the model is called for each model element. If the method
#' is called on a model object a tidy tibble will be set up with the result for the treatment
#' or the treatment levels. If the method is called on a family object individual tibbles will
#' be set up for each model element which will then be binded together in a map-reduce pattern.
#' @param object Either an mmi model object or an mmi model family object.
#' @export
lrt <- function(object) UseMethod("lrt")

#' Performs a likelihood ratio test (an F-test) for the \code{\link[stats]{lm}} object in the
#' slot fit and sets up a tidy tibble for the result.
#' @export
lrt.mmi_lm <- function(object) {
  ll_null <- logLik(lm(object@null_formula, object@fit$model))
  setup_lrt_tib(object, p_lrt(ll_null, logLik(object@fit)))
}

#' Performs a likelihood ratio test for the \linkS4class{lmerMod} in the fit slot and sets up
#' a tidy tibble for the result.
#' @export
lrt.mmi_lmm <- function(object) {
  ll_null <- logLik(lmer(object@null_formula, object@fit@frame, REML = FALSE))
  ll_alt <- logLik(refitML(object@fit))
  setup_lrt_tib(object, p_lrt(ll_null, ll_alt))
}

#' Performs a likelihood ratio test for the binomial \code{\link[stats]{glm}} in the fit slot
#' and sets up a tidy tibble with the result.
#' @export
lrt.mmi_logreg <- function(object) {
  tib <- test(object)
  tib$treatment_levels <- NULL
  tib[1, ]
}

#' @export
ft <- function(object) UseMethod("ft")

#' @export
ft.mmi_lmm <- function(object) {
  tib <- test.mmi_lmm(object)
  tib$treatment_levels <- NULL
  tib[1, ]
}

#' @export
prop_var <- function(object) UseMethod("prop_var")

#' @export
prop_var.mmi_lmm <- function(object) {
  est <- as.data.frame(lme4::VarCorr(object@fit))[c("grp", "vcov")]
  res_var <- est$vcov[est$grp == "Residual"]
  est <- left_join(tibble(grp = object@rands), est, by = "grp")
  tibble(response = object@response,
         random_effects = object@rands,
         prop_var = 100 * est$vcov / c(est$vcov + res_var))
}


