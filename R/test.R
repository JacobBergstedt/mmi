#' @include specifications.R
NULL


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
  tib <- setup_test_tib(object, p)
  tib$treatment_levels <- NULL
  tib
}

#' @describeIn test
#' Performs an lrt and setups a tidy tibble with the information.
#' @export
test.mmi_logreg <- function(object) {
  ll_null <- warn(logLik(glm(object@null_formula, object@fit$model, family = "binomial")),
                  object,
                  "LRT logistic regresion")
  tib <- setup_test_tib(object, p_lrt(ll_null, logLik(object@fit)))
  tib$treatment_levels <- NULL
  tib
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
  tib <- setup_lrt_tib(object, p_lrt(ll_null, logLik(object@fit)))
  tib$treatment_levels <- NULL
  tib
}

#' Performs a likelihood ratio test for the \linkS4class{lmerMod} in the fit slot and sets up
#' a tidy tibble for the result.
#' @export
lrt.mmi_lmm <- function(object) {
  ll_null <- logLik(lmer(object@null_formula, object@fit@frame, REML = FALSE))
  ll_alt <- logLik(refitML(object@fit))
  tib <- setup_lrt_tib(object, p_lrt(ll_null, ll_alt))
  tib$treatment_levels <- NULL
  tib
}

#' Performs a likelihood ratio test for the binomial \code{\link[stats]{glm}} in the fit slot
#' and sets up a tidy tibble with the result.
#' @export
lrt.mmi_logreg <- function(object) {
  tib <- test(object)
  tib[1, ]
}

#' @export
ft <- function(object) UseMethod("ft")

#' @export
ft.mmi_lmm <- function(object) {
  tib <- test.mmi_lmm(object)
  tib[1, ]
}