#' @include specifications.R models.R
NULL

#  S3 generic method test --------------------------------------------------------------
#' S3 generic method to do hypothesis tests in the mmi model and family objects.
#'
#' Generic method to do tests for the mmi model and family objects. For the models the appropriate
#' tests will be done depending on model type. Currently, a t-test using sandwich estimated standard errors
#' is done for \code{\linkS4class{mmi_lm}} and \code{\link[pbkrtest]{KRmodcomp}} Kenward-Rogers F-test is done for
#' \code{\linkS4class{mmi_lmm}} objects. An \code{\link{lrt}} is done for all other model objects.
#' The information is stored in a tibble.
#' For a \code{\linkS4class{fam}} object, a test will be performed for each element in the family
#' and the information will be binded together in a using a map-reduce pattern.
#' The FDR-adjusted significance level is also calculated and added to the tibble.
#' @param object An mmi model object.
#' @export
test <- function(object, ...) UseMethod("test")

#' @describeIn test
#' Method for superclass \code{linkS4class{mmi_model}}
#' @export
test.mmi_model <- function(object) {
  lrt(object)
}
#' @describeIn test
#' Performs a t-test with the sandwiched-estimated standard errors and sets up a tibble
#' with the information.
#' @export
test.mmi_lm <- function(object) {
  wald(object)
}

#' @describeIn test
#' Performs an F test and setups a tibble with the information.
#' @export
test.mmi_lmm <- function(object) {
  ft(object)
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
#'
#' Will loop through all treatment variables and fit null models for them. This is done using
#' update methods for the model fit if that works (depends on how the model fit is implemented),
#' otherwise it's done by using update on the \code{\link{get_formula}} formula. P values are
#' then computed using the \code{\link{logLik}} method on the null and full models.
#' @param object Either an mmi model object or an mmi model family object.
#' @export
lrt <- function(object) UseMethod("lrt")

#' @describeIn lrt
#' Performs a likelihood ratio test for the \code{\link[stats]{lm}} object in the
#' slot fit and sets up a tibble for the result.
#' @export
lrt.mmi_model <- function(object) {
  nulls <- fit_all_nulls(object, REML = FALSE)
  alt_log_lik <- logLik(object@fit)
  p <- map_dbl(nulls, ~ p_lrt(logLik(.), alt_log_lik))
  setup_lrt_tib(object, p, "lrt")
}

# ft --------------------------------------------------------------------------------
#' Generic method for F-tests.
#'
#' A method is implemented for all the mmi model classes. A method is also implemented for
#' the family class. In that case the method is basically a loop over all models stored in
#' the family and then the method for the model is called for each model element. If the method
#' is called on a model object a tidy tibble will be set up with the result for the treatment
#' or the treatment levels. If the method is called on a family object individual tibbles will
#' be set up for each model element which will then be binded together in a map-reduce pattern.
#'
#' Will loop through all treatment variables and fit null models for them. This is done using
#' update methods for the model fit if that works (depends on how the model fit is implemented),
#' otherwise it's done by using update on the formula. P values are
#' then computed using appropriate functions for each model class.
#' For \code{\linkS4class{mmi_lmm}} objects
#' \code{\link[pbkrtest]{KRmodcomp}} Kenward-Rogers F-test are used.
#' @param object Either an mmi model object or an mmi model family object.
#' @export
ft <- function(object) UseMethod("ft")

#' @describeIn ft
#' Performs a F test for the \code{\link[stats]{lm}} object in the
#' slot fit and sets up a tibble for the result.
#' @export
ft.mmi_lm <- function(object) {
  nulls <- fit_all_nulls(object)
  p <- map_dbl(nulls, ~ anova(object@fit, .)[-1, "Pr(>F)"])
  setup_lrt_tib(object, p, "ft")
}

#' @describeIn ft
#' Performs a F test for the \code{\link[lme4]{lmerMod}} object in the slot fit
#' using a \code{\link[pbkrtest]{KRmodcomp}} Kenward-Rogers F-test
#' and sets up a tibble with the result.
#' @export
ft.mmi_lmm <- function(object) {
  nulls <- warn(fit_all_nulls(object), object, "fitting of null model")
  setup_lrt_tib(object, map_dbl(nulls, ~ KRmodcomp(object@fit, .)$stats$p.value), "ft")
}

#' A method is implemented for all the mmi model classes. A method is also implemented for
#' the family class. In that case the method is basically a loop over all models stored in
#' the family and then the method for the model is called for each model element. If the method
#' is called on a model object a tidy tibble will be set up with the result for the treatment
#' or the treatment levels. If the method is called on a family object individual tibbles will
#' be set up for each model element which will then be binded together in a map-reduce pattern.
#'
#' Currently only implemented for \code{\linkS4class{mmi_lm}} objects. Standard wald test.
#' @param object Either an mmi model object or an mmi model family object.
#' @export
wald <- function(object) UseMethod("wald")

#' @describeIn wald
#' Performs a wald test for \code{\linkS4class{mmi_lm}} objects.
#' @export
wald.mmi_lm <- function(object) {
  est <- coef(object@fit)[object@var_labels[, "levels"]]
  t <- est / object@se
  p <- 2 * pt(abs(t), object@fit$df.residual, lower.tail = FALSE)
  tibble(response = object@response,
         variable = object@var_labels[, "variable"],
         levels = object@var_labels[, "levels"],
         test = "wald",
         p = p,
         model = str_extract(class(object), "(?<=mmi_).*"),
         model_id = object@id)
}
