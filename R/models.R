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


# Variance component estimations ----------------------------------------------------


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


