#' @include specifications.R
NULL


# Register need classes -------------------------------------------------------------
setOldClass("betareg")
setOldClass("negbin")

# mmi_model classes -------------------------------------------------------
#' S4 class for a model representation and model fit. This is the superclass which defines
#' the parsed variable terms in the model matrix.
#' @slot var_labels Parsed variable and variable level names that appears in model.matrix
.make_model <- setClass("mmi_model",
                        slots = c(var_labels = "matrix"))

#' S4 class for representing a linear model specification and a fit to that specification.
#'
#' Inherits from \code{\linkS4class{spec_lm}} and \code{\linkS4class{mmi_model}}.
#' An \code{\link[stats]{lm}} fit and sandwich estimated standard errors are added.
#'
#' @slot fit The fitted \code{\link[stats]{lm}} object.
#' @slot se The standard error of the treatment (levels) estimated using
#' \code{\link[sandwich]{vcovHC}}
.make_lm <- setClass("mmi_lm",
                     slots = c(fit = "lm", se = "numeric"),
                     contains = c("spec_lm", "mmi_model"))

#' S4 class for representing a linear mixed model specification and a fit to that specification.
#'
#' Inherits from \code{\linkS4class{spec_lmm}} and \code{\linkS4class{mmi_model}}.
#' Contains the \code{\linkS4class{lmerMod}} that us fitted according to the specification.
#' @slot fit The \code{\linkS4class{lmerMod}} fit to the contained model specification.
.make_lmm <- setClass("mmi_lmm",
                      slots = c(fit = "lmerMod"),
                      contains = c("spec_lmm", "mmi_model"))

#' S4 class for representing a logistic regression specification and a fit to that specification.
#'
#' Inherits from \code{\linkS4class{spec_logreg}} and \code{\linkS4class{mmi_model}}.
#' A \code{\link[stats]{glm}} binomial logit-link fit is added to the specification object.
#'
#' @slot fit The fitted \code{\link[stats]{glm}} object.
.make_logreg <- setClass("mmi_logreg",
                         slots = c(fit = "glm"),
                         contains = c("spec_logreg", "mmi_model"))

#' S4 class for representing a beta regression specification and a fit to that specification.
#'
#' Inherits from \code{\linkS4class{spec_beta}} and \code{\linkS4class{mmi_model}}.
#' A \code{\link[betareg]{betareg}} logit-lik fit is added to the specification object.
#'
#' @slot fit The fitted \code{\link[betareg]{betareg}} object.
.make_beta <- setClass("mmi_beta",
                       slots = c(fit = "betareg"),
                       contains = c("spec_beta", "mmi_model"))

#' S4 class for representing a negative binomial regression specification and a fit to that specification.
#'
#' Inherits from \code{\linkS4class{spec_nb}} and \code{\linkS4class{mmi_model}}.
#' A \code{\link[MASS]{glm.nb}} negative binomial fit is added to the specification object.
#'
#' @slot fit The fitted \code{\link[MASS]{glm.nb}} object.
.make_nb <- setClass("mmi_nb",
                     slots = c(fit = "negbin"),
                     contains = c("spec_nb", "mmi_model"))


# Fit all null model methods ----------------------------------------------------------
fit_all_nulls <- function(object, ...) UseMethod("fit_all_nulls")

fit_all_nulls.mmi_model <- function(object, REML = TRUE) {
  vars <- unique(object@var_labels[, "variable"])
  map(vars, ~ update(object@fit, paste(". ~ . -", .)))
}

fit_all_nulls.mmi_lm <- function(object, ...) {
  f_loc <- function(var) {
    fm <- update(as.formula(get_formula(object)), paste(". ~ . -", var))
    lm(fm, object@fit$model)
  }
  vars <- unique(object@var_labels[, "variable"])
  map(vars, f_loc)
}

fit_all_nulls.mmi_beta <- function(object, ...) {
  f_loc <- function(var) {
    fm <- update(as.formula(get_formula(object)), paste(". ~ . -", var))
    betareg(fm, object@fit$model)
  }
  vars <- unique(object@var_labels[, "variable"])
  map(vars, f_loc)
}

# fit_all_nulls.mmi_logreg <- function(object, ...) {
#   f_loc <- function(var) {
#     fm <- update(as.formula(get_formula(object)), paste(". ~ . -", var))
#     glm(fm, object@fit$model, family = "binomial")
#   }
#   vars <- unique(object@var_labels[, "variable"])
#   map(vars, f_loc)
# }

# Coef methods ----------------------------------------------------------------------
coef.mmi_model <- function(object) coef(object@fit)[object@var_labels[,"levels"]]
coef.mmi_lmm <- function(object) fixef(object@fit)[object@var_labels[,"levels"]]

# Variance component estimations ----------------------------------------------------
#' Extract estimate of variance component to the treatment random effects
#' @export
prop_var <- function(object) UseMethod("prop_var")

#' @export
prop_var.mmi_lmm <- function(object) {
  rands <- find_rands(object@str_treatment_fm)
  est <- as.data.frame(lme4::VarCorr(object@fit))[c("grp", "vcov")]
  res_var <- est$vcov[est$grp == "Residual"]
  est <- left_join(tibble(grp = rands), est, by = "grp")
  tibble(response = object@response,
         random_effects = rands,
         prop_var = 100 * est$vcov / c(est$vcov + res_var))
}






