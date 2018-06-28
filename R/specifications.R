#' @include data.R
# Define spec classes -----------------------------------------------------
#'S4 class spec that represents a model specification.
#'
#'Contains a model representation including the part of the mean function to control for
#'and the part to draw inference on. Each particular distribution assumption has its own
#'sub class.
#'
#'@slot response Response variable.
#'@slot treatment Treatment variable.
#'@slot controls Control variables.
#'@slot trans If the response is transformed in any way.
#'Will be used to back-transform estimates and confidence intervals.
.make_spec <- setClass("spec",
                       slots = c(response = "character",
                                 str_response_fm = "character",
                                 str_treatment_fm = "character",
                                 str_control_fm = "character",
                                 trans = "character",
                                 id = "character"))

#'S4 class spec_lm for an lm model specification.
#'
#'See help for \code{\linkS4class{spec}}.
.make_spec_lm <- setClass("spec_lm", contains = "spec")

#'S4 class spec_lmm for a linear model with random effects specification.
#'
#'See help for \code{\linkS4class{spec}}.
.make_spec_lmm <- setClass("spec_lmm",
                           contains = "spec")

#'S4 class spec_logreg for a logistic regression specification.
#'
#'See help for \code{\linkS4class{spec}}.
.make_spec_logreg <- setClass("spec_logreg", contains = "spec")

#'S4 class spec_nb for a negative binomial specification.
#'
#'See help for \code{\linkS4class{spec}}.
.make_spec_nb <- setClass("spec_nb", contains = "spec")

#'S4 class spec_nb for a beta regression specification.
#'
#'See help for \code{\linkS4class{spec}}.
.make_spec_beta <- setClass("spec_beta", contains = "spec")


# Initialize methods ------------------------------------------------------------------
#' S4 initialize method for \code{\linkS4class{spec}}.
#'
setMethod("initialize", "spec", function(.Object, ...) {
  .Object <- callNextMethod()
  .Object@response <- find_response(.Object@str_response_fm)
  .Object
})

#' S4 initialize method for \code{\linkS4class{spec_logreg}}.
#'
#' @param .Object The \code{\linkS4class{spec}} object to be created.
#' @param ... Arguments from the constructor.
setMethod("initialize", "spec_logreg", function(.Object, ...){
  .Object <- callNextMethod()
  .Object@trans <- "log"
  .Object
})

#' S4 initialize method for \code{\linkS4class{spec_nb}}.
#'
#' @param .Object The \code{\linkS4class{spec}} object to be created.
#' @param ... Arguments from the constructor.
setMethod("initialize", "spec_nb", function(.Object, ...){
  .Object <- callNextMethod()
  .Object@trans <- "log"
  .Object
})

# Constructors for the mmi_model objects ----------------------------------
#' S3 Generic function fit_model to construct mmi_model objects.
#'
#' Sets up the formula for the particular model using \code{\link{get_formula}} and then
#' fits an an appropriate model using the data in \code{study_frame}. An mmi_model object is
#' created by adding the model fit, and possibly additional needed information,
#' to the specification object.
#' @param object \code{\linkS4class{spec}} object.
#' @param study_frame The data frame that includes all variables needed for the analysis
#' @export
fit_model <- function(object, study_frame) {
  UseMethod("fit_model")
}

#'@describeIn fit_model
#' Constructs a \code{\linkS4class{mmi_lm}} object.
#'
#' Sets up the formula using \code{\link{get_formula}} and then fits an
#' \code{\link[stats]{lm}} using the formula and the data in study_frame.
#' @export
fit_model.spec_lm <- function(object, study_frame){
  # Try and see if it's better to use all.vars to copy the study_frame subset before further processing
  if (should_transform(object@str_response_fm)) {
    study_frame[[object@response]] <- match.fun(object@trans)(study_frame[[object@response]])
  }
  fm <- get_formula(object)
  fit <- lm(as.formula(fm), study_frame)
  var_labels <- get_var_levels(object, fit$model)
  se = est_sandwich_se(fit, var_labels[, "levels"])
  .make_lm(object, fit = fit, var_labels = var_labels, se = se)
}

#'@describeIn fit_model
#' Constructs a \code{\linkS4class{mmi_lmm}} object by setting up a formula using \code{\link{get_formula}}
#' and then fitting a \code{\linkS4class{lmerMod}} using the formula and the data in study_frame.
#' @export
fit_model.spec_lmm <- function(object, study_frame) {
  if (should_transform(object@str_response_fm)) {
    study_frame[[object@response]] <- match.fun(object@trans)(study_frame[[object@response]])
  }
  fm <- get_formula(object)
  fit <- warn(lmer(as.formula(fm), study_frame), object, "fitting of model")
  var_labels <- get_var_levels(object, fit@frame)
  .make_lmm(object, fit = fit,
            var_labels = var_labels)
}

#'@describeIn fit_model
#' Constructs a \code{\linkS4class{mmi_logreg}} object by setting up a formula using \code{\link{get_formula}}
#' and then fitting a \code{\link[stats]{glm}} logistic regression using the formula and the data in study_frame.
#' @export
fit_model.spec_logreg <- function(object, study_frame) {
  fm <- get_formula(object)
  fit <- warn(glm(as.formula(fm), study_frame, family = "binomial"), object, activity = "fitting of model")
  var_labels <- get_var_levels(object, fit$model)
  .make_logreg(object, fit = fit, var_labels = var_labels)
}

#'@describeIn fit_model
#' Constructs a \code{\linkS4class{mmi_nb}} object by setting up a formula using \code{\link{get_formula}}
#' and then fitting a \code{\link[MASS]{glm.nb}} logistic regression using the formula and the data in study_frame.
#' @export
fit_model.spec_nb <- function(object, study_frame) {
  fm <- get_formula(object)
  fit <- warn(glm.nb(as.formula(fm), study_frame), object, activity = "fitting of model")
  var_labels <- get_var_levels(object, fit$model)
  .make_nb(object, fit = fit, var_labels = var_labels)
}

#'@describeIn fit_model
#' Constructs a \code{\linkS4class{mmi_beta}} object by setting up a formula using \code{\link{get_formula}}
#' and then fitting a \code{\link{betareg}} beta regression model using the formula and the data in study_frame.
#' @export
fit_model.spec_beta <- function(object, study_frame) {
  fm <- get_formula(object)
  fit <- warn(betareg(as.formula(fm), study_frame), object, activity = "fitting of model")
  var_labels <- get_var_levels(object, fit$model)
  .make_beta(object,
             fit = fit,
             var_labels = var_labels)
}
