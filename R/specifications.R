# Define spec classes -----------------------------------------------------
#'S4 class spec that represents a model specification.
#'
#'Contains a specification of variables that should be included for the fitting of one model.
#'Has 4 subclasses that has the same specification and also defines what model to use.
#'Currently the subclasses \code{\linkS4class{spec_lm}}, \code{\linkS4class{spec_lm}},
#'\code{\linkS4class{spec_logreg}} and \code{\linkS4class{spec_trans_lm}} are implemented for
#'linear models, linear mixed models, logistic regression models and
#'transformed linear models respectively.
#'
#'@slot response Response variable.
#'@slot treatment Treatment variable.
#'@slot controls Control variables.
#'@slot trans If the response is transformed in any way.
#'Will be used to back-transform estimates and confidence intervals.
.make_spec <- setClass("spec",
                 slots = c(response = "character",
                           treatment = "character",
                           controls = "character",
                           trans = "character"))

#'S4 class spec_lm for an lm model specification.
#'
#'See help for \code{\linkS4class{spec}}.
.make_spec_lm <- setClass("spec_lm", contains = "spec")

#'S4 class spec_lmm for a linear model with random effects specification.
#'
#'See help for \code{\linkS4class{spec}}.
.make_spec_lmm <- setClass("spec_lmm",
                     slots = c(rands = "character"),
                     contains = "spec")

#'S4 class spec_logreg for a logistic regression specification.
#'
#'See help for \code{\linkS4class{spec}}.
.make_spec_logreg <- setClass("spec_logreg", contains = "spec")

#'S4 class "spec_trans_lm_lm" for linear model with a transformed response.
#'
#'Using this specification will transformed the response variable.
#'For the slots see help for \code{\linkS4class{spec}}.
.make_spec_trans_lm <- setClass("spec_trans_lm", contains = "spec_lm")

#'S4 class "spec_trans_lmm" for linear mixed model with a transformed response.
#'
#'Using this specification will transformed the response variable.
#'For the slots see help for \code{\linkS4class{spec}}.
.make_spec_trans_lmm <- setClass("spec_trans_lmm", contains = "spec_lmm")

#'S4 class "spec_int_lm for a linear model where the treatment is an interaction term
.make_spec_int_lm <- setClass("spec_int_lm",
                              slots = "interacting_var",
                              contains = "spec_lm")

# Initialize methods ------------------------------------------------------------------
#' S4 initialize method for \code{\linkS4class{spec}}.
#'
#' Makes sure that the control variables
#' are not responses or treatments, and makes sure that the transformation is implemented.
#' @param .Object The \code{\linkS4class{spec}} object to be created.
#' @param ... Arguments from the constructor.
#' @param trans The specific argument \code{trans} from the constructor.
setMethod("initialize", "spec", function(.Object, ..., trans, treatment, response, controls) {
  .Object <- callNextMethod()
  if (length(.Object@trans) != 0) {
    if (!.Object@trans %in% c("identity", "log", "log10")) {
      stop("trans must be identity, log or log10")
    }
  }
  .Object@controls <- base::setdiff(.Object@controls,
                                    c(.Object@treatment, .Object@response))
  .Object
})

#' S4 initialize method for \code{\linkS4class{spec_lmm}}.
#'
#' Makes sure that the specification has random effects.
#' @param .Object The \code{\linkS4class{spec}} object to be created.
#' @param ... Arguments from the constructor.
setMethod("initialize", "spec_lmm", function(.Object, ...){
  .Object <- callNextMethod()
  if (length(.Object@rands) == 0) stop("lmm models must have a random effect")
  .Object
})

#' S4 initialize method for \code{\linkS4class{spec_logreg}}.
#'
#' Makes sure that the transformation for the logistic regression class is a log.
#' @param .Object The \code{\linkS4class{spec}} object to be created.
#' @param ... Arguments from the constructor.
setMethod("initialize", "spec_logreg", function(.Object, ...){
  .Object <- callNextMethod()
  .Object@trans <- "log"
  .Object
})

#' S4 initialize method for \code{\linkS4class{spec_trans_lm}}.
#'
#' Makes sure that the lm transformation class has a defined transformation
#' @param .Object The \code{\linkS4class{spec}} object to be created.
#' @param ... Arguments from the constructor.
setMethod("initialize", "spec_trans_lm", function(.Object, ...){
  .Object <- callNextMethod()
  if (length(.Object@trans) == 0) {
    stop("transformation specifications must have a specified transformation")
  }
  .Object
})

#' S4 initialize method for \code{\linkS4class{spec_trans_lmm}}.
#'
#' Makes sure that the lmm transformation class has a defined transformation
#' @param .Object The \code{\linkS4class{spec}} object to be created.
#' @param ... Arguments from the constructor.
setMethod("initialize", "spec_trans_lmm", function(.Object, ...){
  .Object <- callNextMethod()
  if (length(.Object@trans) == 0) {
    stop("transformation specifications must have a specified transformation")
  }
  .Object
})

# get_formula methods -----------------------------------------------------
#' Generic method that computes the formula for the fit.
#'
#' Methods for this generic are implemented for the different \code{\linkS4class{spec}}
#' classes.
#' @param object A \code{\linkS4class{spec}} object.
get_formula <- function(object) {
  UseMethod("get_formula")
}

get_formula.spec <- function(object) {
  covs <- c(object@treatment, object@controls)
  as.formula(paste0(object@response, " ~ ", paste(covs, collapse = " + ")))
}

get_formula.spec_int_lm <- function(object) {
  preds <- paste0(c(object@treatment, object@controls), collapse = " + ")
  fm <- paste0(object@response, " ~ ", preds)
  fm <- as.formula(fm)
}

get_formula.spec_lmm <- function(object) {
  rands <- paste0("(1|", object@rands, ")")
  covs <- c(object@treatment, object@controls, rands)
  as.formula(paste0(object@response, " ~ ", paste0(covs, collapse = " + ")))
}

#' Generic method that computes the formula without the treatment for the fit.
#'
#' Methods for this generic are implemented for the different \code{\linkS4class{spec}}
#' classes.
#' @param object A \code{\linkS4class{spec}} object.
get_null_formula <- function(object) {
  UseMethod("get_null_formula")
}

get_null_formula.spec <- function(object) {
  response <- paste0(object@response, " ~ ")
  covs <- one_if_no_preds(object@controls)
  as.formula(paste0(response, covs))
}

get_null_formula.spec_lmm <- function(object) {
  response <- paste0(object@response, " ~ ")
  covs <- c(object@controls, paste0("(1|", object@rands, ")"))
  covs <- paste0(covs, collapse = " + ")
  as.formula(paste0(response, covs))
}

get_null_formula.spec_int_lm <- function(object) {
  response <- paste0(object@response, " ~ ")
  int_vars <- interacting_vars(object@treatment)
  covs <- c(object@treatment, int_vars$left, int_vars$right)
  as.formula(paste0(response, paste0(covs, collapse = " + ")))
}

# get_trt_levels methods --------------------------------------------------------------
#' Generic method that computes the names given to effects from variables that are
#' factors/characters/logicals.
#'
#' Methods for this generic are implemented for the different \code{\linkS4class{spec}}
#' classes.
#' @param object A \code{\linkS4class{spec}} object.
#' @param fit The model fit that will be added to the \code{\linkS4class{spec}} in the subsequent
#' mmi model construction.
get_trt_levels <- function(object, fit) {
  UseMethod("get_trt_levels")
}

get_trt_levels.spec <- function(object, fit) {
  mf <- fit[["model"]]
  trt <- mf[[object@treatment]]
  if (!is.character(trt) & !is.logical(trt)) {
    paste0(object@treatment, fit[["xlevels"]][[object@treatment]][-1])
  } else {
    paste0(object@treatment, levels(factor(trt))[-1])
  }
}

get_trt_levels.spec_lmm <- function(object, fit) {
  if (!is_empty(object@treatment)) {
    mf <- fit@frame
    trt <- mf[[object@treatment]]
    if (!class(trt) %in% c("character", "logical", "factor")) {
      object@treatment
    } else if (is.factor(trt)) {
      paste0(object@treatment, levels(trt)[-1])
    } else {
      paste0(object@treatment, levels(factor(trt))[-1])
    }
  } else {
    character(0)
  }
}

get_trt_levels.spec_int_lm <- function(object, fit) {
  mf <- fit[["model"]]
  int_vars <- interacting_vars(object@treatment)
  left <- paste0(int_vars$left, levels(mf[[int_vars$left]])[-1])
  right <- paste0(int_vars$right, levels(mf[[int_vars$right]])[-1])
  paste0(left, ":", right)
}

# Constructors for the mmi_model objects ----------------------------------
#' S3 Generic function fit_model to construct mmi_model objects.
#'
#' Sets up the formula using \code{\link{get_formula}} and then fits an
#' an appropriate model using the data in \code{study_frame} and adds to the specifiation object
#' creating a mmi model object.
#' @param object \code{\linkS4class{spec}} object.
#' @param study_frame The data frame that includes all variables in all models
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
  fm <- get_formula(object)
  fit <- lm(fm, study_frame)
  trt_levels <- get_trt_levels(object, fit)
  se = est_sandwich_se(fit, trt_levels)
  .make_lm(object, fit = fit, trt_levels = trt_levels, se = se, formula = fm,
           null_formula = get_null_formula(object))
}

#'@describeIn fit_model
#' Constructs a \code{\linkS4class{mmi_logreg}} object by setting up a formula using \code{\link{get_formula}}
#' and then fitting a \code{\link[stats]{glm}} logistic regression using the formula and the data in study_frame.
#' @export
fit_model.spec_logreg <- function(object, study_frame) {
  fm <- get_formula(object)
  fit <- warn(glm(fm, study_frame, family = "binomial"), object, activity = "fitting of model")
  trt_levels <- get_trt_levels(object, fit)
  .make_logreg(object, fit = fit, trt_levels = trt_levels,
               formula = fm,
               null_formula = get_null_formula(object))
}

#'@describeIn fit_model
#' Constructs a \code{\linkS4class{mmi_lmm}} object by setting up a formula using \code{\link{get_formula}}
#' and then fitting a \code{\linkS4class{lmerMod}} using the formula and the data in study_frame.
#' @export
fit_model.spec_lmm <- function(object, study_frame) {
  fm <- get_formula(object)
  fit <- warn(lmer(fm, study_frame), object, "fitting of model")
  trt_levels <- get_trt_levels(object, fit)
  .make_lmm(object, fit = fit,
            trt_levels = trt_levels,
            formula = fm,
            null_formula = get_null_formula(object))
}

#'@describeIn fit_model
#' Constructs a \code{\linkS4class{mmi_trans_lm}} object by setting up a formula using \code{\link{get_formula}}
#' and then fitting a \code{\link[stats]{lm}} on the transformed response using the formula and the data in study_frame.
#' @export
fit_model.spec_trans_lm <- function(object, study_frame) {
  study_frame[[object@response]] <- match.fun(object@trans)(study_frame[[object@response]])
  .make_lm(NextMethod())
}

#'@describeIn fit_model
#' Constructs a \code{\linkS4class{mmi_trans_lmm}} object by setting up a formula using \code{\link{get_formula}}
#' and then fitting a \code{\linkS4class{lmerMod}} on the transformed response using the formula and the data in study_frame.
#' @export
fit_model.spec_trans_lmm <- function(object, study_frame) {
  study_frame[[object@response]] <- match.fun(object@trans)(study_frame[[object@response]])
  .make_lmm(NextMethod())
}

