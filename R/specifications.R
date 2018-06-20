#' @include data.R
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
                           str_response_fm = "character",
                           str_treatment_fm = "character",
                           str_controls_fm = "character",
                           trans = "character"))

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

.make_spec_nb <- setClass("spec_nb", contains = "spec")

.make_spec_beta <- setClass("spec_beta", contains = "spec")


# Initialize methods ------------------------------------------------------------------
#' S4 initialize method for \code{\linkS4class{spec}}.
#'
#' Makes sure that the control variables
#' are not responses or treatments, and makes sure that the transformation is implemented.
#' @param .Object The \code{\linkS4class{spec}} object to be created.
#' @param ... Arguments from the constructor.
#' @param trans The specific argument \code{trans} from the constructor.
setMethod("initialize", "spec", function(.Object, ...) {
  .Object <- callNextMethod()
  .Object@response <- find_response(.Object@str_response_fm)
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

#' S4 initialize method for \code{\linkS4class{spec_nb}}.
#'
#' Makes sure that the transformation for the logistic regression class is a log.
#' @param .Object The \code{\linkS4class{spec}} object to be created.
#' @param ... Arguments from the constructor.
setMethod("initialize", "spec_nb", function(.Object, ...){
  .Object <- callNextMethod()
  .Object@trans <- "log"
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
  if (!is_empty(object@str_controls_fm)) {
    paste0(object@response, " ~ ", object@str_treatment_fm, " + ", object@str_controls_fm)
  } else paste0(object@response, " ~ ", object@str_treatment_fm)
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
  paste0(object@response, " ~ ", object@str_controls_fm)
}


# get_var_levels methods --------------------------------------------------------------
#' Generic method that computes the names given to effects from variables that are
#' factors/characters/logicals.
#'
#' Methods for this generic are implemented for the different \code{\linkS4class{spec}}
#' classes.
#' @param object A \code{\linkS4class{spec}} object.
#' @param fit The model fit that will be added to the \code{\linkS4class{spec}} in the subsequent
#' mmi model construction.
get_var_levels <- function(object, fit) {
  UseMethod("get_var_levels")
}

get_var_levels.spec <- function(object, model_frame) {
  terms <- find_terms(object@str_treatment_fm)
  terms_list <- vector(mode = "list", length = length(terms))
  names(terms_list) <- terms

  for (i in seq_len(length(terms))) {
    if (str_detect(terms[i], "\\*")) {
      int_vars <- interacting_levels(terms[i], model_frame)
      terms_list[[i]] <- c(int_vars$left, int_vars$right,
                          paste(rep(int_vars$left, each = length(int_vars$right)),
                                int_vars$right,
                                sep = ":"))
    }

    else if (str_detect(terms[i], "\\:")) {
      int_vars <- interacting_levels(terms[i], model_frame)
      terms_list[[i]] <- paste(rep(int_vars$left, each = length(int_vars$right)),
                               int_vars$right,
                               sep = ":")
    }

    else if (str_detect(terms[i], "I\\(.*\\)")) {
      terms_list[[i]] <- str_extract(terms[i], "I\\(.*\\)")
    }

    else terms_list[[i]] <- paste0(terms[i], levels(model_frame[[terms[i]]])[-1])
  }

  do.call("rbind", map2(names(terms_list), terms_list, ~ cbind(variable = .x, levels = .y)))
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
  if (should_transform(object@str_response_fm)) {
    study_frame[[object@response]] <- match.fun(object@trans)(study_frame[[object@response]])
  }
  fm <- get_formula(object)
  fit <- lm(fm, study_frame)
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
  fit <- warn(lmer(fm, study_frame), object, "fitting of model")
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
  fit <- warn(glm(fm, study_frame, family = "binomial"), object, activity = "fitting of model")
  var_labels <- get_var_levels(object, fit$model)
  .make_logreg(object, fit = fit, var_labels = var_labels)
}

#' @export
fit_model.spec_nb <- function(object, study_frame) {
  fm <- get_formula(object)
  fit <- warn(glm.nb(fm, study_frame), object, activity = "fitting of model")
  var_labels <- get_var_levels(object, fit$model)
  .make_nb(object, fit = fit, var_labels = var_labels)
}

#'@describeIn fit_model
#' Constructs a \code{\linkS4class{mmi_logreg}} object by setting up a formula using \code{\link{get_formula}}
#' and then fitting a \code{\link[stats]{glm}} logistic regression using the formula and the data in study_frame.
#' @export
fit_model.spec_beta <- function(object, study_frame) {
  fm <- get_formula(object)
  fit <- warn(betareg(fm, study_frame), object, activity = "fitting of model")
  var_labels <- get_var_levels(object, fit$model)
  .make_beta(object,
             fit = fit,
             var_labels = var_labels)
}


# a <- 1:2; b <- stri_rand_strings(1e1, 2);
# microbenchmark::microbenchmark(paste(rep(a, each = length(b)), b, sep = ":"),
#                                as.vector(outer(a, b, paste, sep=":")),
#                                times = 1e3)







cbind(names(p)[1], p[[1]])




