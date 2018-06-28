#' @include test.R confidence.R
NULL

# Defines the family and spec_fam classes ------------------------------
#' An S4 class representing a family of specifications
#'
#' Basically a list of specification objects. Can hold any of the specification objects.
#' Its fate is to become to a fam object using make_fam(obj).
.make_spec_fam <- setClass("spec_fam", contains = "namedList")

#' S4 class "fam"
#'
#' List of family objects.
#'
#' Class that represents a complete analysis. It's a list of model objects
#' create to be a vehicle for looping and
#' then binding together information from the model objects in a map-reduce like
#' pattern. The object also defines a family in the multiple testing sense.
.make_fam <- setClass("fam", contains = "spec_fam")


#'Constructs a list of model specifications from formula strings.
#'
#'Main function to create analysis representations. \code{responses}, \code{treatments}
#'and \code{controls} should be vectors of formula strings.
#'Every combination of \code{response} and \code{treatment} formulas
#'will be a \code{spec} object in the list.
#'They will all share the properties in \code{controls} and \code{model}.
#'\code{trans} should be given if its known that a response variable in the database is
#'transformed and a backtransformation is wanted.
#'
#'@param responses Character vector of response formula strings.
#'@param treatments Character vector of treatment formula strings.
#'@param controls Control formula string that will be shared for all models.
#'@param model Distributional assumption.
#'@param trans The pretransformation of response variables.
#'@export
specify <- function(responses, treatments = NULL, controls = NULL,
                        model, trans = NULL) {
  spec_frame <- expand.grid(response = responses, str_treatment_fm = treatments,
                            stringsAsFactors = FALSE)
  spec_fam <- vector(mode = "list", length = nrow(spec_frame))
  model_ids <- make_ids(length(spec_fam))
  names(spec_fam) <- model_ids
  spec_fam <- .make_spec_fam(spec_fam)
  for (i in seq_len(nrow(spec_frame))) {
    if (is.null(trans)) trans <- find_trans(spec_frame[["response"]][i])
    spec_obj <- .make_spec(str_response_fm = spec_frame[["response"]][i],
                           str_treatment_fm = spec_frame[["str_treatment_fm"]][i],
                           str_control_fm = controls,
                           trans = trans,
                           id = model_ids[i])

    spec_fam[[i]] <- switch(model,
                            lm = .make_spec_lm(spec_obj),
                            lmm = .make_spec_lmm(spec_obj),
                            logreg = .make_spec_logreg(spec_obj),
                            negbin = .make_spec_nb(spec_obj),
                            betareg = .make_spec_beta(spec_obj))
  }
  spec_fam
}

#' Creates a \code{\linkS4class{spec_fam}} from a list where each element is a list with
#' elements response, treatment and control which all carries a string formula, and model
#' which states the distributional assumption, i.e. the model to fit.
#' @export
specify_with_list <- function(spec_list) {
  model_ids <- make_ids(length(spec_list))
  names(spec_list) <- model_ids
  for (i in seq_len(length(spec_list))) {
    spec <- spec_list[[i]]
    trans <- spec[["trans"]]
    control <- spec[["control"]]
    if (is.null(trans)) trans <- "identity"
    if (is.null(control)) control <- character(0)
    spec_obj <- .make_spec(str_response_fm = spec[["response"]],
                           str_treatment_fm = spec[["treatment"]],
                           str_control_fm = control,
                           trans = trans,
                           id = model_ids[i])
    spec_list[[i]] <- switch(spec[["model"]],
                             lm = .make_spec_lm(spec_obj),
                             lmm = .make_spec_lmm(spec_obj),
                             logreg = .make_spec_logreg(spec_obj),
                             beta = .make_spec_beta(spec_obj),
                             negbin = .make_spec_nb(spec_obj))
  }
  .make_spec_fam(spec_list)
}

# Constructor for family --------------------------------------------------
#' Constructs fam objects from a specification family.
#'
#' Main function to create fam objects, lists of model objects. When make_fam is called
#' models will be fit to the data in \code{study_frame} according to each specification.
#'
#' @param spec_fam A specification family object.
#' @param study_frame The data to be investigated. Must include all response, treatment and
#' control variables in the specification family.
#' @export
make_fam <- function(spec_fam, study_frame) {
  fam <- map(spec_fam, fit_model, study_frame = study_frame)
  .make_fam(fam)
}

# show and print methods ------------------------------------------------------------------------
#' S4 method for generic show for spec_fam objects
#' @param object The \code{\linkS4class{spec_fam}} object to be shown.
#' @export
setMethod("show", "spec_fam", function(object) {
  print(object)
})

#' S4 method for generic show for fam objects
#' @param object The \code{\linkS4class{fam}} object to be shown.
#' @export
setMethod("show", "fam", function(object) {
  print(object)
})

#' S3 method for generic print for spec_fam objects.
#' @param x The \code{\linkS4class{spec_fam}} object to be printed.
#' @param ... Not used currently.
#' @export
print.spec_fam <- function(x, ...) {
  tab <- table(purrr::map_chr(x, class))
  tab_str <- paste(paste0(tab, " ", names(tab), " objects"), collapse = ", ")
  cat(class(x), " object of length ", length(x), "\nContains: \n", tab_str)
}

# Concatenate methods ------------------------------------------------------------------
#' S3 method for generic c() for \code{\linkS4class{spec_fam}} objects.
#' @export
c.spec_fam <- function(...) {
  objs <- list(...)
  .Data <- unlist(map(objs, ~ .@.Data))
  names(.Data) <- unlist(map(objs, names))
  .make_spec_fam(.Data = .Data)
}

#' S3 method for generic c() for \code{\linkS4class{fam}} objects.
#' @export
c.fam <- function(...) {
  .make_fam(NextMethod())
}

# Subset methods ----------------------------------------------------------------------

#' S3 method for generic `[` for \code{\linkS4class{spec_fam}} objects.
#' @export
`[.spec_fam` <- function(x, idx, ...) {
  if (is.character(idx)) {
    if (!idx %in% names(x)) stop("The fam object doesn't have those names")
    new_sfam <- x@.Data[x@names %in% idx]
    names <- idx
  } else {
    new_sfam <- x@.Data[idx]
    names <- names(x)[idx]
  }
  .make_spec_fam(new_sfam, names = names)
}

#' S3 method for generic `[` for \code{\linkS4class{fam}} objects.
#' @export
`[.fam` <- function(x, i, ...) {
  .make_fam(NextMethod())
}

# Fam inference methods -------------------------------------------------------------

#' @describeIn confidence
#' Maps over the family, constructs a confidence tibble for each model object using the
#' corresponding S3 method, and the binds the information together in a tidy way.
#' @export
confidence.fam <- function(object, level = 0.95) {
  map_dfr(object, confidence, level = level)
}

#' @describeIn test
#' Maps over the family, performs a test for each model using the corresponding S3 method,
#' constructs a tibble with the p-value of the test included, and then binds the tibbles together.
#' Also computes the FDR-adjusted significance
#' level.
#' @export
test.fam <- function(object) {
  tib <- map_dfr(object, test)
  tib <- add_column(tib, FDR = stats::p.adjust(tib$p, "fdr"), .before = "model_id")
  tib
}

#' @describeIn lrt
#' Loops over all model elements in the family and does a lrt test for each; tidy tibbles with
#' this information will then be binded together. Also computes the FDR-adjusted significance
#' level.
#' @export
lrt.fam <- function(object) {
  tib <- map_dfr(object, lrt)
  tib <- add_column(tib, FDR = stats::p.adjust(tib$p, "fdr"), .before = "model_id")
  tib
}

#' @describeIn ft
#' Loops over all model elements in the family and does a ft test for each, if defined;
#' tidy tibbles with this information will then be binded together.
#' Also computes the FDR-adjusted significance level.
#' @export
ft.fam <- function(object) {
  tib <- map_dfr(object, ft)
  tib <- add_column(tib, FDR = stats::p.adjust(tib$p, "fdr"), .before = "model_id")
  tib
}

#' @describeIn wald
#' Loops over all model elements in the family and does a Wald test for each, if defined;
#' tidy tibbles with this information will then be binded together.
#' Also computes the FDR-adjusted significance level.
#' @export
wald.fam <- function(object) {
  tib <- map_dfr(object, wald)
  tib <- add_column(tib, FDR = stats::p.adjust(tib$p, "fdr"), .before = "model_id")
  tib
}

#' @describeIn prop_var
#' Loops over all model elements in the family and estimates the variance component for each
#' treatment random effect;
#' tidy tibbles with this information will then be binded together.
#' @export
#' @export
prop_var.fam <- function(object) {
  map_dfr(object, prop_var)
}
