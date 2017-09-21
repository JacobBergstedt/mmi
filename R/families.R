#' @include models.R
NULL

# Defines the family and spec_fam classes ------------------------------
#' An S4 class representing a family of specifications
#'
#' Basically a list of specification objects. Can hold any of the specification objects.
#' It's fate is to become to a fam object using make_fam(obj).
#'
#' @slot responses All response variables for this family.
#' @slot treatments All treatment variables for this family.
#'
.make_spec_fam <- setClass("spec_fam",
                      slots = c(responses = "character",
                                treatments = "character"),
                      contains = "namedList")

#' S4 class "fam"
#'
#' List of family objects.
#'
#' Class that represents a list of model objects as a vehicle for looping and
#' then bind together information from the model objects, basically a map-reduce
#' pattern. The object also defines a family in the multiple testing sense.
.make_fam <- setClass("fam", contains = "spec_fam")

#'Constructs a list of model specifications from variable names.
#'
#'Every combination of response and treatment will be a \code{spec} object in
#'the list. They will all share the properties in \code{controls}, \code{model},
#'\code{trans} and (if the model is a mixed model) in \code{rands}. So all
#'models will have the same control variables for instance.
#'
#'@param responses Character vector of response variables.
#'@param treatments Character vector of treatment variables.
#'@param controls Character vector of control variables that will be shared for
#'all models.
#'@param model Which model should be fitted?
#'@param trans Are the response variables transformed?
#'@param rands Possible random effects shared by the response variables.
#'@export
specify <- function(responses, treatments = character(0), controls = character(0),
                    model = NULL, trans = NULL, rands = NULL) {
  if (!is.null(model)) {
    if (!model %in% c("lm", "lmm", "trans_lm", "trans_lmm", "logreg")) stop("Not a valid model name")
    if (model == "lm" & !is.null(rands)) stop("lm should not have random effects")
    if (model == "lmm" & is.null(rands)) stop("lmm must have random effects")
  }
  if (is.null(model)) {
    if (is.null(rands)) model = "lm"
    else model = "lmm"
  }
  if (is.null(trans)) {
    trans <- switch(model,
                    lm = "identity",
                    lmm = "identity",
                    logreg = "log",
                    trans_lm = stop("transformation specifications must have a specified transformation"),
                    trans_lmm = stop("transformation specifications must have a specified transformation"))
  }
  if (!is_empty(treatments)) {
    spec_frame <- expand.grid(response = responses, treatment = treatments,
                            stringsAsFactors = FALSE)
  } else {
    spec_frame <- data.frame(response = responses, stringsAsFactors = FALSE)
  }
  spec_fam <- vector(mode = "list", length = nrow(spec_frame))
  spec_fam <- .make_spec_fam(spec_fam, responses = responses, treatments = treatments)
  for (i in seq_len(nrow(spec_frame))) {
    spec_obj <- .make_spec(response = spec_frame[i, 1],
                           treatment = as.character(spec_frame[i, 2]),
                           controls = base::setdiff(controls,
                                                    c(spec_frame[i, 1], as.character(spec_frame[i, 2]))),
                           trans = trans)
    spec_fam[[i]] <- switch(model,
                            lm = .make_spec_lm(spec_obj),
                            lmm = .make_spec_lmm(spec_obj, rands = rands),
                            logreg = .make_spec_logreg(spec_obj),
                            trans_lm = .make_spec_trans_lm(spec_obj),
                            trans_lmm = .make_spec_trans_lmm(spec_obj, rands = rands))
  }
  names(spec_fam) <- name_fam(spec_frame)
  spec_fam
}


#' @export
specify_with_list <- function(spec_list) {
  len <- length(spec_list)
  responses <- character(len)
  treatments <- character(len)
  for (i in seq_len(length(spec_list))) {
    spec <- spec_list[[i]]
    trans <- spec[["trans"]]
    controls <- spec[["controls"]]
    if (is.null(trans)) trans <- "identity"
    if (is.null(controls)) controls <- character(0)
    responses[i] <- spec[["response"]]
    treatments[i] <- spec[["treatment"]]
    controls = base::setdiff(controls,
                             c(spec[["response"]], spec[["treatment"]]))
    spec_obj <- .make_spec(response = spec[["response"]],
                           treatment = spec[["treatment"]],
                           controls = controls,
                           trans = trans)
    spec_list[[i]] <- switch(spec[["model"]],
                             lm = .make_spec_lm(spec_obj),
                             lmm = .make_spec_lmm(spec_obj, rands = spec[["rands"]]),
                             logreg = .make_spec_logreg(spec_obj),
                             trans_lm = .make_spec_trans_lm(spec_obj),
                             trans_lmm = .make_spec_trans_lmm(spec_obj,
                                                              rands = spec[["rands"]]))
  }
  .make_spec_fam(spec_list, responses = unique(responses), treatments = unique(treatments))
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
  .make_fam(fam, responses = spec_fam@responses, treatments = spec_fam@treatments)
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
  cat(class(x), " object of length ", length(x), ". \nContains: \n", tab_str,", ", sep = "")
  cat(length(x@responses), " response variables, and ", length(x@treatments),
      " treatment variables", sep = "")
}

# Concatenate methods ------------------------------------------------------------------
#' @export
c.spec_fam <- function(...) {
  objs <- list(...)
  responses <- unique(unlist(map(objs, ~ .@responses)))
  treatments <- unique(unlist(map(objs, ~ .@treatments)))
  .Data <- unlist(map(objs, ~ .@.Data))
  names(.Data) <- unlist(map(objs, names))
  .make_spec_fam(.Data = .Data, responses = responses, treatments = treatments)
}

#' @export
c.fam <- function(...) {
  .make_fam(NextMethod())
}

# Subset methods ----------------------------------------------------------------------

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
  responses <- purrr::map_chr(new_sfam, ~ .@response)
  treatments <- purrr::map_chr(new_sfam, ~ .@treatment)
  .make_spec_fam(new_sfam, names = names, responses = responses, treatments = treatments)
}

#' @export
`[.fam` <- function(x, i, ...) {
  .make_fam(NextMethod())
}



# Fam inference methods -------------------------------------------------------------

#' @describeIn confidence
#' Maps over the family, constructs a confidence interval for each model object using the
#' corresponding S3 method, and the binds the information together in a tidy way.
#' @export
confidence.fam <- function(object, level = 0.95) {
    purrr::map_dfr(object, confidence, level = level)
}

#' @describeIn test
#' Maps over the family, performs a test for each model using the corresponding S3 method,
#' constructs a tibble with the p-value of the test included, and then binds the tibbles together.
#' @export
test.fam <- function(object) {
  all_has_treatments(object)
  tib <- purrr::map_dfr(object, test)
  tib$FDR <- stats::p.adjust(tib$p, "fdr")
  tib
}

#' @describeIn lrt
#' Loops over all model elements in the family and does a lrt test for each; tidy tibbles with
#' this information will then be binded together. Also computes the FDR-adjusted significance
#' level.
#' @export
lrt.fam <- function(object) {
  all_has_treatments(object)
  tib <- purrr::map_dfr(object, lrt)
  tib$FDR <- stats::p.adjust(tib$p, "fdr")
  tib
}

#' @export
ft.fam <- function(object) {
  all_has_treatments(object)
  tib <- purrr::map_dfr(object, ft)
  tib$FDR <- stats::p.adjust(tib$p, "fdr")
  tib
}

#' @export
prop_var.fam <- function(object) {
  purrr::map_dfr(object, prop_var)
}


# Inference methods for spec_fam -----------------------------------------------------
confidence.spec_fam <- function(object, level = 0.95, study_frame, nr_cores = 1) {
  fit_and_get_confs <- function(spec, level = 0.95, study_frame) {
    confidence(fit_model(spec, study_frame), level = 0.95)
  }
  if (nr_cores == 1) {
    purrr::map_dfr(object, fit_and_get_confs, level = level, study_frame = study_frame)
  } else {
    cl <- parallel::makePSOCKcluster(nr_cores)
    parallel::clusterExport(cl, c("level", "study_frame"), envir = environment())
    parallel::parLapply(cl, object, function(x) fit_and_get_confs(x, level = level,
                                                                  study_frame = study_frame))
    parallel::stopCluster(cl)
  }
}


# FCR function ----------------------------------------------------------------------


#' Calculates FCR adjusted confidence intervals
#'
#'Takes a \code{\linkS4class{fam}} and selects which of the fits to calculate confidence
#'intervals for based on the significance values of tests given in the tibble test_tib which
#'typically is an output of \code{\link{test}} or \code{\link{anova}}. If a response-treatment
#'pair is significant based on the crit and thresh parameters then confidence levels willl be
#'calculated for all treatment level parameters for that pair. The confidence intervals are
#'FCR-adjusted based on the number of parameters selected in total.
#' @param fam A \code{\linkS4class{fam}} object where models will be selected from.
#' @param test_tib A tibble with results from \code{\link{test}} or \code{\link{anova}} from
#' the fam object supplied with the first argument.
#' @param crit The criterion used for selection. Has to be a significance column in test_tib.
#' @param thresh The threshold for the criterion: select all parameters who had higher significance than this.
#' @param level The FCR confidence level.
#' @export
select_confidence <- function(fam, test_tib, crit = "FDR", thresh = 0.05, level = 0.95) {
  if (!class(fam) == "fam") stop("fam parameter must be a fam object")
  if (!crit %in% names(test_tib)) stop("crit not a name in test_tib")
  if (is.null(names(fam))) stop("This function currently only works for named families")
  ntot <- nrow(test_tib)
  test_tib <- test_tib[test_tib[[crit]] < thresh, c("response", "treatment")]
  nhits <- nrow(test_tib)
  selected_models <- name_fam(test_tib)
  fcr_alpha <- (1 - level) * nhits / ntot
  tib <- confidence(fam[names(fam) %in% selected_models], level = 1 - fcr_alpha)
  names(tib)[names(tib) == "lower"] <- "FCR_lower"
  names(tib)[names(tib) == "higher"] <- "FCR_higher"
  tib
}

