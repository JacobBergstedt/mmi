#' @include test.R confidence.R AIC.R residuals.R
NULL

# Defines the family and spec_fam classes ------------------------------
#' An S4 class representing a family of specifications
#'
#' Basically a list of specification objects. Can hold any of the specification objects.
#' It's fate is to become to a fam object using make_fam(obj).
#'
#'
.make_spec_fam <- setClass("spec_fam",
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
# specify <- function(responses, treatments = character(0), controls = character(0),
#                     model = NULL, trans = NULL, rands = NULL) {
#   if (!is.null(model)) {
#     mvec <- c("lm", "lmm", "logreg", "beta", "negbin")
#   }
#   if (!is_empty(treatments)) {
#     spec_frame <- expand.grid(response = responses, treatment = treatments,
#                               stringsAsFactors = FALSE)
#   } else {
#     spec_frame <- data.frame(response = responses, stringsAsFactors = FALSE)
#   }
#   spec_fam <- vector(mode = "list", length = nrow(spec_frame))
#   spec_fam <- .make_spec_fam(spec_fam)
#   for (i in seq_len(nrow(spec_frame))) {
#     spec_obj <- .make_spec(str_response_fm = spec_frame[i, 1],
#                            str_treatment_fm = spec_frame[i, 2],
#                            str_controls_fm = controls,
#                            trans = trans)
#     spec_fam[[i]] <- switch(model,
#                             lm = .make_spec_lm(spec_obj),
#                             lmm = .make_spec_lmm(spec_obj, rands = rands),
#                             logreg = .make_spec_logreg(spec_obj),
#                             negbin = .make_spec_nb(spec_obj),
#                             beta = .make_spec_beta(spec_obj),
#                             trans_lm = .make_spec_trans_lm(spec_obj),
#                             trans_lmm = .make_spec_trans_lmm(spec_obj, rands = rands),
#                             int_lm = .make_spec_int_lm(spec_obj))
#   }
#   names(spec_fam) <- name_fam(spec_frame)
#   spec_fam
# }

#' @export
specify_new <- function(responses, treatments = character(0), controls = character(0),
                        model = NULL) {
  spec_frame <- expand.grid(response = responses, treatment = treatments,
                            stringsAsFactors = FALSE)
  spec_fam <- vector(mode = "list", length = nrow(spec_frame))
  spec_fam <- .make_spec_fam(spec_fam)
  for (i in seq_len(nrow(spec_frame))) {
    spec_obj <- .make_spec(str_response_fm = spec_frame[i, 1],
                           str_treatment_fm = spec_frame[i, 2],
                           str_controls_fm = controls)

    spec_fam[[i]] <- switch(model,
                            lm = .make_spec_lm(spec_obj),
                            lmm = .make_spec_lmm(spec_obj),
                            logreg = .make_spec_logreg(spec_obj),
                            negbin = .make_spec_nb(spec_obj),
                            beta = .make_spec_beta(spec_obj))
  }
  spec_fam
}

#' @export
# specify_with_list <- function(spec_list) {
#   len <- length(spec_list)
#   responses <- character(len)
#   treatments <- character(len)
#   for (i in seq_len(length(spec_list))) {
#     spec <- spec_list[[i]]
#     trans <- spec[["trans"]]
#     controls <- spec[["controls"]]
#     if (is.null(trans)) trans <- "identity"
#     if (is.null(controls)) controls <- character(0)
#     responses[i] <- spec[["response"]]
#     treatments[i] <- spec[["treatment"]]
#     controls = base::setdiff(controls,
#                              c(spec[["response"]], spec[["treatment"]]))
#     spec_obj <- .make_spec(response = spec[["response"]],
#                            treatment = spec[["treatment"]],
#                            controls = controls,
#                            trans = trans)
#     spec_list[[i]] <- switch(spec[["model"]],
#                              lm = .make_spec_lm(spec_obj),
#                              lmm = .make_spec_lmm(spec_obj, rands = spec[["rands"]]),
#                              logreg = .make_spec_logreg(spec_obj),
#                              beta = .make_spec_beta(spec_obj),
#                              trans_lm = .make_spec_trans_lm(spec_obj),
#                              trans_lmm = .make_spec_trans_lmm(spec_obj,
#                                                               rands = spec[["rands"]]))
#   }
#   .make_spec_fam(spec_list, responses = unique(responses), treatments = unique(treatments))
# }

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
  cat(class(x), " object of length ", length(x), ". \nContains: \n", tab_str,", ", sep = "")
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
  map_dfr(object, confidence, level = level)
}

#' @describeIn test
#' Maps over the family, performs a test for each model using the corresponding S3 method,
#' constructs a tibble with the p-value of the test included, and then binds the tibbles together.
#' @export
test.fam <- function(object) {
  all_has_treatments(object)
  tib <- map_dfr(object, test)
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
  tib <- map_dfr(object, lrt)
  tib$FDR <- stats::p.adjust(tib$p, "fdr")
  tib
}

#' @export
ft.fam <- function(object) {
  all_has_treatments(object)
  tib <- map_dfr(object, ft)
  tib$FDR <- stats::p.adjust(tib$p, "fdr")
  tib
}

#' @export
wald.fam <- function(object) {
  all_has_treatments(object)
  tib <- map_dfr(object, wald)
  tib$FDR <- stats::p.adjust(tib$p, "fdr")
  tib
}

#' @export
prop_var.fam <- function(object) {
  map_dfr(object, prop_var)
}

#' @export
do_fam_inference <- function(spec, data, conf_level = 0.01, selection_thresh = 0.01,
                             selection_conf_level = 0.99) {
  fam <- make_fam(spec, data)
  hyp <- test(fam)
  confs <- confidence(fam, level = 0.99)
  selected_confs <- select_confidence(fam, hyp, thresh = 0.01, level = 0.99)
  list(hyp = hyp, confs = confs, selected_confs = selected_confs)
}


# Fam comparison methods ------------------------------------------------------------
#' @export
residuals.fam <- function(object) {
  map_dfr(object, residuals)
}

#' @export
AIC.fam <- function(object) {
  map_dfr(object, AIC)
}

# Inference methods for spec_fam -----------------------------------------------------
#' @export
confidence.spec_fam <- function(object, level = 0.95, study_frame, nr_cores = 1) {
  fit_and_get_confs <- function(spec, level = 0.95, study_frame) {
    confidence(fit_model(spec, study_frame), level = level)
  }
  if (nr_cores == 1) {
    map_dfr(object, fit_and_get_confs, level = level, study_frame = study_frame)
  } else {
    cl <- parallel::makePSOCKcluster(nr_cores)
    parallel::clusterExport(cl, c("level", "study_frame"), envir = environment())
    tib <- parallel::parLapply(cl, object, function(x) {
      fit_and_get_confs(x, level = level, study_frame = study_frame)
    })
    parallel::stopCluster(cl)
    dplyr::bind_rows(tib)
  }
}

#' @export
test.spec_fam <- function(object, study_frame, nr_cores = 1) {
  all_has_treatments(object)
  fit_and_get_tests <- function(spec, study_frame) {
    test(fit_model(spec, study_frame))
  }
  if (nr_cores == 1) {
    tib <- map_dfr(object, fit_and_get_tests, study_frame = study_frame)
  } else {
    cl <- parallel::makePSOCKcluster(nr_cores)
    parallel::clusterExport(cl, c("study_frame"), envir = environment())
    tib <- parallel::parLapply(cl, object, function(x) {
      fit_and_get_tests(x, study_frame = study_frame)
    })
    parallel::stopCluster(cl)
    tib <- dplyr::bind_rows(tib)
  }
  tib$FDR <- stats::p.adjust(tib$p, "fdr")
  tib
}

#' @export
inference <- function(object, study_frame, level, ...) {UseMethod("inference")}

#' @export
inference.spec_fam <- function(object, study_frame, level, nr_cores = 1) {
  all_has_treatments(object)
  fit_and_get_inference_results <- function(spec, study_frame, level) {
    m <- fit_model(spec, study_frame)
    left_join(confidence(m, level = level), test(m), by = c("response", "treatment"))
  }
  if (nr_cores == 1) {
    tib <- map_dfr(object, fit_and_get_inference_results, level = level,
                   study_frame = study_frame)
    tib$FDR <- p.adjust(tib$p, "fdr")
    tib
    } else {
    cl <- parallel::makePSOCKcluster(nr_cores)
    parallel::clusterExport(cl, c("study_frame", "level", "left_join"),
                            envir = environment())
    tib <- parallel::parLapply(cl, object, function(x) {
      fit_and_get_inference_results(x, study_frame = study_frame, level = level)
    })
    parallel::stopCluster(cl)
    tib <- dplyr::bind_rows(tib)
    tib$FDR <- p.adjust(tib$p, "fdr")
    tib
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
