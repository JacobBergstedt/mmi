# test_package <- function(nr_cores) {
#   library(tidyverse)
#   library(lme4)
#   library(mmi)
#   library(parallel)
#   seros_1000_db <- readRDS("~/SEROLOGIES/Data/RData/seros_1000_db.rds")
#   load("~/SEROLOGIES/Data/RData/globals.RData")
#   controls_counts <- c("HourOfSampling", "Sex", "Age", "CMV", "Smoking")
#
#
#   spec <- specify(responses = G_facs4ser, treatments = G_serolevels,
#                   controls = controls_counts, trans = "log",
#                   model = "lmm", rands = "DayOfSampling")
#
#   # spec <- c(spec,
#   #           specify(responses = G_1000_MFI, treatments = G_serostatus,
#   #                   controls = controls_mfis, trans = "log",
#   #                   model = "lmm", rands = "DayOfSampling"))
#   do_fam_inference(spec, seros_1000_db)
# }

default_control <- function() {
  lme4::lmerControl(optimizer = "nloptwrap",
                    optCtrl = list(maxeval = 1e4, xtol_abs = 1e-8, ftol_abs = 1e-8))
}

est_sandwich_se <- function(fit, trt_levels) {
  v <- sandwich::vcovHC(fit)[trt_levels, trt_levels]
  if (is.matrix(v)) v <- diag(v)
  sqrt(v)
}

get_slots <- function(fam, slot) {
  unique(unlist(map(fam, slot)))
}

all_has_treatments <- function(fam) {
  if (is.null(unlist(map(fam, "treatment")))) {
    stop("All model objects must have treatments for this function")
  }
}

interacting_vars <- function(str) {
  left <- str_extract(str, "[:graph:]+(?= ?\\* ?)")
  right <- str_extract(str, "(?<= ?\\* ?)[:graph:]+")
  list(left = left, right = right)
}

inv <- function(trans) {
  switch(trans,
         identity = function(x) identity(x),
         log = function(x) exp(x),
         log10 = function(x) 10^x)
}

is_empty <- function(x) {
  length(x) == 0
}

name_fam <- function(spec_frame) {
  paste0(spec_frame[["response"]], "_", spec_frame[["treatment"]])
}

one_if_no_preds <- function(covs) {
  if (length(covs) > 0) paste0(covs, collapse = "+")
  else 1
}

p_lrt <- function(ll_null, ll_alt) {
  lr_stat <- 2 * (ll_alt - ll_null)
  df <- attr(ll_alt, "df") - attr(ll_null, "df")
  pchisq(c(lr_stat), df = df, lower.tail = FALSE)
}

setup_lrt_tib <- function(obj, p) {
  tibble(response = obj@response,
         treatment = obj@treatment,
         p = p)
}

simplify_col_sel <- function(mat) {
  if (is.vector(mat)) {
    lower <- mat[1]
    higher <- mat[2]
  } else {
    lower <- mat[, 1]
    higher <- mat[, 2]
  }
  list(lower = lower, higher = higher)
}

setup_confint_tib <- function(object, est, inv_trans, parm, level) {
  confs <- warn(confint(object@fit, parm = parm, level = level, quiet = TRUE), object)
  confs[object@trt_levels,] <- inv_trans(confs[object@trt_levels, ])
  if (is.vector(confs)) {
    lower <- confs[1]
    higher <- confs[2]
  } else {
    lower <- confs[, 1]
    higher <- confs[, 2]
  }
  tibble(response = object@response,
         treatment = object@treatment,
         treatment_levels = object@trt_levels,
         est = inv_trans(est),
         lower = lower,
         higher = higher)
}

setup_test_tib <- function(obj, p) {
  tibble(response = obj@response,
         treatment = obj@treatment,
         treatment_levels = obj@trt_levels,
         p = p)
}

warn <- function(expr, object, activity) {
  withCallingHandlers(expr, warning = function(w) {
    str <- c("During", activity, "Object of class", class(object), "with response",
             object@response, "and treatment", object@treatment,
             "was involved in warning: \n", conditionMessage(w), "\n")
    cat(str)
    invokeRestart("muffleWarning")
  })
}
