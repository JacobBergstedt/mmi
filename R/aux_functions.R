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

setup_compare_tib <- function(object, val) {
  tibble(val = val,
         model = class(object),
         response = object@response,
         treatment = object@treatment,
         controls = list(object@controls),
         trans = object@trans)
}

setup_lrt_tib <- function(obj, p, test) {
  tibble(response = obj@response,
         treatment = obj@treatment,
         test = test,
         p = p)
}

setup_test_tib <- function(obj, p, test) {
  tibble(response = obj@response,
         treatment = obj@treatment,
         treatment_levels = obj@trt_levels,
         test = test,
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


# # 1 ---------------------------------------------------------------------------------
#
# # Load packages and data ------------------------------------------------------------
# library(tidyverse)
# library(mmi)
#
# seros_430_db <- readRDS("~/SEROLOGIES/Data/RData/seros_430_db.rds")
# load("~/SEROLOGIES/Data/RData/globals.RData")
# controls_counts <- c("Sex", "Age", "CMV", "Smoking")
#
#
# # FACS 430 T8 -----------------------------------------------------------------------
# spec_log <- specify(responses = G_430_props_T,
#                     treatments = "CMV",
#                     controls = controls_counts, model = "trans_lm",
#                     trans = "log")
#
# spec_beta <- specify(responses = G_430_props_T,
#                      treatments = "CMV",
#                      controls = controls_counts, model = "beta")
#
# spec <- c(spec_log, spec_beta)
# fam <- make_fam(spec, seros_430_db)
# aic <- AIC(fam) %>% select(-controls, -treatment)
# aic %>% arrange(response, val) %>% group_by(response) %>%
#   summarize(o = diff(val), p = first(model))
#
# # FACS 430 NK -----------------------------------------------------------------------
# spec_log <- specify(responses = G_430_props_NK,
#                     treatments = "CMV",
#                     controls = controls_counts, model = "trans_lm",
#                     trans = "log")
#
# spec_beta <- specify(responses = G_430_props_NK,
#                      treatments = "CMV",
#                      controls = controls_counts, model = "beta")
#
# spec <- c(spec_log, spec_beta)
# fam <- make_fam(spec, seros_430_db)
# aic <- AIC(fam) %>% select(-controls, -treatment)
# aic %>% arrange(response, val) %>% group_by(response) %>%
#   summarize(o = diff(val), p = first(model))
#
#
#
# # FACS 430 ~ NK Residuals -----------------------------------------------------------
# res <- residuals(fam)
#
#
