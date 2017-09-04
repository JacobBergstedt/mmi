test_package <- function() {
  load("/work/jacoba/TREC/Data/trec.data.RData")
  n <- 10
p <- specify(responses = G.facs[1:n], treatments = G.treatments[1:n], controls = "Age",
               model = "trans", trans = "log")
  o <- make_fam(p, mi)
  list(o = o, p = p)
}

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

inv <- function(trans) {
  switch(trans,
         identity = function(x) identity(x),
         log = function(x) exp(x),
         log10 = function(x) 10^x)
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

setup_confint_tib <- function(object, est, inv_trans, level) {
  confs <- warn(confint(object@fit, parm = object@trt_levels, level = level, quiet = TRUE), object)
  confs <- inv_trans(confs)
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

warn <- function(expr, object) {
  withCallingHandlers(expr, warning = function(w) {
    str <- c("Object of class", class(object), "with response",
             object@response, "and treatment", object@treatment,
             "was involved in warning: \n", conditionMessage(w), "\n")
    cat(str)
    invokeRestart("muffleWarning")
  })
}

warn_null <- function(expr, obj) {
  withCallingHandlers(expr, warning = function(w) {
    str <- c("During fitting of the null model, the object of class",
             class(object), "with response", object@response,
             "and treatment", object@treatment,
             "was involved in warning: \n", conditionMessage(w), "\n")
    cat(str)
    invokeRestart("muffleWarning")
  })
}
