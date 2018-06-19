default_control <- function() {
  lme4::lmerControl(optimizer = "nloptwrap",
                    optCtrl = list(maxeval = 1e4, xtol_abs = 1e-8, ftol_abs = 1e-8))
}

est_sandwich_se <- function(fit, var_labels) {
  v <- sandwich::vcovHC(fit)[var_labels, var_labels]
  if (is.matrix(v)) v <- diag(v)
  sqrt(v)
}

get_slots <- function(fam, slot) {
  unique(unlist(map(fam, slot)))
}

find_response <- function(str_response_fm) {
  str_response_fm <- str_replace_all(str_response_fm, "[[:space:]]", "")
  if (should_transform(str_response_fm)) {
    str_extract(str_response_fm, "(?<=\\().*(?=\\))")
  } else str_response_fm

}

find_trans <- function(str_response_fm) {
  str_response_fm <- str_replace_all(str_response_fm, "[[:space:]]", "")
  if (should_transform(str_response_fm)) {
    str_extract(str_response_fm, ".*(?=\\()")
  } else "identity"
}

all_has_treatments <- function(fam) {
  if (is.null(unlist(map(fam, "str_treatment_fm")))) {
    stop("All model objects must have treatments for this function")
  }
}

interacting_vars <- function(str, mf) {
  left <- str_extract(str, ".*(?=\\*|:)")
  right <- str_extract(str, "(?<=\\*|:).*")
  left <- paste0(left, levels(mf[[left]])[-1])
  right <- paste0(right, levels(mf[[right]])[-1])
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

name_fam <- function(spec_frame, model) {
  paste0(spec_frame[["response"]], "_", spec_frame[["str_treatment_fm"]], "_", model)
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

setup_resid_tib <- function(object, res) {
  tib <- tibble(residuals = res,
         model = class(object),
         response = object@response,
         predictors = list(c(object@treatment, object@controls)),
         trans = object@trans,
         expected_norm = qnorm(ppoints(length(res)))[order(order(res))])

  tib <- bind_cols(tib, frame(object))
  names(tib)[names(tib) == object@response] <- "y"
  tib
}

setup_AIC_tib <- function(object, val) {
  tibble(AIC = val,
         model = class(object),
         response = object@response,
         predictors = list(c(object@treatment, object@controls)),
         trans = object@trans)
}

setup_lrt_tib <- function(object, p, test) {
  tibble(response = object@response,
         treatment = object@str_treatment_fm,
         test = test,
         p = p)
}

should_transform <- function(str_response_fm) {
  str_detect(str_response_fm, "(log|log10)\\([a-z, A-Z, 0-9, _, .]+\\)")
}

warn <- function(expr, object, activity) {
  withCallingHandlers(expr, warning = function(w) {
    str <- c("During", activity, "object of class", class(object), "with response",
             object@response, "and treatment formu", object@str_treatment_fm,
             "was involved in warning: \n", conditionMessage(w), "\n")
    cat(str)
    invokeRestart("muffleWarning")
  })
}
