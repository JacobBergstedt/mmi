default_control <- function() {
  lme4::lmerControl(optimizer = "nloptwrap",
                    optCtrl = list(maxeval = 1e4, xtol_abs = 1e-8, ftol_abs = 1e-8))
}

est_sandwich_se <- function(fit, var_labels) {
  v <- sandwich::vcovHC(fit)[var_labels, var_labels]
  if (is.matrix(v)) v <- diag(v)
  sqrt(v)
}

# get_slots <- function(fam, slot) {
#   unique(unlist(map(fam, slot)))
# }

find_response <- function(str) {
  str <- remove_spaces(str)
  ifelse(should_transform(str),
         str_extract(str, "(?<=\\().*(?=\\))"),
         str)
}

find_terms <- function(str) {
  terms <- remove_spaces(str)
  terms <- strsplit(terms, "+", fixed = TRUE)[[1]]
}

find_trans <- function(str) {
  str <- remove_spaces(str)
  if (should_transform(str)) {
    str_extract(str, ".*(?=\\()")
  } else "identity"
}

find_rands <- function(str) {
  terms <- remove_spaces(str)
  terms <- strsplit(terms, "+", fixed = TRUE)[[1]]
  out_terms <- vector("list", length(terms))
  for (i in seq_len(length(terms))) {
    out_terms[[i]] <- str_extract(terms[i], "(?<=\\|).+(?=\\))")
  }
  out_terms <- unlist(out_terms)
  out_terms[!is.na(out_terms)]
}

fit_all_nulls <- function(object, REML = TRUE) {
  vars <- unique(object@var_labels[, "variable"])
  null_list <- vector("list", length(vars))
  for (i in seq_len(length(vars))) {
    null_fm <- paste(vars[vars != vars[i]], collapse = " + ")
    if (!is_empty(object@str_controls_fm)) {
      null_fm <- paste(object@response, " ~ ", null_fm, " + ", object@str_controls_fm)}
    else null_fm <- paste(object@response, " ~ ", null_fm)
    null_list[[i]] <- fit_null(object, null_fm, REML = REML)
  }
  null_list
}

all_has_treatments <- function(fam) {
  if (is.null(unlist(map(fam, "str_treatment_fm")))) {
    stop("All model objects must have treatments for this function")
  }
}

interacting_levels <- function(str, model_fram) {
  left <- str_extract(str, ".*(?=\\*|:)")
  right <- str_extract(str, "(?<=\\*|:).*")
  left <- paste0(left, levels(model_fram[[left]])[-1])
  right <- paste0(right, levels(model_fram[[right]])[-1])
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

name_fam <- function(response, treatment, model) {
  str <- paste0(response, "_", treatment, "_", model)
  remove_spaces(str)
}

# one_if_no_preds <- function(covs) {
#   if (length(covs) > 0) paste0(covs, collapse = "+")
#   else 1
# }

p_lrt <- function(ll_null, ll_alt) {
  lr_stat <- 2 * (ll_alt - ll_null)
  df <- attr(ll_alt, "df") - attr(ll_null, "df")
  pchisq(c(lr_stat), df = df, lower.tail = FALSE)
}

remove_spaces <- function(str) str_replace_all(str, "[[:space:]]", "")

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
         variable = unique(object@var_labels[, "variable"]),
         test = test,
         p = p,
         model = str_extract(class(object), "(?<=mmi_).*"))
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


proto <- function(object, model_frame) {
  terms <- remove_spaces(object@str_treatment_fm)
  terms <- strsplit(terms, "+", fixed = TRUE)[[1]]
  out_terms <- vector(mode = "list", length = length(terms))
  for (i in seq_len(length(terms))) {
    if (str_detect(terms[i], "\\*")) {
      int_vars <- interacting_vars(terms[i])
      out_terms[[i]] <- c(int_vars$left, int_vars$right,
                          paste0(int_vars$left, ":", int_vars$right))
    }

    else if (str_detect(terms[i], "\\:")) {
      int_vars <- interacting_vars(terms[i])
      out_terms[[i]] <- paste0(int_vars$left, ":", int_vars$right)
    }

    else if (str_detect(terms[i], "I\\(.*\\)")) {
      out_terms[[i]] <- str_match(terms[i], "I\\(.*\\)")
    }

    else out_terms[[i]] <- paste0(terms[i])
  }
  unlist(out_terms)
}
