default_control <- function() {
  lme4::lmerControl(optimizer = "nloptwrap",
                    optCtrl = list(maxeval = 1e4, xtol_abs = 1e-8, ftol_abs = 1e-8))
}

est_sandwich_se <- function(fit, var_labels) {
  v <- sandwich::vcovHC(fit)[var_labels, var_labels]
  if (is.matrix(v)) v <- diag(v)
  sqrt(v)
}

get_formula <- function(object) {
  if (!is_empty(object@str_control_fm)) {
    paste0(object@response, " ~ ", object@str_treatment_fm, " + ", object@str_control_fm)
  } else paste0(object@response, " ~ ", object@str_treatment_fm)
}

get_var_levels <- function(object, model_frame) {
  terms <- labels(terms(as.formula(paste("~", object@str_treatment_fm))))
  terms_list <- vector(mode = "list", length = length(terms))
  names(terms_list) <- terms

  for (i in seq_len(length(terms))) {
    if (str_detect(terms[i], "\\:")) {
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
         log2 = function(x) 2 ^ x
         log = function(x) exp(x),
         log10 = function(x) 10 ^ x)
}

is_empty <- function(x) {
  length(x) == 0
}


make_ids <- function(n) {
  paste0(format(Sys.time(), "%y.%m.%d.%H.%M.%OS6"), "_", seq_len(n))
}


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

setup_lrt_tib <- function(object, p, test) {
  tibble(response = object@response,
         variable = unique(object@var_labels[, "variable"]),
         test = test,
         p = p,
         model = str_extract(class(object), "(?<=mmi_).*"),
         model_id = object@id)
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
