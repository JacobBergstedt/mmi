context("specifications")

lm_obj <- .make_spec_lm(response = "resp", treatment = "treat", controls = "contr",
                        trans = "identity")
trans_lm_obj <- .make_spec_trans_lm(response = "resp", treatment = "treat",
                                      controls = "contr", trans = "identity")

logreg_obj <- .make_spec_logreg(response = "resp", treatment = "treat",
                                controls = "contr", trans = "identity")

lmm_obj <- .make_spec_lmm(response = "resp", treatment = "treat", controls = "contr",
                          trans = "log", rands = "rand")

trans_lmm_obj <- .make_spec_trans_lmm(response = "resp", treatment = "treat",
                                    controls = "contr", trans = "identity", rands = "rand")

test_that("Are spec constructors are working as they should?", {
  expect_equal(lm_obj@trans, "identity")
  expect_error(.make_spec_lmm(response = "resp", treatment = "treat", controls = "contr",
                              trans = "log"), "lmm models must have a random effect")
  expect_error(.make_spec_trans_lmm(response = "resp", treatment = "treat",
                                    controls = "contr", rands = "rand"),
               "transformation specifications must have a specified transformation")
  expect_error(.make_spec_trans_lmm(response = "resp", treatment = "treat",
                                    controls = "contr", trans = "identity"),
               "lmm models must have a random effect")
  expect_equal(logreg_obj@trans, "log")
})

test_that("get_formula methods", {
  fm_lm <- get_formula(lm_obj)
  fm_lmm <- get_formula(lmm_obj)
  fm_logreg <- get_formula(logreg_obj)
  fm_trans_lm <- get_formula(trans_lm_obj)
  fm_trans_lmm <- get_formula(trans_lmm_obj)

  nfm_lm <- get_null_formula(lm_obj)
  nfm_lmm <- get_null_formula(lmm_obj)
  nfm_logreg <- get_null_formula(logreg_obj)
  nfm_trans_lm <- get_null_formula(trans_lm_obj)
  nfm_trans_lmm <- get_null_formula(trans_lmm_obj)

  expect_is(fm_lm, "formula")
  expect_is(fm_lmm, "formula")
  expect_is(fm_logreg, "formula")
  expect_is(fm_trans_lm, "formula")
  expect_is(fm_trans_lmm, "formula")
  expect_is(nfm_lm, "formula")
  expect_is(nfm_lmm, "formula")
  expect_is(nfm_logreg, "formula")
  expect_is(nfm_trans_lm, "formula")
  expect_is(nfm_trans_lmm, "formula")

  expect_equal(deparse(fm_lm), "resp ~ treat + contr")
  expect_equal(deparse(fm_lmm), "resp ~ treat + contr + (1 | rand)")
  expect_equal(deparse(fm_logreg), "resp ~ treat + contr")
  expect_equal(deparse(fm_trans_lm), "resp ~ treat + contr")
  expect_equal(deparse(fm_trans_lmm), "resp ~ treat + contr + (1 | rand)")

  expect_equal(deparse(nfm_lm), "resp ~ contr")
  expect_equal(deparse(nfm_lmm), "resp ~ contr + (1 | rand)")
  expect_equal(deparse(nfm_logreg), "resp ~ contr")
  expect_equal(deparse(nfm_trans_lm), "resp ~ contr")
  expect_equal(deparse(nfm_trans_lmm), "resp ~ contr + (1 | rand)")
})

mf <- data.frame(resp = c(1, 2 ,3, 5, 10, 100),
                 treat = factor(c(1, 1, 1, 2, 2, 3)),
                 contr = c(1, 10, 100, 10, 1, 10),
                 rand = c(1, 1, 2, 2, 3, 3))

mf_bin <- data.frame(resp = factor(c(1, 1 ,1, 0, 0, 0)),
                 treat = factor(c(1, 1, 1, 2, 2, 3)),
                 contr = c(1, 10, 100, 10, 1, 10),
                 rand = c(1, 1, 2, 2, 3, 3))

m_lm <- fit_model(lm_obj, mf)
m_lmm <- fit_model(lmm_obj, mf)
m_logreg <- fit_model(logreg_obj, mf_bin)
m_trans_lm <- fit_model(trans_lm_obj, mf)
m_trans_lmm <- fit_model(trans_lmm_obj, mf)

test_that("get_trt_levels methods", {
  expect_equal(get_trt_levels(lm_obj, m_lm@fit), c("treat2", "treat3"))
  expect_equal(get_trt_levels(lmm_obj, m_lmm@fit), c("treat2", "treat3"))
  expect_equal(get_trt_levels(logreg_obj, m_logreg@fit), c("treat2", "treat3"))
  expect_equal(get_trt_levels(trans_lm_obj, m_trans_lm@fit), c("treat2", "treat3"))
  expect_equal(get_trt_levels(trans_lmm_obj, m_trans_lmm@fit), c("treat2", "treat3"))
})

test_that("fit_model methods", {
  expect_is(m_lm@fit, "lm")
  expect_is(m_lmm@fit, "lmerMod")
  expect_is(m_logreg@fit, "glm")
  expect_is(m_trans_lm@fit, "lm")
  expect_is(m_trans_lmm@fit, "lmerMod")
})


