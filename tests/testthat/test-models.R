context("models")

ecrf$PhysicalActivity <- 1 + ecrf$PhysicalActivity
lm_obj <- .make_spec_lm(response = "PhysicalActivity", treatment = "Age",
                        controls = "Sex", trans = "identity")

trans_lm_obj <- .make_spec_trans_lm(response = "PhysicalActivity",
                                      treatment = "Age",
                                      controls = "Sex",
                                      trans = "log10")

logreg_obj <- .make_spec_logreg(response = "CMVPositiveSerology",
                               treatment = "Age",
                               trans = "identity")

lmm_obj <- .make_spec_lmm(response = "PhysicalActivity", treatment = "Age",
                        controls = "Sex", rands = "Income", trans = "identity")

trans_lmm_obj <- .make_spec_lmm(response = "PhysicalActivity", treatment = "Age",
                          controls = "Sex", rands = "Income", trans = "log10")

lmm_only_rands <- .make_spec_lmm(response = "PhysicalActivity", controls = "Sex",
                          rands = "Income")

m_lm <- fit_model(lm_obj, ecrf)
m_lmm <- fit_model(lmm_obj, ecrf)
m_logreg <- fit_model(logreg_obj, ecrf)
m_trans_lm <- fit_model(trans_lm_obj, ecrf)
m_trans_lmm <- fit_model(trans_lmm_obj, ecrf)
m_only_rands <- fit_model(lmm_only_rands, ecrf)

test_that("confidence methods", {
  expect_equal_to_reference(confidence(m_lm, level = 0.95), "confidence_lm.rds")
  expect_equal_to_reference(confidence(m_lmm, level = 0.95), "confidence_lmm.rds")
  expect_equal_to_reference(confidence(m_logreg, level = 0.95), "confidence_logreg.rds")
  expect_equal_to_reference(confidence(m_trans_lm, level = 0.95), "confidence_trans_lm.rds")
  expect_equal_to_reference(confidence(m_trans_lmm, level = 0.95), "confidence_trans_lmm.rds")
})


test_that("test methods", {
  expect_equal_to_reference(test(m_lm), "test_lm.rds")
  expect_equal_to_reference(test(m_lmm), "test_lmm.rds")
  expect_equal_to_reference(test(m_logreg), "test_logreg.rds")
  expect_equal_to_reference(test(m_trans_lm), "test_trans_lm.rds")
  expect_equal_to_reference(test(m_trans_lmm), "test_trans_lmm.rds")
})

test_that("lrt methods", {
  expect_equal_to_reference(lrt(m_lm), "lrt_lm.rds")
  expect_equal_to_reference(lrt(m_lmm), "lrt_lmm.rds")
  expect_equal_to_reference(lrt(m_logreg), "lrt_logreg.rds")
  expect_equal_to_reference(lrt(m_trans_lm), "lrt_trans_lm.rds")
  expect_equal_to_reference(lrt(m_trans_lmm), "lrt_trans_lmm.rds")
})

test_that("ft methods", {
  expect_equal_to_reference(ft(m_lm), "ft_lm.rds")
  expect_equal_to_reference(ft(m_lmm), "ft_lmm.rds")
  expect_equal_to_reference(ft(m_trans_lm), "ft_trans_lm.rds")
  expect_equal_to_reference(ft(m_trans_lmm), "ft_trans_lmm.rds")
})

test_that("Wald methods", {
  expect_equal_to_reference(wald(m_lm), "wald_lm.rds")
  expect_equal_to_reference(wald(m_trans_lm), "wald_trans_lm.rds")
})


