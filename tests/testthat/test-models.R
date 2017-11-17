context("models")
set.seed(10)
iris <- cbind(iris, Bin = purrr::rbernoulli(nrow(iris), 0.5))
lm_obj <- .make_spec_lm(response = "Petal.Length", treatment = "Species",
                        controls = "Sepal.Length", trans = "identity")

trans_lm_obj <- .make_spec_trans_lm(response = "Petal.Length",
                                      treatment = "Sepal.Length",
                                      controls = "Species",
                                      trans = "log10")

logreg_obj <- .make_spec_logreg(response = "Bin",
                               treatment = "Sepal.Length",
                               trans = "identity")

lmm_obj <- .make_spec_lmm(response = "Petal.Length", treatment = "Sepal.Length",
                        controls = "Petal.Width", rands = "Species", trans = "identity")

trans_lmm_obj <- .make_spec_trans_lmm(response = "Petal.Length",
                                    treatment = "Sepal.Length",
                                    controls = "Petal.Width",
                                    rands = "Species",
                                    trans = "log10")
lmm_only_rands <- .make_spec_lmm(response = "Petal.Length", controls = "Petal.Width",
                                 rands = "Species", trans = "identity")

m_lm <- fit_model(lm_obj, iris)
m_lmm <- fit_model(lmm_obj, iris)
m_logreg <- fit_model(logreg_obj, iris)
m_trans_lm <- fit_model(trans_lm_obj, iris)
m_trans_lmm <- fit_model(trans_lmm_obj, iris)
m_only_rands <- fit_model(lmm_only_rands, iris)

test_that("confidence methods", {
  expect_s3_class(confidence(m_lm, 0.95), "tbl_df")
  expect_s3_class(confidence(m_lmm, 0.95), "tbl_df")
  expect_s3_class(confidence(m_logreg, 0.95), "tbl_df")
  expect_s3_class(confidence(m_trans_lm, 0.95), "tbl_df")
  expect_s3_class(confidence(m_trans_lmm, 0.95), "tbl_df")

  expect_equal(nrow(confidence(m_lm, 0.95)), 2)
  expect_equal(nrow(confidence(m_lmm, 0.95)), 1)
  expect_equal(nrow(confidence(m_trans_lm, 0.95)), 1)
  expect_equal(nrow(confidence(m_trans_lmm, 0.95)), 1)
  expect_equal(nrow(confidence(m_logreg, 0.95)), 1)
})


test_that("test methods", {
  expect_equal(ncol(test(m_lm)), 4)
  expect_equal(ncol(test(m_lmm)), 3)
  expect_equal(ncol(test(m_logreg)), 3)
  expect_equal(ncol(test(m_trans_lm)), 4)
  expect_equal(ncol(test(m_trans_lmm)), 3)

  expect_equal(nrow(test(m_lm)),2)
  expect_equal(nrow(test(m_lmm)), 1)
  expect_equal(nrow(test(m_logreg)), 1)
  expect_equal(nrow(test(m_trans_lm)), 1)
  expect_equal(nrow(test(m_trans_lmm)), 1)

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

  expect_equal(nrow(lrt(m_lm)),1)
  expect_equal(nrow(lrt(m_lmm)), 1)
  expect_equal(nrow(lrt(m_logreg)), 1)
  expect_equal(nrow(lrt(m_trans_lm)), 1)
  expect_equal(nrow(lrt(m_trans_lmm)), 1)
})

remove(iris)


