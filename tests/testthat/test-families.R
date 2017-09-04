context("families")
set.seed(10)
iris <- cbind(iris, Bin = purrr::rbernoulli(nrow(iris), 0.5))
sfam_lm <- specify(responses = names(iris)[1], treatments = names(iris)[-1], model = "lm")
sfam_trans_lm <- specify(responses = names(iris)[1], treatments = names(iris)[-1],
                         model = "trans_lm", trans = "log10")
sfam_logreg <- specify(responses = "Bin", treatments = names(iris)[-ncol(iris)], model = "logreg")
sfam_lmm <- specify(responses = "Sepal.Length",
                   treatments = c("Sepal.Width", "Petal.Length", "Bin"),
                   rands = "Species",
                   model = "lmm")
sfam_trans_lmm <- specify(responses = "Sepal.Length",
                    treatments = c("Sepal.Width", "Petal.Length", "Bin"),
                    rands = "Species",
                    model = "lmm",
                    trans = "log10")

sfam_trans_lmm_from_list <- list(list(response = "Sepal.Length", treatment = "Sepal.Width",
                                      rands = "Species", model = "lmm", trans = "log10"),
                                 list(response = "Sepal.Length", treatment = "Petal.Length",
                                      rands = "Species", model = "lmm", trans = "log10"),
                                 list(response = "Sepal.Length", treatment = "Bin",
                                      rands = "Species", model = "lmm", trans = "log10"))
names(sfam_trans_lmm_from_list) <- paste0(c("Sepal.Length"), "_", c("Sepal.Width", "Petal.Length", "Bin"))
sfam_trans_lmm_from_list <- specify_with_list(sfam_trans_lmm_from_list)

test_that("Spec families are created correctly", {
  expect_s4_class(sfam_lm, "spec_fam")
  expect_s4_class(sfam_trans_lm, "spec_fam")
  expect_s4_class(sfam_logreg, "spec_fam")
  expect_s4_class(sfam_lmm, "spec_fam")
  expect_s4_class(sfam_trans_lmm, "spec_fam")
  expect_s4_class(sfam_trans_lmm_from_list, "spec_fam")

  testthat::expect_equal_to_reference(sfam_lm, "sfam_lm.rds")
  testthat::expect_equal_to_reference(sfam_trans_lm, "sfam_trans_lm.rds")
  testthat::expect_equal_to_reference(sfam_trans_lmm, "sfam_trans_lmm.rds")
  testthat::expect_equal_to_reference(sfam_logreg, "sfam_logreg.rds")
  testthat::expect_equal_to_reference(sfam_lmm, "sfam_lmm.rds")
  testthat::expect_equal_to_reference(sfam_trans_lmm_from_list, "sfam_trans_lmm_from_list.rds")
})

test_that("Spec subset operators are working", {
  expect_s4_class(sfam_lm[1], "spec_fam")
  expect_s4_class(sfam_lmm[1], "spec_fam")
  expect_s4_class(sfam_logreg[1], "spec_fam")
  expect_s4_class(sfam_trans_lm[1], "spec_fam")
  expect_s4_class(sfam_trans_lmm[1], "spec_fam")

  expect_equal(sfam_lm[1:2]@treatments, c("Sepal.Width", "Petal.Length"))
})


fam_lm <- make_fam(sfam_lm, iris)
fam_trans_lm <- make_fam(sfam_trans_lm, iris)
fam_logreg <- make_fam(sfam_logreg, iris)
fam_lmm <- make_fam(sfam_lmm, iris)
fam_trans_lmm <- make_fam(sfam_trans_lmm, iris)

test_that("Families are created correctly", {
  expect_s4_class(fam_lm, "fam")
  expect_s4_class(fam_trans_lm, "fam")
  expect_s4_class(fam_logreg, "fam")
  expect_s4_class(fam_lmm, "fam")
  expect_s4_class(fam_trans_lmm, "fam")

  expect_s3_class(fam_lm[[1]]@fit, "lm")
  expect_s3_class(fam_trans_lm[[1]]@fit, "lm")
  expect_s3_class(fam_logreg[[1]]@fit, "glm")
  expect_s4_class(fam_lmm[[1]]@fit, "lmerMod")
  expect_s4_class(fam_trans_lmm[[1]]@fit, "lmerMod")

  testthat::expect_equal_to_reference(fam_lm, "fam_lm.rds")
  testthat::expect_equal_to_reference(fam_trans_lm, "fam_trans_lm.rds")
  testthat::expect_equal_to_reference(fam_logreg, "fam_logreg.rds")
})

test_that("Methods on families are OK", {
  expect_s3_class(lrt(fam_lm), "tbl_df")
  expect_s3_class(lrt(fam_trans_lm), "tbl_df")
  expect_s3_class(lrt(fam_logreg), "tbl_df")
  expect_s3_class(lrt(fam_lmm), "tbl_df")
  expect_s3_class(lrt(fam_trans_lmm), "tbl_df")

  expect_s3_class(test(fam_lm), "tbl_df")
  expect_s3_class(test(fam_trans_lm), "tbl_df")
  expect_s3_class(test(fam_logreg), "tbl_df")
  expect_s3_class(test(fam_lmm), "tbl_df")
  expect_s3_class(test(fam_trans_lmm), "tbl_df")

  expect_s3_class(confidence(fam_lm), "tbl_df")
  expect_s3_class(confidence(fam_trans_lm), "tbl_df")
  expect_s3_class(confidence(fam_logreg), "tbl_df")
  expect_s3_class(confidence(fam_lmm), "tbl_df")
  expect_s3_class(confidence(fam_trans_lmm), "tbl_df")
})

remove(iris)
