context("families")
db <- dplyr::left_join(facs, ecrf)

sfam_lm <- specify(responses = "N_CD8bpos_EMRA.panel1", treatments = "Smoking",
                   controls = c("Age", "Sex"))

sfam_trans_lm <- specify(responses = "N_CD8bpos_EMRA.panel1", treatments = "Smoking",
                   controls = c("Age", "Sex"), trans = "log")

sfam_logreg <- specify(responses = "CMVPositiveSerology",
                       treatments = "Smoking",
                       controls = c("Age", "Sex"),
                       model = "logreg")

sfam_lmm <- specify(responses = "N_CD8bpos_EMRA.panel1", treatments = "Smoking",
                   controls = c("Age", "Sex"),
                   rands = "DayOfSampling")

sfam_trans_lmm <- specify(responses = "N_CD8bpos_EMRA.panel1", treatments = "Smoking",
                    controls = c("Age", "Sex"),
                    trans = "log",
                    rands = "DayOfSampling")

sfam_only_rands <- specify(responses = "N_CD8bpos_EMRA.panel1",
                    controls = c("Age", "Sex"),
                    trans = "log",
                    rands = "DayOfSampling")

sfam_trans_lmm_from_list <- list(list(response = "N_CD8bpos_EMRA.panel1", treatment = "Age",
                                      rands = "DayOfSampling", model = "lmm", trans = "log"),
                                 list(response = "N_CD8bpos_EMRA.panel1", treatment = "CMVPositiveSerology",
                                      controls = "Age", rands = "DayOfSampling",
                                      model = "lmm", trans = "log"))
names(sfam_trans_lmm_from_list) <- paste0(c("N_CD8bpos_EMRA.panel1"), "_", c("Age", "CMVPositiveSerology"))
sfam_trans_lmm_from_list <- specify_with_list(sfam_trans_lmm_from_list)


test_that("Spec families are created correctly", {
  expect_s4_class(sfam_lm, "spec_fam")
  expect_s4_class(sfam_trans_lm, "spec_fam")
  expect_s4_class(sfam_logreg, "spec_fam")
  expect_s4_class(sfam_lmm, "spec_fam")
  expect_s4_class(sfam_trans_lmm, "spec_fam")
  expect_s4_class(sfam_trans_lmm_from_list, "spec_fam")

  expect_equal_to_reference(sfam_lm, "sfam_lm.rds")
  expect_equal_to_reference(sfam_trans_lm, "sfam_trans_lm.rds")
  expect_equal_to_reference(sfam_trans_lmm, "sfam_trans_lmm.rds")
  expect_equal_to_reference(sfam_logreg, "sfam_logreg.rds")
  expect_equal_to_reference(sfam_lmm, "sfam_lmm.rds")
  expect_equal_to_reference(sfam_trans_lmm_from_list, "sfam_trans_lmm_from_list.rds")
})

test_that("Spec subset operators are working", {
  expect_s4_class(sfam_lm[1], "spec_fam")
  expect_s4_class(sfam_lmm[1], "spec_fam")
  expect_s4_class(sfam_logreg[1], "spec_fam")
  expect_s4_class(sfam_trans_lm[1], "spec_fam")
  expect_s4_class(sfam_trans_lmm[1], "spec_fam")
})


fam_lm <- make_fam(sfam_lm, db)
fam_trans_lm <- make_fam(sfam_trans_lm, db)
fam_logreg <- make_fam(sfam_logreg, db)
fam_lmm <- make_fam(sfam_lmm, db)
fam_trans_lmm <- make_fam(sfam_trans_lmm, db)
fam_only_rands <- make_fam(sfam_only_rands, db)

test_that("Families are created correctly", {
  expect_s4_class(fam_lm, "fam")
  expect_s4_class(fam_trans_lm, "fam")
  expect_s4_class(fam_logreg, "fam")
  expect_s4_class(fam_lmm, "fam")
  expect_s4_class(fam_trans_lmm, "fam")
  expect_s4_class(fam_only_rands, "fam")

  expect_s3_class(fam_lm[[1]]@fit, "lm")
  expect_s3_class(fam_trans_lm[[1]]@fit, "lm")
  expect_s3_class(fam_logreg[[1]]@fit, "glm")
  expect_s4_class(fam_lmm[[1]]@fit, "lmerMod")
  expect_s4_class(fam_trans_lmm[[1]]@fit, "lmerMod")
})

test_that("Methods on families are OK", {
  expect_equal_to_reference(confidence(fam_lm), "confidence_fam_lm.rds")
  expect_equal_to_reference(confidence(fam_trans_lm), "confidence_fam_trans_lm.rds")
  expect_equal_to_reference(confidence(fam_logreg), "confidence_fam_logreg.rds")
  expect_equal_to_reference(confidence(fam_lmm), "confidence_fam_lmm.rds")
  expect_equal_to_reference(confidence(fam_trans_lmm), "confidence_fam_trans_lmm.rds")

  expect_equal_to_reference(test(fam_lm), "test_fam_lm.rds")
  expect_equal_to_reference(test(fam_trans_lm), "test_fam_trans_lm.rds")
  expect_equal_to_reference(test(fam_logreg), "test_fam_logreg.rds")
  expect_equal_to_reference(test(fam_lmm), "test_fam_lmm.rds")
  expect_equal_to_reference(test(fam_trans_lmm), "test_fam_trans_lmm.rds")
})
