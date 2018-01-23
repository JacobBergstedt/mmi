
<!-- README.md is generated from README.Rmd. Please edit that file -->
mmi
===

mmi is an R package to conveniently fit and compare many models. It was developed in parallel to the work on the article "Natural variation in immune cell parameters is preferentially driven by genetic factors". The flow cytometry data, as well as the clinical/demographical data analyzed in that article is published within this package. A full tutorial on how to use the package, and how to reproduce the non-genetical analysis done in the paper will be available here shortly.

Installation
------------

You can install mmi from github with:

``` r
# install.packages("devtools")
devtools::install_github("JacobBergstedt/mmi")
```

The data
--------

The flow cytometry and demographical data can be accessed after loading the library:

``` r
library(mmi)
facs
#> # A tibble: 1,000 x 167
#>    SUBJID MFI_CD16_in_CD16hi_of_NKnew.panel4
#>     <int>                              <dbl>
#>  1      1                           8.781862
#>  2      2                          10.347725
#>  3      3                          10.400529
#>  4      4                          10.336924
#>  5      5                          10.219320
#>  6      6                          10.286230
#>  7      7                          10.360121
#>  8      8                          10.094191
#>  9      9                          10.479455
#> 10     10                          10.294516
#> # ... with 990 more rows, and 165 more variables:
#> #   MFI_CD16_of_CD56hi_of_NKnew.panel4 <dbl>,
#> #   MFI_CD69_in_CD16hi.panel4 <dbl>, MFI_CD69_in_CD56hi.panel4 <dbl>,
#> #   MFI_CD8a_in_CD16hi.panel4 <dbl>, MFI_CD8a_in_CD56hi.panel4 <dbl>,
#> #   MFI_HLADR_in_CD16hi.panel4 <dbl>, MFI_HLADR_in_CD56hi.panel4 <dbl>,
#> #   MFI_NKp46_of_NK_cells.panel4 <dbl>,
#> #   MFI_of_CD69_in_CD69posCD16hi.panel4 <dbl>,
#> #   MFI_of_CD69_in_CD69posCD56hi.panel4 <dbl>,
#> #   MFI_of_CD69_in_CD8aposCD16hi.panel4 <dbl>,
#> #   MFI_of_CD69_in_CD8aposCD56hi.panel4 <dbl>,
#> #   MFI_of_CD69_in_HLADRposCD16hi.panel4 <dbl>,
#> #   MFI_of_CD8a_in_CD69posCD16hi.panel4 <dbl>,
#> #   MFI_of_CD8a_in_CD69posCD56hi.panel4 <dbl>,
#> #   MFI_of_CD8a_in_CD8aposCD16hi.panel4 <dbl>,
#> #   MFI_of_CD8a_in_CD8aposCD56hi.panel4 <dbl>,
#> #   MFI_of_CD8a_in_HLADRposCD16hi.panel4 <dbl>,
#> #   MFI_of_HLADR_in_CD69posCD16hi.panel4 <dbl>,
#> #   MFI_of_HLADR_in_CD69posCD56hi.panel4 <dbl>,
#> #   MFI_of_HLADR_in_CD8aposCD16hi.panel4 <dbl>,
#> #   MFI_of_HLADR_in_CD8aposCD56hi.panel4 <dbl>,
#> #   MFI_of_HLADR_in_HLADRposCD16hi.panel4 <dbl>,
#> #   MFI_of_HLADR_in_HLADRposCD56hi.panel4 <dbl>, N_CD16hi.panel4 <dbl>,
#> #   N_CD56hi.panel4 <dbl>, N_CD69posCD16hi.panel4 <dbl>,
#> #   N_CD69posCD56hi.panel4 <dbl>, N_CD8aposCD16hi.panel4 <dbl>,
#> #   N_CD8aposCD56hi.panel4 <dbl>, N_HLADRposCD56hi.panel4 <dbl>,
#> #   N_NK_cells.panel4 <dbl>, ratio_CD16hi_CD56hi.panel4 <dbl>,
#> #   ratio_MFI_CD16_in_CD16hi_and_MFI_CD16_inCD56hi.panel4 <dbl>,
#> #   MFI_CD16_in_CD14hi_mono.panel5 <dbl>,
#> #   MFI_CD16_in_CD16hi_mono.panel5 <dbl>, N_CD14hi_mono.panel5 <dbl>,
#> #   N_CD16hi_mono.panel5 <dbl>, N_mono.panel5 <dbl>,
#> #   N_CD45pos.panel5 <dbl>, N_total.panel5 <dbl>,
#> #   MFI_BASOPHILS_CD16.panel7 <dbl>, MFI_BASOPHILS_CD203c.panel7 <dbl>,
#> #   MFI_BASOPHILS_CD32.panel7 <dbl>, MFI_BASOPHILS_FceRI.panel7 <dbl>,
#> #   MFI_EOSINOPHILS_CD16.panel7 <dbl>, MFI_EOSINOPHILS_CD62L.panel7 <dbl>,
#> #   MFI_EOSINOPHILS_FceRI.panel7 <dbl>, MFI_NEUTROPHILS_CD16.panel7 <dbl>,
#> #   MFI_NEUTROPHILS_CD62L.panel7 <dbl>,
#> #   MFI_NEUTROPHILS_FceRI.panel7 <dbl>, N_VIABLE_BASOPHILS.panel7 <dbl>,
#> #   N_VIABLE_EOSINOPHILS.panel7 <dbl>, N_VIABLE_NEUTROPHILS.panel7 <dbl>,
#> #   CD86_MFI_in_CD14hi.panel8 <dbl>, CD86_MFI_in_cDC1.panel8 <dbl>,
#> #   CD86_MFI_in_cDC3.panel8 <dbl>, CD86_MFI_in_pDC.panel8 <dbl>,
#> #   HLADR_MFI_in_CD14hi.panel8 <dbl>, HLADR_MFI_in_cDC1.panel8 <dbl>,
#> #   HLADR_MFI_in_cDC3.panel8 <dbl>, HLADR_MFI_in_pDC.panel8 <dbl>,
#> #   N_cDC1.panel8 <dbl>, N_cDC3.panel8 <dbl>, N_pDC.panel8 <dbl>,
#> #   N_ILC.panel10 <dbl>, N_ILC1.panel10 <dbl>, N_group2_ILC.panel10 <dbl>,
#> #   N_CD56negCD117pos_LTiandILC3.panel10 <dbl>, N_CD56_ILC.panel10 <dbl>,
#> #   MFI_of_CD127_of_ILC1.panel10 <dbl>,
#> #   MFI_of_CD161_of_ILC1.panel10 <dbl>,
#> #   MFI_of_CD161_of_CD56negCD117pos_LTiandILC3.panel10 <dbl>,
#> #   N_CD161pos_CD56negCD117pos_LTiandILC3.panel10 <dbl>,
#> #   N_CD56posNKp44neg_group1and3_ILC.panel10 <dbl>,
#> #   N_NCRpos_ILC3.panel10 <dbl>, N_CD8bnegCD4neg.panel1 <dbl>,
#> #   MFI_CCR7_in_CD4pos_CM.panel1 <dbl>,
#> #   MFI_CCR7_in_CD4pos_EM.panel1 <dbl>,
#> #   MFI_CCR7_oin_CD4pos_EMRA.panel1 <dbl>,
#> #   MFI_CCR7_in_CD4_naive.panel1 <dbl>,
#> #   MFI_CCR7_in_CD8bpos_CM.panel1 <dbl>,
#> #   MFI_CCR7_in_CD8bpos_EM.panel1 <dbl>,
#> #   MFI_CCR7_in_CD8bpos_EMRA.panel1 <dbl>,
#> #   MFI_CCR7_in_CD8bpos_naive.panel1 <dbl>, N_CD3pos.panel1 <dbl>,
#> #   N_CD4pos.panel1 <dbl>, N_CD4pos_CM.panel1 <dbl>,
#> #   N_CD4pos_EM.panel1 <dbl>, N_CD4pos_naive.panel1 <dbl>,
#> #   N_CD8bpos.panel1 <dbl>, N_CD8bpos_CM.panel1 <dbl>,
#> #   N_CD8bpos_EM.panel1 <dbl>, N_CD8bpos_EMRA.panel1 <dbl>,
#> #   N_CD8bpos_naive.panel1 <dbl>, N_HLADRpos_in_CD4pos_CM.panel1 <dbl>,
#> #   N_HLADRpos_in_CD4pos_EM.panel1 <dbl>,
#> #   N_HLADRpos_in_CD8bpos_CM.panel1 <dbl>,
#> #   N_HLADRpos_in_CD8bpos_EM.panel1 <dbl>, ratio_CD4_CD8.panel1 <dbl>, ...
ecrf
#> # A tibble: 1,000 x 45
#>      Age OwnsHouse PhysicalActivity    Sex LivesWithPartner LivesWithKids
#>    <dbl>    <fctr>            <dbl> <fctr>           <fctr>        <fctr>
#>  1 27.25       Yes              0.5 Female              Yes           Yes
#>  2 22.33       Yes              3.0 Female               No            No
#>  3 28.83       Yes              0.0 Female              Yes            No
#>  4 23.67       Yes              0.0 Female              Yes            No
#>  5 21.17        No              0.5 Female               No            No
#>  6 22.50       Yes              3.0 Female              Yes            No
#>  7 24.25       Yes              0.0 Female              Yes           Yes
#>  8 26.17       Yes              1.5 Female               No            No
#>  9 23.83       Yes              0.0 Female              Yes            No
#> 10 20.25        No              2.0 Female               No            No
#> # ... with 990 more rows, and 39 more variables: BornInCity <fctr>,
#> #   AncestryPC1 <dbl>, AncestryPC2 <dbl>, Inbreeding <dbl>, BMI <dbl>,
#> #   CMVInfection <fctr>, FluIgG <dbl>, MetabolicScore <dbl>,
#> #   LowAppetite <dbl>, TroubleConcentrating <dbl>, TroubleSleeping <dbl>,
#> #   HoursOfSleep <dbl>, Listless <dbl>, UsesCannabis <fctr>,
#> #   RecentPersonalCrisis <fctr>, Smoking <fctr>, Employed <fctr>,
#> #   Education <fctr>, DustExposure <fctr>, Income <fctr>,
#> #   HadMeasles <fctr>, HadRubella <fctr>, HadChickenPox <fctr>,
#> #   HadMumps <fctr>, HadTonsillectomy <fctr>, HadAppendicectomy <fctr>,
#> #   VaccineHepA <fctr>, VaccineMMR <fctr>, VaccineTyphoid <fctr>,
#> #   VaccineWhoopingCough <fctr>, VaccineYellowFever <fctr>,
#> #   VaccineHepB <fctr>, VaccineFlu <fctr>, SUBJID <int>,
#> #   DepressionScore <dbl>, HeartRate <dbl>, Temperature <dbl>,
#> #   HourOfSampling <dbl>, DayOfSampling <fctr>
```
