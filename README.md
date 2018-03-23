
<!-- README.md is generated from README.Rmd. Please edit that file -->
mmi
===

mmi is an R package to conveniently fit and compare many models. It was developed in parallel to the work on the article "Natural variation in immune cell parameters is preferentially driven by genetic factors". The flow cytometry data, as well as the clinical/demographical data analyzed in that article are published within this package. A tutorial on how the package was used to analyze the impact of the clinical/demographical data on immunophenotypes can be found [here](https://jacobbergstedt.github.io/mmi/). Note that the R documentation as of now is not updated and is not correct. It will be updated soon.

Installation
------------

You can install mmi from github with:

``` r
install.packages("devtools")
devtools::install_github("JacobBergstedt/mmi")
```

The data
--------

The flow cytometry and demographical data can be accessed after loading the library:

``` r
library(mmi)
facs
#> # A tibble: 816 x 167
#>    SUBJID MFI_CD16_in_CD16hi_of… MFI_CD16_of_CD56hi_of… MFI_CD69_in_CD16h…
#>     <int>                  <int>                  <dbl>              <int>
#>  1      2                  31186                  1910.                605
#>  2      3                  32877                  4283.                794
#>  3      4                  30851                  4633.                688
#>  4      5                  27428                  3261.                620
#>  5      8                  24202                  3247.                627
#>  6      9                  35577                  2037.                594
#>  7     11                  27207                  2874.                388
#>  8     13                  12138                  1348.                419
#>  9     16                  18179                  2338.                353
#> 10     19                  15629                  1639.                381
#> # ... with 806 more rows, and 163 more variables:
#> #   MFI_CD69_in_CD56hi.panel4 <int>, MFI_CD8a_in_CD16hi.panel4 <dbl>,
#> #   MFI_CD8a_in_CD56hi.panel4 <int>, MFI_HLADR_in_CD16hi.panel4 <dbl>,
#> #   MFI_HLADR_in_CD56hi.panel4 <dbl>, MFI_NKp46_of_NK_cells.panel4 <int>,
#> #   MFI_of_CD69_in_CD69posCD16hi.panel4 <int>,
#> #   MFI_of_CD69_in_CD69posCD56hi.panel4 <int>,
#> #   MFI_of_CD69_in_CD8aposCD16hi.panel4 <dbl>,
#> #   MFI_of_CD69_in_CD8aposCD56hi.panel4 <int>,
#> #   MFI_of_CD69_in_HLADRposCD16hi.panel4 <dbl>,
#> #   MFI_of_CD8a_in_CD69posCD16hi.panel4 <dbl>,
#> #   MFI_of_CD8a_in_CD69posCD56hi.panel4 <dbl>,
#> #   MFI_of_CD8a_in_CD8aposCD16hi.panel4 <int>,
#> #   MFI_of_CD8a_in_CD8aposCD56hi.panel4 <int>,
#> #   MFI_of_CD8a_in_HLADRposCD16hi.panel4 <dbl>,
#> #   MFI_of_HLADR_in_CD69posCD16hi.panel4 <dbl>,
#> #   MFI_of_HLADR_in_CD69posCD56hi.panel4 <dbl>,
#> #   MFI_of_HLADR_in_CD8aposCD16hi.panel4 <dbl>,
#> #   MFI_of_HLADR_in_CD8aposCD56hi.panel4 <dbl>,
#> #   MFI_of_HLADR_in_HLADRposCD16hi.panel4 <int>,
#> #   MFI_of_HLADR_in_HLADRposCD56hi.panel4 <int>, N_CD16hi.panel4 <int>,
#> #   N_CD56hi.panel4 <int>, N_CD69posCD16hi.panel4 <int>,
#> #   N_CD69posCD56hi.panel4 <int>, N_CD8aposCD16hi.panel4 <int>,
#> #   N_CD8aposCD56hi.panel4 <int>, N_HLADRposCD56hi.panel4 <int>,
#> #   N_NK_cells.panel4 <int>, ratio_CD16hi_CD56hi.panel4 <dbl>,
#> #   ratio_MFI_CD16_in_CD16hi_and_MFI_CD16_inCD56hi.panel4 <dbl>,
#> #   MFI_CD16_in_CD14hi_mono.panel5 <int>,
#> #   MFI_CD16_in_CD16hi_mono.panel5 <int>, N_CD14hi_mono.panel5 <int>,
#> #   N_CD16hi_mono.panel5 <int>, N_mono.panel5 <int>,
#> #   N_CD45pos.panel5 <dbl>, N_total.panel5 <dbl>,
#> #   MFI_BASOPHILS_CD16.panel7 <dbl>, MFI_BASOPHILS_CD203c.panel7 <dbl>,
#> #   MFI_BASOPHILS_CD32.panel7 <int>, MFI_BASOPHILS_FceRI.panel7 <int>,
#> #   MFI_EOSINOPHILS_CD16.panel7 <int>, MFI_EOSINOPHILS_CD62L.panel7 <int>,
#> #   MFI_EOSINOPHILS_FceRI.panel7 <int>, MFI_NEUTROPHILS_CD16.panel7 <dbl>,
#> #   MFI_NEUTROPHILS_CD62L.panel7 <int>,
#> #   MFI_NEUTROPHILS_FceRI.panel7 <dbl>, N_VIABLE_BASOPHILS.panel7 <int>,
#> #   N_VIABLE_EOSINOPHILS.panel7 <int>, N_VIABLE_NEUTROPHILS.panel7 <dbl>,
#> #   CD86_MFI_in_CD14hi.panel8 <int>, CD86_MFI_in_cDC1.panel8 <dbl>,
#> #   CD86_MFI_in_cDC3.panel8 <dbl>, CD86_MFI_in_pDC.panel8 <dbl>,
#> #   HLADR_MFI_in_CD14hi.panel8 <int>, HLADR_MFI_in_cDC1.panel8 <int>,
#> #   HLADR_MFI_in_cDC3.panel8 <int>, HLADR_MFI_in_pDC.panel8 <int>,
#> #   N_cDC1.panel8 <int>, N_cDC3.panel8 <int>, N_pDC.panel8 <int>,
#> #   N_ILC.panel10 <int>, N_ILC1.panel10 <int>, N_group2_ILC.panel10 <int>,
#> #   N_CD56negCD117pos_LTiandILC3.panel10 <int>, N_CD56_ILC.panel10 <int>,
#> #   MFI_of_CD127_of_ILC1.panel10 <int>,
#> #   MFI_of_CD161_of_ILC1.panel10 <dbl>,
#> #   MFI_of_CD161_of_CD56negCD117pos_LTiandILC3.panel10 <int>,
#> #   N_CD161pos_CD56negCD117pos_LTiandILC3.panel10 <int>,
#> #   N_CD56posNKp44neg_group1and3_ILC.panel10 <int>,
#> #   N_NCRpos_ILC3.panel10 <int>, N_CD8bnegCD4neg.panel1 <int>,
#> #   MFI_CCR7_in_CD4pos_CM.panel1 <dbl>,
#> #   MFI_CCR7_in_CD4pos_EM.panel1 <dbl>,
#> #   MFI_CCR7_oin_CD4pos_EMRA.panel1 <dbl>,
#> #   MFI_CCR7_in_CD4_naive.panel1 <int>,
#> #   MFI_CCR7_in_CD8bpos_CM.panel1 <dbl>,
#> #   MFI_CCR7_in_CD8bpos_EM.panel1 <int>,
#> #   MFI_CCR7_in_CD8bpos_EMRA.panel1 <int>,
#> #   MFI_CCR7_in_CD8bpos_naive.panel1 <int>, N_CD3pos.panel1 <dbl>,
#> #   N_CD4pos.panel1 <dbl>, N_CD4pos_CM.panel1 <int>,
#> #   N_CD4pos_EM.panel1 <int>, N_CD4pos_naive.panel1 <int>,
#> #   N_CD8bpos.panel1 <int>, N_CD8bpos_CM.panel1 <int>,
#> #   N_CD8bpos_EM.panel1 <int>, N_CD8bpos_EMRA.panel1 <int>,
#> #   N_CD8bpos_naive.panel1 <int>, N_HLADRpos_in_CD4pos_CM.panel1 <int>,
#> #   N_HLADRpos_in_CD4pos_EM.panel1 <int>,
#> #   N_HLADRpos_in_CD8bpos_CM.panel1 <int>,
#> #   N_HLADRpos_in_CD8bpos_EM.panel1 <int>, ratio_CD4_CD8.panel1 <dbl>,
#> #   N_CD4pos_EMRA.panel1 <int>, N_HLADRpos_in_CD4pos_EMRA.panel1 <int>, …
ecrf
#> # A tibble: 816 x 44
#>    SUBJID HourOfSampling DayOfSampling   Age OwnsHouse PhysicalActivity
#>     <int>          <dbl> <fct>         <dbl> <fct>                <dbl>
#>  1      2           8.88 40             22.3 Yes                  3.00 
#>  2      3           9.35 40             28.8 Yes                  0.   
#>  3      4           8.67 40             23.7 Yes                  0.   
#>  4      5           9.88 40             21.2 No                   0.500
#>  5      8           8.55 81             26.2 Yes                  1.50 
#>  6      9           8.80 81             23.8 Yes                  0.   
#>  7     11           9.05 82             26.4 No                   4.00 
#>  8     13           9.55 82             21.7 Yes                  0.   
#>  9     16           9.08 109            26.5 No                   0.   
#> 10     19           9.23 110            23.2 Yes                  1.50 
#> # ... with 806 more rows, and 38 more variables: Sex <fct>,
#> #   LivesWithPartner <fct>, LivesWithKids <fct>, BornInCity <fct>,
#> #   AncestryPC1 <dbl>, AncestryPC2 <dbl>, BMI <dbl>, CMVInfection <fct>,
#> #   FluIgG <dbl>, MetabolicScore <dbl>, LowAppetite <dbl>,
#> #   TroubleConcentrating <dbl>, TroubleSleeping <dbl>, HoursOfSleep <dbl>,
#> #   Listless <dbl>, UsesCannabis <fct>, RecentPersonalCrisis <fct>,
#> #   Smoking <fct>, Employed <fct>, Education <fct>, DustExposure <fct>,
#> #   Income <fct>, HadMeasles <fct>, HadRubella <fct>, HadChickenPox <fct>,
#> #   HadMumps <fct>, HadTonsillectomy <fct>, HadAppendicectomy <fct>,
#> #   VaccineHepA <fct>, VaccineMMR <fct>, VaccineTyphoid <fct>,
#> #   VaccineWhoopingCough <fct>, VaccineYellowFever <fct>,
#> #   VaccineHepB <fct>, VaccineFlu <fct>, DepressionScore <dbl>,
#> #   HeartRate <dbl>, Temperature <dbl>
```
