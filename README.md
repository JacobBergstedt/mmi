The impact of non-genetical factors on immunophenotypes
================
Jacob Bergstedt

Introduction
------------

The *mmi* package was used in the recently published article "Natural variation in immune cell parameters is preferentially driven by genetic factors". The article tries to quantify the sources of variation for different functions in the immune system. The article includes three different types of immunophenotypes: absolute counts of immune cells per blood volume, mean flourescence intensity of surface markers, and a few ratios between absolute cell counts.

The typical determinants of a phenotype are either genetical, *i.e* differences in the genome of individuals, or non-genetical, things such as age, sex, and various other cultural and demographical factors, or an interaction of the two. We focused on genetical and non-genetical factors in the article. A total of 166 immunophenotypes arewere analyzed, measured on 1000 Frenchmen stratified evenly across bins of 10 years between 20 and 70 years old, and across sex.

Here, we redo the analysis of the impact of non-genetical factorso on the immunophenotypes. We do not reproduce the analysis of genetical factors, since we cant publically release the genetics data due to privacy issues. Also, 184 of the 1000 donors included in the study did not want their data to be publically available. Therefore, we only publish data from the remaining 816 donors.

The analysis is based on functionality implemented **mmi** package. We will not describe the workings of the package itself in any detail here, interested readers are pointed to the forthcoming **mmi** package vignette and the **mmi** documentation in R.

First we load some libraries:

``` r
library(tidyverse)
library(magrittr)
library(scales)
library(mmi)
```

The immunophenotypes, stored in the variable *facs*, and the non-genetical variables, stored in the variable *ecrf*, are loaded to memory with the mmi package:

``` r
dim(facs)
```

    ## [1] 816 174

``` r
dim(ecrf)
```

    ## [1] 816  45

Included in the package is the tibble *G.annotation* that records all the names of the immunophenotypes in the *facs* tibble, cleaned up names use in figures, and if they are a part of the adaptive or innate immune system.

We will need a tibble including all data:

``` r
db <- left_join(facs, ecrf)
```

We can take a look at the distribution of age and sex in the data after removing donors that didn't want their data to be public:

``` r
table(ecrf$Sex)
```

    ## 
    ##   Male Female 
    ##    399    417

``` r
table(cut(ecrf$Age, breaks = c(20, 30, 40, 50, 60, 70)))
```

    ## 
    ## (20,30] (30,40] (40,50] (50,60] (60,70] 
    ##     122     160     170     188     176

Originally, each of these bins had 200 donors, so we see that it's mostly young people that are concerned with their data privacy.

The *mmi* package includes the function *plot\_list* that conveniently plots histograms of variables in a data frame (actually it plots variables in a list, hence the name, but data frames are lists). We can use it to look at the numerical non-genetical variables:

``` r
plot_list(select(keep(ecrf, is.numeric), -SUBJID), title_sz = 9)
```

<img src="mmi_facs_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-5-1.png" width="1000px" style="display: block; margin: auto;" />

The *MetabolicScore* variable is a composite variable designed to measure an individuals metabolic health, described in \[REF\]. The *HourOfSampling* is the hour of the day when the blood sample for the measurement was drawn. The other variables are usual intrinsic factors like age, typical clinical variables like body temperature and heart rate, antibodies for flu (*FluIgG*), and variables related to mental health. The variable *DepressionScore* is a composite variable, similar to *MetabolicScore* that summarizes the other mental health variables.

If we convert categorical values to numeric, we can also look at the categorical variables:

``` r
plot_list(keep(ecrf, function(x) !is.numeric(x)), title_sz = 7)
```

<img src="mmi_facs_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-7-1.png" width="1000px" style="display: block; margin: auto;" /> There are some socio-economic variables like level of education and income, and variables related to vaccination and childhood diseases. The variables *Sex*, *CMVInfection*, which states if an individual is infected by the cytomegalovirus (<https://en.wikipedia.org/wiki/Cytomegalovirus>), and *Smoking* are key variables in the study. The *DayOfSampling* variable is what day the blood was drawn for the sample.

Batch effects
=============

Lets look a little closer on the *DayOfSampling* variable. To see how many donors had blood drawn at a particular day (counted from the first day) we can for instance do

``` r
people_each_day <- as.numeric(table(ecrf$DayOfSampling))
c(mean(people_each_day), 1.96 * sd(people_each_day))
```

    ## [1] 7.698113 4.802294

``` r
plt_frame <- tibble(day = as.numeric(levels(ecrf$DayOfSampling)), 
                    nr_people = people_each_day)

ggplot(plt_frame, aes(x = day, y = nr_people)) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = seq(1, 12, 1), minor_breaks = NULL)
```

<img src="mmi_facs_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-8-1.png" width="1000px" style="display: block; margin: auto;" /> So around 7.7 ± 4.80 blood samples are drawn each day. To see if there is a batch effect across day of blood draw we can estimate the intraclass correlation coefficient. This is the correlation among observations within the same day. If this parameter is large it means that the variation between days is larger then the variation within days, indicating a batch effect, since donors where randomly assigned to day of blood draw. To compute the intraclass correlation coefficient we must first setup the models. In this case we only want the day of sampling as a random effect, and we want to build a model for each immunophenotype. The intraclass coefficient can also be interpreted as the proportion of the total variability attributed to variability between days.

``` r
spec <- specify(G.annotation$FACS.NAME, rands = "DayOfSampling")
fam <- make_fam(spec, db)
intraclass <- prop_var(fam)
select(intraclass, response, prop_var) %>% arrange(desc(prop_var))
```

    ## # A tibble: 166 x 2
    ##                                response prop_var
    ##                                   <chr>    <dbl>
    ##  1        MFI_CCR7_in_CD8bpos_EM.panel1 88.57938
    ##  2         MFI_CCR7_in_CD4pos_EM.panel1 88.37496
    ##  3      MFI_CCR7_in_CD8bpos_EMRA.panel1 88.27123
    ##  4        MFI_CCR7_in_CD8bpos_CM.panel1 86.35755
    ##  5            CD19_MFI_in_Bcells.panel6 83.23105
    ##  6         MFI_NEUTROPHILS_FceRI.panel7 83.14817
    ##  7         MFI_CCR7_in_CD4pos_CM.panel1 77.35704
    ##  8       MFI_CD16_in_CD14hi_mono.panel5 76.25022
    ##  9      MFI_CCR7_oin_CD4pos_EMRA.panel1 74.21744
    ## 10           MFI_HLADR_in_CD56hi.panel4 70.71926
    ## 11         MFI_CCR7_in_CD4_naive.panel1 70.65100
    ## 12     MFI_CCR7_in_CD8bpos_naive.panel1 70.52851
    ## 13 MFI_of_HLADR_in_CD69posCD16hi.panel4 69.36956
    ## 14 MFI_of_HLADR_in_CD8aposCD16hi.panel4 69.31276
    ## 15           MFI_HLADR_in_CD16hi.panel4 69.09242
    ## 16 MFI_of_HLADR_in_CD8aposCD56hi.panel4 68.63971
    ## 17   MFI_CD16_in_CD16hi_of_NKnew.panel4 59.76492
    ## 18  MFI_of_CD69_in_CD69posCD16hi.panel4 50.39060
    ## 19          MFI_NEUTROPHILS_CD16.panel7 49.86390
    ## 20            MFI_BASOPHILS_CD16.panel7 47.69724
    ## 21               CD86_MFI_in_pDC.panel8 44.83041
    ## 22         MFI_NEUTROPHILS_CD62L.panel7 44.45143
    ## 23         MFI_EOSINOPHILS_FceRI.panel7 44.02209
    ## 24         MFI_of_CD127_of_ILC1.panel10 42.53130
    ## 25          MFI_EOSINOPHILS_CD16.panel7 42.11400
    ## 26                        N_cDC1.panel8 41.88718
    ## 27         MFI_NKp46_of_NK_cells.panel4 39.83472
    ## 28  MFI_of_CD69_in_CD8aposCD56hi.panel4 37.37920
    ## 29         MFI_EOSINOPHILS_CD62L.panel7 36.59514
    ## 30       MFI_ICOS_in_memory_Treg.panel2 34.89225
    ## 31        MFI_ICOS_in_naive_Treg.panel2 34.38638
    ## 32       N_HLADRpos_in_CD4pos_CM.panel1 34.21556
    ## 33            CD24_MFI_in_NaiveB.panel6 33.66949
    ## 34            MFI_CD69_in_CD56hi.panel4 33.31129
    ## 35                        N_cDC3.panel8 32.63381
    ## 36 MFI_of_HLADR_in_CD69posCD56hi.panel4 32.11407
    ## 37  MFI_of_CD69_in_CD8aposCD16hi.panel4 31.98400
    ## 38        CD21_MFI_in_memBCD24lo.panel6 31.56317
    ## 39         CD24_MFI_in_memBcells.panel6 30.24108
    ## 40            MFI_CD69_in_CD16hi.panel4 30.20404
    ## # ... with 126 more rows

Apparently, day of blood draw has a huge effect on some of the phenotypes, particularly MFIs. To get a sense of how day of blood draw affects the measurements we can plot immunophenotype values across days. Let's take a look at the 6 immunophenotypes with the largest intraclass coefficient.

``` r
resp <- intraclass %>% 
  arrange(desc(prop_var)) %>% 
  slice(1:8) %$% response

plt_frame <- db[c("DayOfSampling", resp)] %>% 
  gather(key = "Pheno", value = "Value", -DayOfSampling) 

plt_frame$Pheno <- factor(plt_frame$Pheno, levels = resp)

plt_frame %>% 
  ggplot(aes(x = factor(DayOfSampling), Value)) +
  geom_boxplot() +
  facet_wrap(~ Pheno, scales = "free", ncol = 2)
```

<img src="mmi_facs_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-1.png" width="1000px" style="display: block; margin: auto;" /> As expected, there is considerable between-day variability. Also, the pattern of variability doesn't look particularly Gaussian: some days are outliers for some phenotypes, and the values sometimes seem to jump between blocks of days with similar measurements. There also appears to be autocorrelation in time. We dont make an attempt to model this in any meaningful way in the paper, we introduce a Gaussian distributed random effect to control for the batch effect, and assume that non of our treatments are affected by day of blood draw in a meaningful way, which would still give us relevant inference for the treatment effects.

We can take a look at how including a random effect controls for the batch effect by using the **residuals** function in the **mmi** package. Note that this function only works for if all models fitted has the same predictors.

``` r
resids <- residuals(fam) %>% 
  filter(response %in% resp)
resids$response <- factor(resids$response, levels = resp)

resids %>% 
  ggplot(aes(x = factor(DayOfSampling), residuals)) +
  geom_boxplot() +
  facet_wrap(~ response, scales = "free", ncol = 2)
```

<img src="mmi_facs_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-11-1.png" width="1000px" style="display: block; margin: auto;" />

Modeling
========

It is well-known that a person's age affects their immunophenotype. Links have also been made between the immune parameter levels and sex. Several studies have also reported that being infected by the cytomegalovirus (CMV) can affect immune cell counts and proportions. We therefore include age and sex as controls for all models, and CMV infection status for absolute cell counts.

There are three different types of immunophenotypes in the study. We also have immune cells at various levels in the immune cell differentiation hierarchy. The further a particular cell is differentiated the more skewed the distribution tends to be. Almost all immunophenotypes should also be non-negative. The most reasonable way to model such data is often to use a log-normal distribution for the response. The count phenotypes could also be modelled as being distributed accoding to a negative binomial distribution. There are some phenotypes that have zero values. To deal with that we add a unit value to these distributions prior to log transformation. We can make a comparison of the normal, lognormal, and negative binomal models using the AIC (<https://en.wikipedia.org/wiki/Akaike_information_criterion>),

``` r
controls <- c("Age", "Sex")
count_controls <- c(controls, "CMVInfection")
only_counts <- G.counts[!grepl("ratio", G.counts)]

# Specify negative binomial models for all counts with the controls defined above as predictors
spec_nb <- specify(only_counts, controls = count_controls, model = "negbin")

# Specify normal models
spec_no <- specify(only_counts, controls = count_controls, model = "lm")          

# Specify lognormal models
spec_logno <- specify(only_counts, controls = count_controls, model = "trans_lm", trans = "log")

# Log normal models cannot include zeros
facs_with_zero <- map_lgl(facs, any_leq_zero)
sum(facs_with_zero) 
```

    ## [1] 47

``` r
facs_with_zero <- names(facs[facs_with_zero])
facs_no_zeros <- facs
facs_no_zeros[facs_with_zero] <- facs_no_zeros[facs_with_zero] + 1
db_no_zeros <- left_join(facs_no_zeros, ecrf)

# Here we fit the models
fam_nb <- make_fam(spec_nb, db)
fam_no <- make_fam(spec_no, db)
fam_logno <- make_fam(spec_logno, db_no_zeros)

# The concatenation operator is overloaded for both spec_fam and fam objects
fam <- c(fam_nb, fam_no, fam_logno)

# Compute AIC
AIC_comp <- AIC(fam)

# We sort the phenotypes according to AIC value for the normal model
sort_resp <- AIC_comp %>% 
  filter(model == "mmi_lm", trans == "log") %>% 
  arrange(desc(AIC)) %$% 
  response

# Fixing some nice labels
anno <- left_join(tibble(FACS.NAME = sort_resp), G.annotation)
AIC_comp$legend <- factor(AIC_comp$response, anno$FACS.NAME)
levels(AIC_comp$legend) <- anno$FACS.DESC
AIC_comp$model <- factor(paste0(AIC_comp$model, "x", AIC_comp$trans))
levels(AIC_comp$model) <- c("Normal", "Lognormal", "Negative binomial")

# Comparison plot
AIC_comp %>% 
  ggplot(aes(x = legend, y = AIC, group = model, colour = model)) +
  geom_line() +
  scale_y_continuous(breaks = pretty_breaks(20)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

<img src="mmi_facs_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-12-1.png" width="1000px" style="display: block; margin: auto;" />

Using a normal distribution for the response is the worst for almost all phenotypes. The distinction between the log-normal and the negative binomial model is less clear but the negative binomial model seems to have the advantage. We can try to investigate by plotting the qq-plots of the models that differed the most. First we plot qq-plots from the negative binomial models,

``` r
# Compute difference in AIC between negative binomial and lognormal models
aic_diff <- AIC_comp %>% 
  filter(model != "Normal") %>% 
  arrange(response, AIC) %>% 
  group_by(response) %>%
  summarize(diff_models = diff(AIC), best_model = first(model)) %>%
  arrange(diff_models)

# Find the 10 immunophenotypes that differed the most
top_10_nb <- aic_diff %>% 
  filter(best_model == "Negative binomial") %>% 
  tail(10) %>% 
  dplyr::select(response)

res <- residuals(fam_nb) %>%
  dplyr::select(-predictors, -trans)

res %>%
  filter(response %in% top_10_nb$response) %>%
  ggplot(aes(x = expected_norm, y = residuals)) +
  geom_point() +
  facet_wrap(~ response, scales = "free", ncol = 3)
```

<img src="mmi_facs_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-13-1.png" width="1000px" style="display: block; margin: auto;" />

Then we plot the qq-plots from the log-normal models.

``` r
res <- residuals(fam_logno) %>%
  dplyr::select(-predictors, -trans)

res %>%
  filter(response %in% top_10_nb$response) %>%
  ggplot(aes(x = expected_norm, y = residuals)) +
  geom_point() +
  facet_wrap(~ response, scales = "free", ncol = 3)
```

<img src="mmi_facs_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-14-1.png" width="1000px" style="display: block; margin: auto;" />

It's clear that the log-link in the negative binomial model and the log transformation in the log-normal model isn't appropriate for the smallest values of these response variables. This appears to be problem for all 10 response variables where the AIC differed the most between the models. Therefore is seems reasonable to assume that the negative binomial model somehow deals better with the problem of misspecification for small values, although it cannot be easily discerned from the qq-plots.

It doesn't make sense to use a count distribution for the ratio and MFI phenotypes so for those we only compare the normal and the log-normal distribution,

``` r
controls <- c("Age", "Sex", "Smoking", "CMVInfection")
spec <- specify(G.mfis, controls = controls, model = "lm")
spec <- c(spec,
          specify(G.mfis, controls = controls,
                  model = "trans_lm", 
                  trans = "log"))


fam <- make_fam(spec, db_no_zeros)
AIC_comp <- AIC(fam)

sort_resp <- AIC_comp %>% 
  filter(model == "mmi_lm", trans == "log") %>% 
  arrange(desc(AIC)) %$% 
  response
anno <- left_join(tibble(FACS.NAME = sort_resp), G.annotation)
AIC_comp$response <- factor(AIC_comp$response, anno$FACS.NAME)
levels(AIC_comp$response) <- anno$FACS.DESC
AIC_comp$model <- factor(paste0(AIC_comp$model, "x", AIC_comp$trans))
levels(AIC_comp$model) <- c("Normal", "Lognormal")

AIC_comp %>% 
  ggplot(aes(x = response, y = AIC, group = model, colour = model)) +
  geom_line(size = 1.5) +
  scale_y_continuous(breaks = pretty_breaks(20)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = rel(0.8)),
        axis.title = element_blank())
```

<img src="mmi_facs_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-15-1.png" width="1000px" style="display: block; margin: auto;" />

The two models appear to be similar, with some spikes in the normal model for some response variables. Overall the log-normal model works relatively well for all data, and in the interest of comparison between models we choose to model all response variables as being log-normally distributed. Note also that these comparisons have been done without the *DayOfSampling* random effect. Theoretically, adding a random effect would probably not change the relative performance of the models. However, (at least in my experience) the fitting of negative binomial random effects models is less robust than its linear counterpart, and the inference theory is less well understood.

Inference
=========

We will fit a model for each response variable and each treatment variable, for a total of 6806 models. The models will be fit by the *mmi* package using functions from the *lme4* package. To be more formal we will fit the following models,
