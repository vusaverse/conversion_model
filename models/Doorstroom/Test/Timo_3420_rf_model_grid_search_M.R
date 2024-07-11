## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Titel van het bestand ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code voor Student Analytics Vrije Universiteit Amsterdam
## Copyright 2022 VU
## Web Page: http://www.vu.nl
## Contact: vu-analytics@vu.nl
## Verspreiding buiten de VU: Ja
##
## Doel: Doel
##
## Afhankelijkheden: Afhankelijkheid
##
## Datasets: Datasets
##
## Opmerkingen:
## 1) Geen.
## 2) ___
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## TODO:
## 1) ___.
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Geschiedenis:
## 18-07-2022: TK: Aanmaak bestand
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. INLEZEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Lees alle benodigde bestanden in:
install.packages('tidymodels')
install.packages("ranger")
library(ranger)
library(tidymodels)

## Variables for filtering students
vFiltervariabelen <- c("INS_Studentnummer",
                       "INS_Inschrijvingsjaar",
                       "INS_Inschrijvingsjaar_EOI",
                       "INS_Opleidingsfase_BPM",
                       "INS_Datum_uitschrijving")


## For analysis in Tableau, and/or calculation of other variables
vExtravariabelen <- c("INS_Faculteit",
                      "RES_Aantal_EC_cumulatief",
                      "RES_Aantal_EC_tm_P7",
                      "RES_Aantal_eindresultaten_tm_P7",
                      "RES_Aantal_herkansingen_tm_P7",
                      "RES_Aantal_no_shows_tm_P7",
                      "RES_Gem_resultaat_tm_nu",
                      "RES_Gem_resultaat_tm_jaar_1",
                      "RES_Gem_resultaat_tm_jaar_2",
                      "RES_Gem_resultaat_tm_jaar_3",
                      "RES_Gem_resultaat_tm_jaar_4",
                      "RES_Gem_resultaat_tm_jaar_5",
                      "RES_Gem_resultaat_tm_jaar_6",
                      "RES_Gem_resultaat_tm_jaar_7",
                      "RES_Gem_resultaat_tm_jaar_8")
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3. Modelleren ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

dfAS_full <- readrds_csv(dataloc='Tableau/205 VU-Inschrijvingen prognose 2021/Random Forest/Data/M/dfAS_prepped_m.rds')

dfAS_trainval <- dfAS_full %>%
  filter(INS_Inschrijvingsjaar < 2021,
         INS_Inschrijvingsjaar != 2019)

set.seed(10)
## Only use trainset for all models
vClass_weights <- rev(dfAS_trainval %>%
                        count(SUC_Type_uitstroom_studiejaar) %>%
                        pull(n) / nrow(dfAS_trainval))



rf_model <- rand_forest(mode = 'classification',
                          trees = tune(),
                          mtry = tune()) %>%
  set_engine('ranger',
             class.weights = vClass_weights,
             max.depth = tune('depth'),
             always.split.variables = c("OPL_Cluster"),
             num.threads = 6)



vRemovals <- c(
  "INS_Aantal_inschrijvingen_in_HO",
  "INS_Dagen_tussen_aanmelding_en_1_september",
  "VOP_Cijfer_gemiddeld_alfa",
  "VOP_Cijfer_gemiddeld_gamma",
  "VOP_Cijfer_gemiddeld_beta",
  "VOP_Gemiddelde_cijfer_cat",
  "VOP_Cijfer_wiskunde_alpha",
  "VOP_Cijfer_wiskunde_beta",
  "VOP_Cijfer_wiskunde_a",
  "VOP_Cijfer_wiskunde_b",
  "VOP_Cijfer_wiskunde_c",
  "VOP_Cijfer_wiskunde_d",
  "VOP_Cijfer_wiskunde",
  "RES_Gem_cijfer_voor_jaar",
  "RES_Gem_cijfer_voor_jaar_cat",
  "RES_Gem_resultaat_tm_voltooid_b",
  "INS_IO_Herkomst_NL",
  "VOP_Wiskundesoort",
  "INS_Vooropleiding_binnen_HO_sector"
)


vRemovalsTest <- c(
  "doorstroom_laag_bsa_2013",
  "uitval_2013",
  "gemEC_2013",
  "gemEC_2013_P2"
)

vRemovalsVal <- c(
  "doorstroom_laag_bsa_prev_year",
  "uitval_prev_year",
  "gemEC_prev_year",
  "gemEC_prev_year_P2"
)


rf_recipe_base <-
  recipe(
    SUC_Type_uitstroom_studiejaar ~ .,
    data = dfAS_trainval) %>%
  step_mutate_at(INS_Uitwonend, INS_Direct, INS_Tussenjaar_voor_M, fn = as.character) %>%
  ## Slow, but improves results. Use carefully (max two numerical columns, depending on amount of NAs).
  step_impute_bag(VOP_Cijfer_gemiddeld,
                  trees = 25, seed_val = 23) %>%
  step_novel(INS_Opleidingsnaam_2002,
             OPL_Cluster,
             OPL_Instructietaal,
             VOP_Wiskundesoort,
             INS_Vooropleiding_binnen_HO_sector,
             #INS_Hoogste_vooropleiding_naam,
             INS_Hoogste_vooropleiding_soort_cat
  ) %>%
  step_unknown(c(INS_Aansluiting_cat,
                 INS_Hoogste_vooropleiding_soort_cat,
                 INS_Direct,
                 INS_Tussenjaar_voor_M,
                 INS_Uitwonend,
                 INS_Vooropleiding_voor_HO_soort_SAP,
                 INS_Vooropleiding_voor_HO_profiel_standaard,
                 INS_Vooropleiding_voor_HO_VWO_examen_kans,
                 INS_Status_Zachte_knip,
                 Premaster_grade_cat,
                 INS_Vooropleiding_binnen_HO_sector,
                 OPL_Cluster,
                 VOP_Wiskundesoort,
                 VOP_Wiskundecijfer_cat,
                 VOP_Gemiddelde_cijfer_cat,
                 RES_Gem_cijfer_voor_jaar_cat,
                 RES_Gem_resultaat_tm_voltooid_b_cat)) %>%
  step_string2factor(all_nominal_predictors()) %>%
  step_rm(c(all_of(c(vRemovals, vFiltervariabelen, vExtravariabelen)), starts_with("WISK")))

rf_recipe_val <- rf_recipe_base #%>%
#step_rm(all_of(vRemovalsVal))

rf_recipe_test <- rf_recipe_base# %>%
# step_rm(all_of(vRemovalsTest))

rf_wflow_val <-
  workflow() %>%
  add_model(rf_model) %>%
  add_recipe(rf_recipe_base)


rf_wflow_test <-
  workflow() %>%
  add_model(rf_model) %>%
  add_recipe(rf_recipe_test)

# Train/val
keep_pred <- control_resamples(save_pred = TRUE)

# dfAS_trainval <- dfAS_trainval %>%
#   mutate(temp = OPL_Studielast_nominaal > 60)

set.seed(10)

rf_param <- rf_wflow_val %>%
  extract_parameter_set_dials() %>%
  update(trees = trees(c(3.9, 7), trans = log_trans()),
         depth = tree_depth(c(2, 5.3), trans = sqrt_trans()),
         mtry = mtry(c(1, 40))) %>%
  finalize(dfAS_voor_model)

gridsize <- 750
entropygrid <- grid_max_entropy(rf_param, size = gridsize)

set.seed(652)
roc_res <- metric_set(roc_auc)

data_folds <- vfold_cv(dfAS_trainval, v = 6, strata = SUC_Type_uitstroom_studiejaar)


rf_tune <-
  tune_grid(
    rf_wflow_val,
    data_folds,
    grid = entropygrid,
    metrics = roc_res
  )



paramresults <- show_best(rf_tune, n = gridsize) %>% select(-.estimator)
date <- gsub('-', '', Sys.Date())


saverds_csv(paramresults, paste0('gridsearch_results_m_', date), dataloc = 'Tableau/205 VU-Inschrijvingen prognose 2021/Random Forest/Data/M/', save_csv = TRUE)


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## BEWAAR & RUIM OP ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## Illustratie

# library(plotly)
#
# gridplot <- function(df, color = ''){
#   plot_ly(data = df,
#           x=~depth,
#           y=~mtry,
#           z=~trees,
#           type="scatter3d",
#           mode="markers",
#           alpha = 0.9,
#           size = 10,
#           alpha_stroke = 0.9,
#           color = color,
#           colors = 'Spectral'
#   )
# }
#
# gridsearched <- readrds_csv(dataloc = 'Tableau/205 VU-Inschrijvingen prognose 2021/Random Forest/Data/M/gridsearch_results_m_20230123.csv')
# gridplot(gridsearched, color = ~mean)

Clear_script_objects()
