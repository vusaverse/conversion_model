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

## Variables included in model (some removed later)
vModelvariabelen <- c("DEM_Geslacht",
                      "DEM_Leeftijd_peildatum_1_oktober",
                      "INS_Aansluiting_cat",
                      "INS_Aantal_inschrijvingen_in_HO",
                      "INS_Dagen_tussen_aanmelding_en_1_september",
                      "INS_Dubbele_studie_VU",
                      "INS_Hoofdneven",
                      "INS_IO_Herkomst_EER_naam",
                      "INS_Opleiding_uitgeloot_ja_nee",
                      "INS_Opleidingsnaam_2002",
                      "INS_Opleidingsvorm_naam",
                      "INS_Opleiding_uitgeloot_naam",
                      "INS_Soort_eerstejaars",
                      "INS_Uitwonend",
                      "INS_Vooropleiding_voor_HO_soort_SAP",
                      "INS_Vooropleiding_voor_HO_profiel_standaard",
                      "INS_Vooropleiding_voor_HO_VWO_examen_kans",
                      "MVR_AV_Studiedoelen_stellen",
                      "MVR_AV_Studieplanning",
                      "MVR_AV_Zelfdiscipline",
                      "MVR_Studiesituatie_Ouders_universiteit",
                      "MVR_OBK_Vertrouwen_studiesucces",
                      "MVR_Score_ingeschreven_opleiding",
                      "MVR_Studiesituatie_Ondersteuning_nodig",
                      "MVR_Studiesituatie_Studiekeuze",
                      "MVR_Studiesituatie_Reistijd_minuten",
                      "MVR_Ingeschreven_voor_hoogste_interesse",
                      "MVR_OBK_Keuzezekerheid",
                      "OPL_BSA_EC_eis_aug_jr1",
                      "OPL_VU_alfa_beta_gamma",
                      "OPL_Cluster",
                      "OPL_Numerus_fixus_selectie",
                      "ORI_Orientatie_komt_voor",
                      "PUC_Deelgenomen",
                      "SUC_Uitval_na_jaar_1_cohorten",
                      "VOP_Cijfer_gemiddeld",
                      "VOP_Cijfer_gemiddeld_alfa",
                      "VOP_Cijfer_gemiddeld_gamma",
                      "VOP_Cijfer_gemiddeld_beta",
                      "VOP_Cijfer_wiskunde",
                      "VOP_Cijfer_wiskunde_alpha",
                      "VOP_Cijfer_wiskunde_beta",
                      "VOP_Cijfer_wiskunde_a",
                      "VOP_Cijfer_wiskunde_b",
                      "VOP_Cijfer_wiskunde_c",
                      "VOP_Cijfer_wiskunde_d")

## Variables for filtering students
vFiltervariabelen <- c("INS_Studentnummer",
                       "INS_Studiejaar",
                       "INS_Inschrijvingsjaar",
                       "INS_Opleidingsfase_BPM",
                       "INS_Uitschrijving_voor_1_feb")

## For analysis in Tableau
vExtravariabelen <- c('INS_Faculteit',
                      'RES_Aantal_EC_tm_jaar_1',
                      'RES_Aantal_herkansingen_tm_jaar_1')

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3. Modelleren ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

dfAS_full <- readrds_csv(dataloc='Tableau/205 VU-Inschrijvingen prognose 2021/Random Forest/Data/dfAS_prepped.rds')

dfAS_trainval <- dfAS_full %>%
  filter(INS_Inschrijvingsjaar < 2018)

set.seed(10)

data_folds <- vfold_cv(dfAS_trainval, v = 10, strata = SUC_Uitval_na_jaar_1_cohorten)

## Pipe into rev does not work for some reason
vClass_weights <- rev(dfAS_trainval %>%
  count(SUC_Uitval_na_jaar_1_cohorten) %>%
  pull(n) / nrow(dfAS_trainval))


rf_model_hellinger <- rand_forest(mode = 'classification',
                        trees = tune(),
                        mtry = tune()) %>%
  set_engine('ranger',
             class.weights = vClass_weights,
             max.depth = tune('depth'),
             always.split.variables = 'INS_Opleidingsnaam_2002',
             num.threads = 6,
             splitrule = 'hellinger')

rf_model <- rand_forest(mode = 'classification',
                          trees = tune(),
                          mtry = tune()) %>%
  set_engine('ranger',
             class.weights = vClass_weights,
             max.depth = tune('depth'),
             always.split.variables = 'INS_Opleidingsnaam_2002',
             num.threads = 6)


vRemovals <- c(
  "MVR_AV_Studiedoelen_stellen",
  "MVR_AV_Studieplanning",
  "MVR_AV_Zelfdiscipline",
  "MVR_OBK_Vertrouwen_studiesucces",
  "MVR_Studiesituatie_Ouders_universiteit",
  "MVR_Studiesituatie_Reistijd_minuten",
  "VOP_Cijfer_wiskunde_alpha",
  "VOP_Cijfer_wiskunde_beta",
  "VOP_Cijfer_gemiddeld_beta",
  "VOP_Cijfer_wiskunde_a",
  "VOP_Cijfer_wiskunde_b",
  "VOP_Cijfer_wiskunde_c",
  "VOP_Cijfer_wiskunde_d"
)

vRemovalsVal <- c(
  'doorstroom_laag_bsa_prev_year',
  'uitval_prev_year',
  'gemEC_prev_year'
)


rf_recipe_base <-
  recipe(
    SUC_Uitval_na_jaar_1_cohorten ~ .,
    data = dfAS_trainval) %>%
  step_mutate_at(INS_Uitwonend, MVR_Ingeschreven_voor_hoogste_interesse, fn = as.character) %>%
  step_novel(INS_Opleidingsnaam_2002, OPL_Numerus_fixus_selectie) %>%
  step_unknown(c(INS_Aansluiting_cat,
                 INS_Uitwonend,
                 INS_Vooropleiding_voor_HO_soort_SAP,
                 INS_Vooropleiding_voor_HO_profiel_standaard,
                 INS_Vooropleiding_voor_HO_VWO_examen_kans,
                 MVR_Ingeschreven_voor_hoogste_interesse,
                 MVR_Studiesituatie_Studiekeuze,
                 MVR_Studiesituatie_Ouders_universiteit,
                 OPL_Cluster,
                 OPL_Numerus_fixus_selectie,
                 INS_Opleidingsnaam_2002)) %>%
  step_ordinalscore(MVR_Studiesituatie_Ondersteuning_nodig) %>%
  step_string2factor(all_nominal_predictors()) %>%
  step_rm(c(all_of(c(vRemovals, vFiltervariabelen, vExtravariabelen)), starts_with('WISK')))

rf_recipe_val <- rf_recipe_base %>%
  step_rm(all_of(vRemovalsVal))


rf_wflow_hellinger_val <-
  workflow() %>%
  add_model(rf_model_hellinger) %>%
  add_recipe(rf_recipe_val)

rf_wflow_val <-
  workflow() %>%
  add_model(rf_model_hellinger) %>%
  add_recipe(rf_recipe_val)

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

rf_tune <-
  tune_grid(
    rf_wflow_val,
    data_folds,
    grid = entropygrid,
    metrics = roc_res
  )



paramresults <- show_best(rf_tune, n = gridsize) %>% select(-.estimator)
date <- gsub('-', '', Sys.Date())
saverds_csv(paramresults, paste0('gridsearch_results_', date), dataloc = 'Tableau/205 VU-Inschrijvingen prognose 2021/Random Forest/Data/', save_csv = TRUE)

rf_tune_hellinger <-
  tune_grid(
    rf_wflow_hellinger_val,
    data_folds,
    grid = entropygrid,
    metrics = roc_res
  )

paramresults_hellinger <- show_best(rf_tune_hellinger, n = gridsize) %>% select(-.estimator)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## BEWAAR & RUIM OP ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#saverds_csv(rf_tune, paste0('gridsearch_results_tune_grid_', date), dataloc = 'Tableau/205 VU-Inschrijvingen prognose 2021/Random Forest/Data/')
saverds_csv(paramresults_hellinger, paste0('gridsearch_results_hellinger', date), dataloc = 'Tableau/205 VU-Inschrijvingen prognose 2021/Random Forest/Data/', save_csv = TRUE)


## Illustratie

library(plotly)

gridplot <- function(df, color = ''){
  plot_ly(data = df,
          x=~depth,
          y=~mtry,
          z=~trees,
          type="scatter3d",
          mode="markers",
          alpha = 0.9,
          size = 10,
          alpha_stroke = 0.9,
          color = color,
          colors = 'Spectral'
  )
}

gridsearched <- readrds_csv(dataloc = 'Tableau/205 VU-Inschrijvingen prognose 2021/Random Forest/Data/gridsearch_results_20221011.csv')
gridplot(gridsearched, color = ~mean)

Clear_script_objects()
