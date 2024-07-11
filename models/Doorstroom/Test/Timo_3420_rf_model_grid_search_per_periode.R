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
library(ranger)
library(tidymodels)

## Variables for filtering students
vFiltervariabelen <- c("INS_Studentnummer",
                       "INS_Studiejaar",
                       "INS_Inschrijvingsjaar",
                       "INS_Opleidingsfase_BPM",
                       "INS_Uitschrijving_voor_1_feb")

## For analysis in Tableau or feature engineering
vExtravariabelen <- c("INS_Faculteit",
                      "RES_Aantal_EC_tm_jaar_1",
                      "RES_Aantal_herkansingen_tm_jaar_1",
                      "INS_IO_Herkomst_EER_naam")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3. Modelleren ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

set.seed(10)


vRemovals <- c(
  "INS_IO_Herkomst_EER_naam",
  "MVR_OBK_Keuzezekerheid",
  "VOP_Cijfer_wiskunde_alpha",
  "VOP_Cijfer_wiskunde_beta",
  "VOP_Cijfer_gemiddeld_beta",
  "VOP_Cijfer_wiskunde_a",
  "VOP_Cijfer_wiskunde_b",
  "VOP_Cijfer_wiskunde_c",
  "VOP_Cijfer_wiskunde_d",

  "VOP_School_reistijd_OV_VU",
  "MVR_Studiesituatie_Ouders_universiteit",
  "MVR_Studiesituatie_Financien_belemmering",
  "MVR_Studiesituatie_Ondersteuning_nodig",
  "MVR_Studiesituatie_Goede_werkplek",
  "MVR_Studiesituatie_Zorgtaak_belemmering",

  "VOP_School_provincie_vestiging",
  "VOP_School_gemeente_vestiging",
  "VOP_School_Amsterdam_eo"
)

vRemovalsTest <- c(
  "gemEC_2013",
  "gemEC_2013_P2",
  "uitval_2013",
  "doorstroom_laag_bsa_2013"
)




fit_modellen <- function(bestandspad, periode) {
  print(periode)

  dfAS_full <- readrds_csv(dataloc = bestandspad)
  dfAS_trainval <- dfAS_full %>%
    filter(INS_Inschrijvingsjaar <= 2022)

  data_folds <- group_vfold_cv(dfAS_trainval, group = INS_Inschrijvingsjaar)

  vClass_weights <- rev(dfAS_trainval %>%
                          count(SUC_Uitval_na_jaar_1_herberekend) %>%
                          pull(n) / nrow(dfAS_trainval))


  rf_model <- rand_forest(mode = 'classification',
                          trees = tune(),
                          mtry = tune()) %>%
    set_engine('ranger',
               class.weights = vClass_weights,
               max.depth = tune('depth'),
               #always.split.variables = 'INS_Opleidingsnaam_2002',
               num.threads = 4,
               splitrule = 'hellinger')

  rf_recipe_base <-
    recipe(
      SUC_Uitval_na_jaar_1_herberekend ~ .,
      data = dfAS_trainval) %>%
    step_rm(c(all_of(c(vRemovals, vFiltervariabelen, vExtravariabelen)), starts_with("WISK"))) %>%
    step_mutate_at(INS_Uitwonend, VOP_School_NH, OPL_Numerus_fixus_selectie, fn = as.character) %>%
    step_novel(INS_Opleidingsnaam_2002, OPL_Numerus_fixus_selectie, INS_Hoogste_vooropleiding_soort_cat) %>%
    ## Impute bag is very slow, so use wisely in development
    step_impute_bag(VOP_Cijfer_gemiddeld, VOP_Cijfer_wiskunde, seed_val = 23) %>%
    step_unknown(c(INS_Aansluiting_cat,
                   #INS_Opleiding_uitgeloot_naam,
                   INS_Uitwonend,
                   INS_Vooropleiding_voor_HO_soort_SAP,
                   INS_Vooropleiding_voor_HO_profiel_standaard,
                   INS_Vooropleiding_voor_HO_VWO_examen_kans,
                   INS_Hoogste_vooropleiding_soort_cat,
                   OPL_Cluster,
                   OPL_VU_alfa_beta_gamma,
                   OPL_Numerus_fixus_selectie,
                   MVR_AI_Keuzezekerheid,
                   MVR_Studiesituatie_Omgeving_hulp,
                   VOP_School_NH)) %>%
    step_ordinalscore(MVR_AI_Keuzezekerheid) %>%
    step_string2factor(all_nominal_predictors())


  rf_recipe_test <- rf_recipe_base %>%
    step_rm(all_of(vRemovalsTest))

  rf_wflow_test <-
    workflow() %>%
    add_model(rf_model) %>%
    add_recipe(rf_recipe_test)

  rf_param <- rf_wflow_test %>%
    extract_parameter_set_dials() %>%
    update(trees = trees(c(3.9, 6.8), trans = log_trans()),
           depth = tree_depth(c(2, 5.3), trans = sqrt_trans()),
           mtry = mtry(c(1, 40))) %>%
    finalize(dfAS_voor_model)

  gridsize <- 350
  entropygrid <- grid_max_entropy(rf_param, size = gridsize)

  set.seed(652)
  roc_res <- metric_set(roc_auc)

  rf_tune <-
    tune_grid(
      rf_wflow_test,
      data_folds,
      grid = entropygrid,
      metrics = roc_res
    )


  paramresults <- show_best(rf_tune, n = gridsize) %>% select(-.estimator)

  date <- gsub('-', '', Sys.Date())
  saverds_csv(paramresults, paste0('gridsearch_results_geen_always_split_', periode, "_", date),
              dataloc = 'Tableau/205 VU-Inschrijvingen prognose 2021/Random Forest/Data/', save_csv = TRUE)
}

lPrepped_data_paden <- c(
  'Tableau/205 VU-Inschrijvingen prognose 2021/Random Forest/Data/dfAS_prepped_P2_20240429.rds',
  'Tableau/205 VU-Inschrijvingen prognose 2021/Random Forest/Data/dfAS_prepped_P3_20240429.rds',
  'Tableau/205 VU-Inschrijvingen prognose 2021/Random Forest/Data/dfAS_prepped_P4_20240429.rds'
)

lPeriodes <- c("P2", "P3", "P4")


map2(lPrepped_data_paden, lPeriodes, fit_modellen)

#paramresults_hellinger <- show_best(rf_tune_hellinger, n = gridsize) %>% select(-.estimator)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## BEWAAR & RUIM OP ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#saverds_csv(rf_tune, paste0('gridsearch_results_tune_grid_', date), dataloc = 'Tableau/205 VU-Inschrijvingen prognose 2021/Random Forest/Data/')
#saverds_csv(paramresults_hellinger, paste0('gridsearch_results_hellinger', date), dataloc = 'Tableau/205 VU-Inschrijvingen prognose 2021/Random Forest/Data/', save_csv = TRUE)


# ## Illustratie
# #
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
# gridsearched <- readrds_csv(dataloc = 'Tableau/205 VU-Inschrijvingen prognose 2021/Random Forest/Data/gridsearch_results_P4_20240430.rds')
# gridplot(gridsearched, color = ~mean)

clear_script_objects()
