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
                       "INS_Inschrijvingsjaar",
                       "INS_Opleidingsfase_BPM",
                       "INS_Datum_uitschrijving",
                       "INS_Hoofdneven")


## For analysis in Tableau, and/or calculation of other variables
vExtravariabelen <- c("RES_Aantal_EC_cumulatief",
                      "RES_Aantal_EC_tm_P7",
                      "RES_Aantal_eindresultaten_tm_P7",
                      "RES_Aantal_herkansingen_tm_P7",
                      "RES_Aantal_no_shows_tm_P7",
                      "RES_Gem_resultaat_tm_nu",
                      "INS_Eerste_jaar_opleiding_en_instelling",
                      "INS_IO_Herkomst_EER_naam",
                      "INS_Opleidingsvorm_naam",
                      "OPL_BSA_EC_eis_aug_jr1",
                      "SUC_Diploma_nominaal",
                      "VOP_Cijfer_gemiddeld_beta",
                      "VOP_Cijfer_wiskunde_alpha",
                      "VOP_Cijfer_wiskunde_beta",
                      "VOP_Cijfer_wiskunde_a",
                      "VOP_Cijfer_wiskunde_b",
                      "VOP_Cijfer_wiskunde_c",
                      "VOP_Cijfer_wiskunde_d")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3. Modelleren ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

set.seed(10)


vRemovals <- c(
  "RES_Aantal_vakken_cum"
)

vRemovalsTest <- c(
)




fit_modellen <- function(bestandspad, periode) {
  print(periode)
  gc()
  dfAS_full <- readrds_csv(output = bestandspad)
  dfAS_trainval <- dfAS_full %>%
    filter(INS_Inschrijvingsjaar %in% c(2018, 2020:2022))

  data_folds <- group_vfold_cv(dfAS_trainval, group = INS_Inschrijvingsjaar)

  vClass_weights <- rev(dfAS_trainval %>%
                          count(SUC_Type_uitstroom_studiejaar) %>%
                          pull(n) / nrow(dfAS_trainval))


  rf_model <- rand_forest(mode = 'classification',
                          trees = tune(),
                          mtry = tune()) %>%
    set_engine('ranger',
               class.weights = vClass_weights,
               max.depth = tune('depth'),
               always.split.variables = 'INS_Opleidingsnaam_2002',
               num.threads = 4)

  rf_recipe_base <-
    recipe(
      SUC_Type_uitstroom_studiejaar ~ .,
      data = dfAS_trainval) %>%
    step_novel(INS_Opleidingsnaam_2002, OPL_Numerus_fixus_selectie, OPL_VU_alfa_beta_gamma) %>%
    step_mutate_at(INS_Uitwonend, fn = as.character) %>%
    step_impute_bag(VOP_Cijfer_gemiddeld, VOP_Cijfer_wiskunde,
                    trees = 25, seed_val = 23) %>%
    step_unknown(c(BSA_Status_EJ,
                   INS_Aansluiting_cat,
                   INS_Uitwonend,
                   INS_Vooropleiding_voor_HO_soort_SAP,
                   INS_Vooropleiding_voor_HO_profiel_standaard,
                   INS_Vooropleiding_voor_HO_VWO_examen_kans,
                   #INS_Hoogste_vooropleiding_soort_cat,
                   OPL_Cluster,
                   OPL_Numerus_fixus_selectie,
                   OPL_VU_alfa_beta_gamma)) %>%
    #step_ordinalscore(MVR_Studiesituatie_Ondersteuning_nodig) %>%
    step_string2factor(all_nominal_predictors()) %>%
    step_rm(c(all_of(c(vRemovals, vFiltervariabelen, vExtravariabelen))))


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
  set.seed(10)
  gridsize <- 400
  entropygrid <- grid_max_entropy(rf_param, size = gridsize)
  print(entropygrid)
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
  saverds_csv(paramresults, paste0('gridsearch_results_B2+_', periode, "_", date),
              output = '4. Analyses/Doorstroomprognose/Data/Gridsearch/', save_csv = TRUE)
}

lPrepped_data_paden <- c(
  '4. Analyses/Doorstroomprognose/Data/Geprepareerd/B2/dfAS_prepped_B2+_P2_20240430.rds',
  '4. Analyses/Doorstroomprognose/Data/Geprepareerd/B2/dfAS_prepped_B2+_P3_20240430.rds',
  '4. Analyses/Doorstroomprognose/Data/Geprepareerd/B2/dfAS_prepped_B2+_P4_20240430.rds'
)

lPeriodes <- c("P2", "P3", "P4")


walk2(lPrepped_data_paden, lPeriodes, fit_modellen)

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
