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
                       "INS_Opleidingsfase_BPM",
                       "INS_Datum_uitschrijving")


## For analysis in Tableau, and/or calculation of other variables
vExtravariabelen <- c("RES_Aantal_EC_cumulatief",
                      "RES_Aantal_EC_tm_P7",
                      "RES_Aantal_eindresultaten_tm_P7",
                      "RES_Aantal_herkansingen_tm_P7",
                      "RES_Aantal_no_shows_tm_P7",
                      "RES_Gem_resultaat_tm_nu",
                      "INS_Eerste_jaar_opleiding_en_instelling",
                      "SUC_Diploma_nominaal")
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3. Modelleren ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

dfAS_full <- readrds_csv(dataloc = 'Tableau/205 VU-Inschrijvingen prognose 2021/Random Forest/Data/B2+/dfAS_prepped.rds')

dfAS_trainval <- dfAS_full %>%
  filter(INS_Inschrijvingsjaar < 2022,
         INS_Inschrijvingsjaar != 2019) %>%
  group_by(INS_Studentnummer) %>%
  slice_sample(n = 1) %>%
  ungroup()

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
             always.split.variables = c("INS_Opleidingsnaam_2002"),
             num.threads = 6)
vRemovals <- c(
  "INS_Aantal_inschrijvingen_jaar",
  "INS_Dubbele_studie_VU",
  "INS_Herinschrijving_jaar_2_na_uitschrijving_voor_1_feb_in_jaar_1",
  "INS_Hoofdneven",
  "INS_IO_Herkomst_EER_naam",
  "INS_Opleidingsvorm_naam",
  "INS_Opleiding_uitgeloot_naam",
  "MVR_Ingeschreven_voor_hoogste_interesse",
  "MVR_OBK_Keuzezekerheid",
  "OPL_BSA_EC_eis_aug_jr1",
  "VOP_Cijfer_gemiddeld_alfa",
  "VOP_Cijfer_gemiddeld_gamma",
  "VOP_Cijfer_gemiddeld_beta",
  "VOP_Cijfer_wiskunde_alpha",
  "VOP_Cijfer_wiskunde_beta",
  "VOP_Cijfer_wiskunde_a",
  "VOP_Cijfer_wiskunde_b",
  "VOP_Cijfer_wiskunde_c",
  "VOP_Cijfer_wiskunde_d",
  "INS_Datum_uitschrijving",
  "RES_Aantal_EC_voor_jaar",
  "INS_Verblijfsjaren_hoger_onderwijs_EJ",
  "RES_gem_EC_per_jaar",
  "RES_Gemiddeld_cijfer_diff",
  "RES_Aantal_herkansingen_voor_jaar",
  "RES_Aantal_vakken_cum",
  "RES_Aantal_no_shows_relative",
  "RES_Aantal_herkansingen_tm_jaar_1",
  "RES_Gem_resultaat_tm_jaar_1",
  "RES_Aantal_eindresultaten_voor_jaar",
  "RES_Aantal_vakken_niet_gehaald_relative",
  "RES_Aantal_EC_extracurriculair_cum"
)




rf_recipe_base <-
  recipe(
    SUC_Type_uitstroom_studiejaar ~ .,
    data = dfAS_trainval) %>%
  step_novel(INS_Opleidingsnaam_2002, OPL_Numerus_fixus_selectie, INS_Opleiding_uitgeloot_naam) %>%
  step_mutate_at(INS_Uitwonend, MVR_Ingeschreven_voor_hoogste_interesse, fn = as.character) %>%
  ## impute bag is too slow to use in grid search
  # step_impute_bag(VOP_Cijfer_gemiddeld, VOP_Cijfer_wiskunde,
  #                 trees = 25, seed_val = 23) %>%
  step_unknown(c(BSA_Status_EJ,
                 INS_Aansluiting_cat,
                 INS_Opleiding_uitgeloot_naam,
                 INS_Uitwonend,
                 INS_Vooropleiding_voor_HO_soort_SAP,
                 INS_Vooropleiding_voor_HO_profiel_standaard,
                 INS_Vooropleiding_voor_HO_VWO_examen_kans,
                 MVR_Ingeschreven_voor_hoogste_interesse,
                 # MVR_Studiesituatie_Studiekeuze,
                 # MVR_Studiesituatie_Ouders_universiteit,
                 OPL_Cluster,
                 OPL_Numerus_fixus_selectie)) %>%
  #step_ordinalscore(MVR_Studiesituatie_Ondersteuning_nodig) %>%
  step_string2factor(all_nominal_predictors()) %>%
  step_rm(c(all_of(c(vRemovals, vFiltervariabelen, vExtravariabelen))))

rf_wflow_val <-
  workflow() %>%
  add_model(rf_model) %>%
  add_recipe(rf_recipe_base)



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

gridsize <- 1000
entropygrid <- grid_max_entropy(rf_param, size = gridsize)

set.seed(652)
roc_res <- metric_set(roc_auc)

data_folds <- vfold_cv(dfAS_trainval, v = 8, strata = SUC_Type_uitstroom_studiejaar)


rf_tune <-
  tune_grid(
    rf_wflow_val,
    data_folds,
    grid = entropygrid,
    metrics = roc_res
  )



paramresults <- show_best(rf_tune, n = gridsize) %>% select(-.estimator)
date <- gsub('-', '', Sys.Date())


saverds_csv(paramresults, paste0('gridsearch_results_b2_', date), dataloc = 'Tableau/205 VU-Inschrijvingen prognose 2021/Random Forest/Data/B2+/', save_csv = TRUE)


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
