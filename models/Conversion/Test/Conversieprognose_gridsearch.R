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
# install.packages('tidymodels')
# install.packages("ranger")

library(ranger)
library(tidymodels)

dfAanmeldingen <- readrds_csv(output = "4. Analyses/Instroom komend jaar/Conversieprognose/Conversieprognose_prepped_data.rds")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3. Modelleren ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
gridsize <- 200

dfAanmeldingenB <- dfAanmeldingen %>%
  filter(INS_Opleidingsfase_BPM == "B",
         ## GNK is always full anyway, so no need to predict
         INS_Faculteit != "GNK",
         INS_Inschrijvingsjaar >= 2018)

data_foldsB <- group_vfold_cv(dfAanmeldingenB, group = INS_Inschrijvingsjaar)

## Factor order is FALSE, TRUE (so weights are reversed)
## Note: introduces more data leakage, as we calculate this for the whole set and not just for
## training set, due to cross validation. Impact is low though, so I don't care.
vClass_weightsB <- rev(dfAanmeldingenB %>%
                         count(Ingestroomd) %>%
                         pull(n) / nrow(dfAanmeldingenB))


rf_modelB <- rand_forest(mode = 'classification',
                        trees = tune(),
                        mtry = tune()) %>%
  set_engine('ranger',
             class.weights = vClass_weightsB,
             max.depth = tune('depth'),
             always.split.variables = 'INS_Opleidingsnaam_2002',
             num.threads = 4)


vRemovalsB <- c(
  "INS_Studentnummer",
  "AAN_Dagen_tot_1_sept",
  "DEM_Nationaliteit_EER_Naam",
  "INS_Opleidingsfase_BPM",
  "INS_Faculteit",
  "AAN_Datum",
  "INS_Inschrijvingsjaar",
  "Masteraanmelding_na_B_VU",
  "INS_Inschrijvingsjaar_EI",
  # "AAN_Status",
  # "AAN_Substatus",

  #"Conv_groep_lag_opl",
  "Conv_groep_lag_VU",
  "Conv_groep_lag_fac"
)



rf_recipeB <-
  recipe(
    Ingestroomd ~ .,
    data = dfAanmeldingenB) %>%
  step_novel(AAN_Substatus) %>%
  step_string2factor(all_nominal_predictors()) %>%
  step_unknown(c(AAN_Substatus, INS_Opleidingsnaam_2002, INS_Hoogste_vooropleiding_soort,
                 AAN_Soort_aanmelding)) %>%
  step_rm(all_of(vRemovalsB))

rf_wflowB <- workflow() %>%
  add_model(rf_modelB) %>%
  add_recipe(rf_recipeB)



rf_paramB <- rf_wflowB %>%
  extract_parameter_set_dials() %>%
  update(trees = trees(c(3.5, 5.5), trans = log_trans()),
         depth = tree_depth(c(4, 15)),
         mtry = mtry(c(1, 15))) %>%
  finalize(huh)


entropygrid <- grid_max_entropy(rf_paramB, size = gridsize)

set.seed(652)
roc_res <- metric_set(roc_auc)

rf_tuneB <-
  tune_grid(
    rf_wflowB,
    data_foldsB,
    grid = entropygrid,
    metrics = roc_res
  )



paramresultsB <- show_best(rf_tuneB, n = gridsize) %>% select(-.estimator)
date <- gsub('-', '', Sys.Date())
write_file(paramresultsB,
           paste0('gridsearch_results_B_', date),
           "4. Analyses/Instroom komend jaar/Conversieprognose/Grid search/",
           save_rds = TRUE)

## Master

dfAanmeldingenM <- dfAanmeldingen %>%
  filter(INS_Opleidingsfase_BPM == "M",
         INS_Inschrijvingsjaar >= 2018)

data_foldsM <- group_vfold_cv(dfAanmeldingenM, group = INS_Inschrijvingsjaar)

## Factor order is FALSE, TRUE (so weights are reversed)
## Note: introduces more data leakage, as we calculate this for the whole set and not just for
## training set, due to cross validation. Impact is low though, so I don't care.
vClass_weightsM <- rev(dfAanmeldingenM %>%
                         count(Ingestroomd) %>%
                         pull(n) / nrow(dfAanmeldingenM))


rf_modelM <- rand_forest(mode = 'classification',
                         trees = tune(),
                         mtry = tune()) %>%
  set_engine('ranger',
             class.weights = vClass_weightsM,
             max.depth = tune('depth'),
             always.split.variables = 'INS_Opleidingsnaam_2002',
             num.threads = 4)


vRemovalsM <- c(
  "INS_Studentnummer",
  "AAN_Dagen_tot_1_sept",
  "DEM_Nationaliteit_EER_Naam",
  "INS_Opleidingsfase_BPM",
  "AAN_Datum",
  "INS_Inschrijvingsjaar",
  "INS_Inschrijvingsjaar_EI",
  "Conv_groep_lag_VU",
  "Conv_groep_lag_fac"
)



rf_recipeM <-
  recipe(
    Ingestroomd ~ .,
    data = dfAanmeldingenM) %>%
  step_novel(AAN_Substatus) %>%
  step_string2factor(all_nominal_predictors()) %>%
  step_unknown(c(AAN_Substatus, INS_Opleidingsnaam_2002, INS_Hoogste_vooropleiding_soort,
                 AAN_Soort_aanmelding)) %>%
  step_rm(all_of(vRemovalsM))

rf_wflowM <- workflow() %>%
  add_model(rf_modelB) %>%
  add_recipe(rf_recipeB)



rf_paramM <- rf_wflowM %>%
  extract_parameter_set_dials() %>%
  update(trees = trees(c(3.5, 5.5), trans = log_trans()),
         depth = tree_depth(c(4, 15)),
         mtry = mtry(c(1, 15))) %>%
  finalize(huh)


entropygrid <- grid_max_entropy(rf_paramM, size = gridsize)

set.seed(652)
roc_res <- metric_set(roc_auc)

rf_tuneM <-
  tune_grid(
    rf_wflowM,
    data_foldsM,
    grid = entropygrid,
    metrics = roc_res
  )


paramresultsM <- show_best(rf_tuneM, n = gridsize) %>% select(-.estimator)
date <- gsub('-', '', Sys.Date())
write_file(paramresultsM,
           paste0('gridsearch_results_M_', date),
           "4. Analyses/Instroom komend jaar/Conversieprognose/Grid search/",
           save_rds = TRUE)
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## BEWAAR & RUIM OP ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## Illustratie

library(plotly)

gridplot <- function(df, color = ''){
  plot_ly(data = df,
          x = ~depth,
          y = ~mtry,
          z = ~trees,
          type = "scatter3d",
          mode = "markers",
          alpha = 0.9,
          size = 10,
          alpha_stroke = 0.9,
          color = color,
          colors = 'Spectral'
  )
}

#gridsearched <- readrds_csv(output = '4. Analyses/Instroom komend jaar/Conversieprognose/Grid search/gridsearch_results_B_20240422.rds')
#gridplot(gridsearched, color = ~mean)

clear_script_objects()
