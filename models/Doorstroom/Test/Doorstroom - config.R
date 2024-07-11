vTest_years <- 2023
vTrain_years <- c(2018, 2020, 2021, 2022)

## Gridsearched hyperparameters
dfHyperparameters <- readrds_csv(output = "4. Analyses/Doorstroomprognose/Gridsearch/Hyperparameters_config.csv")

dfHyperparameters <- dfHyperparameters %>%
  filter(Periode == peilperiode,
         Model == model)

mtry = dfHyperparameters %>% pull(mtry)
trees = dfHyperparameters %>% pull(trees)
depth = dfHyperparameters %>% pull(depth)
always_split_opl = ifelse(dfHyperparameters %>% pull(always_split_opl), "INS_Opleidingsnaam_2002", NA_character_)
