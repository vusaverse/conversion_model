vTest_years <- 2023
vTrain_years <- c(2018, 2020, 2021, 2022)

## Years solely used for feature engineering, not training. Should be the two years prior to training
vFeature_creation_years <- min(vTrain_years) - 2:1


Dates <- read_file_proj("ACA_Dates", dir = "2. Geprepareerde data")
## Date of prognosis
peildatum <- today()

dfPeilperiode <- Dates %>%
  filter(ACA_Peildatum_invoer < peildatum) %>%
  filter(ACA_Peildatum_invoer == max(ACA_Peildatum_invoer))

peilperiode <- dfPeilperiode %>%
  pull(ACA_Periode)

peildatum_beoordeling <- dfPeilperiode %>%
  pull(ACA_Peildatum)

peildatum_beoordeling_md_str <- str_sub(peildatum_beoordeling, 6)


## Gridsearched hyperparameters
dfHyperparameters <- read_file_proj("Hyperparameters_config", dir = "4. Analyses/Doorstroomprognose/Gridsearch/", extension = "csv")

dfHyperparameters <- dfHyperparameters %>%
  filter(Periode == peilperiode,
         Model == model)

mtry = dfHyperparameters %>% pull(mtry)
trees = dfHyperparameters %>% pull(trees)
depth = dfHyperparameters %>% pull(depth)
always_split_opl = ifelse(dfHyperparameters %>% pull(always_split_opl), "INS_Opleidingsnaam_2002", NA_character_)
