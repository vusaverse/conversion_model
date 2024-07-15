## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code voor Education Analytics Vrije Universiteit Amsterdam
## Copyright 2024 VU
## Web Page: http://www.vu.nl
## Contact: vu-analytics@vu.nl
## Verspreiding buiten de VU: Ja
##
##' *INFO*:
## 1) ___
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

nTest_year = vvconverter::academic_year(lubridate::today()) + 1

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. INLEZEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(tidymodels)

vColumns <- c(
  "INS_Inschrijvingsjaar",
  "INS_Opleidingsfase_BPM",
  "INS_Opleidingsnaam_2002",
  "INS_Studentnummer",
  "INS_Studiejaar",
  "INS_Indicatie_actief_op_peildatum_status",
  "INS_Soort_eerstejaars",
  "INS_Hoofdneven"
)

dfAS_raw <- get_analysisset(columns = vColumns)

dfAanmeldingen <- read_file_proj("dfAanmeldingen_geprepareerd",
                dir = "4. Analyses/Instroom komend jaar/Conversieprognose/Geprepareerd/")


## OPLAS for NF and language data, TODO check for more features
dfOpleidingen_raw <- read_file_proj("OPLAS_VU", base_dir = Sys.getenv("NETWORK_DIR"),
                                    add_branch = FALSE,
                                    dir = "Output/_REPOSITORIES/opleidingen-analyseset/main/3. Analyseset/") %>%
  select(INS_Opleidingsnaam_2002, INS_Inschrijvingsjaar, OPL_Numerus_fixus_selectie, OPL_Numerus_fixus_selectie_capaciteit_max,
         OPL_Instructietaal) %>%
  distinct() %>%
  filter(INS_Inschrijvingsjaar <= 2022) %>%
  filter(row_number() == 1, .by = c(INS_Inschrijvingsjaar, INS_Opleidingsnaam_2002))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. BEWERKEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

vAggregatieniveau = c("INS_Studentnummer", "INS_Opleidingsnaam_2002", "INS_Opleidingsfase_BPM",
                      "INS_Inschrijvingsjaar")

dfOpleidingen <- dfOpleidingen_raw %>%
  mutate(
    ## Zet capaciteit om naar numeriek en maak van character waarden NA's
    ## suppreswarnings omdat de str_detect een warning geeft bij NA
    OPL_Numerus_fixus_selectie_capaciteit_max =  suppressWarnings(ifelse(
      str_detect(OPL_Numerus_fixus_selectie_capaciteit_max, "[0-9]"),
      as.numeric(OPL_Numerus_fixus_selectie_capaciteit_max),
      NA_integer_)),
    OPL_Numerus_fixus_selectie = !is.na(OPL_Numerus_fixus_selectie_capaciteit_max))


## Opleidingen in 2023 and 2024 are missing NF data still, so create new rows
vNieuwe_NF <- c("B Gezondheid en Leven", "B Computer Science")

for (extra_jaar in (max(dfOpleidingen$INS_Inschrijvingsjaar) + 1) : (vvconverter::academic_year(lubridate::today()) + 1)) {
  dfOpleidingen <- dfOpleidingen %>%
    filter(INS_Inschrijvingsjaar == max(INS_Inschrijvingsjaar)) %>%
    mutate(INS_Inschrijvingsjaar = extra_jaar,
           OPL_Numerus_fixus_selectie = if_else(INS_Opleidingsnaam_2002 %in% vNieuwe_NF,
                                                TRUE, OPL_Numerus_fixus_selectie)) %>%
    rbind(dfOpleidingen)
}

## Determine ingestroomde studenten
dfAS <- dfAS_raw %>%
  filter(INS_Studiejaar == 1,
         INS_Indicatie_actief_op_peildatum_status == "actief",
         INS_Hoofdneven == "Hoofdinschrijving") %>%
  mutate(Ingestroomd = TRUE) %>%
  select(INS_Inschrijvingsjaar, INS_Studentnummer, INS_Opleidingsnaam_2002, INS_Opleidingsfase_BPM,
         Ingestroomd)

## Feature creation
dfAanmeldingen <- dfAanmeldingen %>%
  ## Add dates in 2023-2024 format (necessary for leap year) to be able to compare dates
  mutate(AAN_Datum_aangepast = case_when(
    month(AAN_Datum) > 9 ~ `year<-`(AAN_Datum, 2023),
    TRUE ~ `year<-`(AAN_Datum, 2024)
  )) %>%
  mutate(dag_aanmelding = as.numeric(as.Date("2024-09-01") - AAN_Datum_aangepast)) %>%
  mutate(Masteraanmelding_na_B_VU = INS_Opleidingsfase_BPM == "M" &
           INS_Studentnummer %in% (dfAS %>% filter(INS_Opleidingsfase_BPM == "B") %>%
                                     pull(INS_Studentnummer))) %>%
  mutate(nAanmeldingen_VU_gelijke_fase = n(), .by = c(INS_Studentnummer, INS_Opleidingsfase_BPM,
                                                      INS_Inschrijvingsjaar)) %>%
  mutate(Buitenland = INS_Hoogste_vooropleiding_soort == "buitenland",
         ## Aanmeldingen from April onwards have high coversion
         Aanmelding_vanaf_april = month(AAN_Datum) %in% c(4, 5, 6),
         deadlineaanmelder = case_when(
           INS_Opleidingsfase_BPM == "B" & AAN_Datum_aangepast %in% c(as.Date("2024-04-30"),
                                                                      as.Date("2024-05-01")) ~ TRUE,

           INS_Opleidingsfase_BPM == "M" & AAN_Datum_aangepast %in% c(as.Date("2024-05-31"),
                                                                      as.Date("2024-06-01")) ~ TRUE,
           .default = FALSE)) %>%
  rowwise() %>%
  ## Some students have higher EI year than Inschrijvingsjaar, should not happen (also causes data leakage)
  mutate(INS_Inschrijvingsjaar_EI = min(INS_Inschrijvingsjaar_EI, INS_Inschrijvingsjaar)) %>%
  ungroup() %>%
  mutate(INS_Inschrijvingsjaar_EI = coalesce(INS_Inschrijvingsjaar_EI, INS_Inschrijvingsjaar),
         INS_Inschrijvingsjaar_EI_diff = INS_Inschrijvingsjaar - INS_Inschrijvingsjaar_EI)


## Add AS and opleidingen data, and create features from these
dfAanmeldingen <- dfAanmeldingen %>%
  left_join(dfAS, by = c("INS_Studentnummer", "INS_Opleidingsfase_BPM", "INS_Opleidingsnaam_2002",
                         "INS_Inschrijvingsjaar"), relationship = "one-to-one") %>%
  left_join(dfOpleidingen, by = c("INS_Opleidingsnaam_2002", "INS_Inschrijvingsjaar"), relationship = "many-to-one") %>%
  mutate(Ingestroomd = replace_na(Ingestroomd, FALSE),
         Ingestroomd = as.factor(Ingestroomd),
         INS_Opleidingsvorm = ifelse(INS_Opleidingsvorm == "Voltijd", "voltijd", "deeltijd")) %>%
  mutate(Aangemeld_voor_VU_NF = any(OPL_Numerus_fixus_selectie, na.rm = TRUE),
         Afgewezen_voor_NF = any(OPL_Numerus_fixus_selectie & AAN_Substatus == "Afgekeurd door VU", na.rm = TRUE),
         .by = c(INS_Studentnummer, INS_Inschrijvingsjaar))

## Historical conversions
sGroep <- c("Buitenland", "INS_Opleidingsvorm", "INS_Opleidingsfase_BPM")

Conversies_per_groep_lagged <- function(df, groep, min_aantal = 0, suffix = NULL) {
  ## Calculates the conversions of the defined group in the previous year.
  df %>%
    summarise("Conv_groep{suffix}" := ifelse(n() >= min_aantal, sum(Ingestroomd == TRUE) / n(), NA_real_),
              .by = c(INS_Inschrijvingsjaar, all_of(groep))) %>%
    arrange(INS_Inschrijvingsjaar) %>%
    mutate("Conv_groep_lag{suffix}" := dplyr::lag(!!sym(paste0("Conv_groep", suffix)), n = 1),
           .by = all_of(groep)) %>%
    select(-!!sym(paste0("Conv_groep", suffix)))
}

dfConversies_per_groep_opl <- dfAanmeldingen %>%
  Conversies_per_groep_lagged(c(sGroep, "INS_Opleidingsnaam_2002"), min_aantal = 15, suffix = "_opl")
dfConversies_per_groep_fac <- dfAanmeldingen %>%
  Conversies_per_groep_lagged(c(sGroep, "INS_Faculteit"), min_aantal = 15, suffix = "_fac")
dfConversies_per_groep_VU <- dfAanmeldingen %>%
  Conversies_per_groep_lagged(c(sGroep), suffix = "_VU")


dfAanmeldingen <- dfAanmeldingen %>%
  left_join(dfConversies_per_groep_opl, by = c(sGroep, "INS_Opleidingsnaam_2002", "INS_Inschrijvingsjaar"),
            relationship = "many-to-one") %>%
  left_join(dfConversies_per_groep_fac, by = c(sGroep, "INS_Faculteit", "INS_Inschrijvingsjaar"),
            relationship = "many-to-one") %>%
  left_join(dfConversies_per_groep_VU, by = c(sGroep, "INS_Inschrijvingsjaar"),
            relationship = "many-to-one")

## Fill missing data
dfAanmeldingen <- dfAanmeldingen %>%
  mutate(OPL_Numerus_fixus_selectie = replace_na(OPL_Numerus_fixus_selectie, FALSE),
         OPL_Instructietaal = replace_na(OPL_Instructietaal, "Nederlands"),
         ## Use "bigger"group if not enough aanmeldingen in current group
         Conv_groep_lag_fac = coalesce(Conv_groep_lag_fac, Conv_groep_lag_VU),
         Conv_groep_lag_opl = coalesce(Conv_groep_lag_opl, Conv_groep_lag_fac))

## Joint degrees are weird
dfAanmeldingen <- dfAanmeldingen %>%
  filter(str_detect(INS_Opleidingsnaam_2002, "joint degree", negate = TRUE))

###########################################################
## Prognose - Bachelor
###########################################################

dfAanmeldingenB <- dfAanmeldingen %>%
  filter(INS_Opleidingsfase_BPM == "B",
         ## GNK is always full anyway, so no need to predict
         INS_Faculteit != "GNK",
         INS_Inschrijvingsjaar >= 2018)

dfAanmeldingenB_train <- dfAanmeldingenB %>%
  filter(INS_Inschrijvingsjaar != nTest_year)
dfAanmeldingenB_test <- dfAanmeldingenB %>%
  filter(INS_Inschrijvingsjaar == nTest_year)

## Factor order is FALSE, TRUE (so weights are reversed)
vClass_weightsB <- rev(dfAanmeldingenB_train %>%
                         count(Ingestroomd) %>%
                         pull(n) / nrow(dfAanmeldingenB_train))

rf_modelB <- rand_forest(mode = "classification",
                         trees = 219,
                         mtry = 5) %>%
  set_engine("ranger",
             class.weights = vClass_weightsB,
             max.depth = 11,
             always.split.variables = "INS_Opleidingsnaam_2002",
             ## Documentation lies, uses only 1 thread by default
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
  "Conv_groep_lag_VU",
  "Conv_groep_lag_fac",
  "Test_set",
  "OPL_Numerus_fixus_selectie_capaciteit_max"
)


rf_recipeB <-
  recipe(
    Ingestroomd ~ .,
    data = dfAanmeldingenB_train) %>%
  step_novel(AAN_Substatus) %>%
  step_string2factor(all_nominal_predictors()) %>%
  step_unknown(c(AAN_Substatus, INS_Opleidingsnaam_2002, INS_Hoogste_vooropleiding_soort,
                 AAN_Soort_aanmelding)) %>%
  step_rm(all_of(vRemovalsB))

rf_wflowB <- workflow() %>%
  add_model(rf_modelB) %>%
  add_recipe(rf_recipeB)

set.seed(652)
rf_fitB <- fit(rf_wflowB, dfAanmeldingenB_train)

dfResultB <- rf_fitB %>%
  predict(dfAanmeldingenB_test, type = "class") %>%
  cbind(predict(rf_fitB, dfAanmeldingenB_test, type = "prob"))

dfCombined_testB <- dfAanmeldingenB_test %>%
  select(INS_Studentnummer, INS_Opleidingsnaam_2002, INS_Inschrijvingsjaar) %>%
  cbind(dfResultB)

dfOutputB <- dfAanmeldingenB %>%
  left_join(dfCombined_testB, by = c("INS_Studentnummer", "INS_Opleidingsnaam_2002",
                                     "INS_Inschrijvingsjaar"), relationship = "one-to-one")

###########################################################
## Prognose - Master
###########################################################

dfAanmeldingenM <- dfAanmeldingen %>%
  filter(INS_Opleidingsfase_BPM == "M",
         INS_Inschrijvingsjaar >= 2018)

dfAanmeldingenM_train <- dfAanmeldingenM %>%
  filter(INS_Inschrijvingsjaar != nTest_year)
dfAanmeldingenM_test <- dfAanmeldingenM %>%
  filter(INS_Inschrijvingsjaar == nTest_year)

## Factor order is FALSE, TRUE (so weights are reversed)
vClass_weightsM <- rev(dfAanmeldingenM_train %>%
                         count(Ingestroomd) %>%
                         pull(n) / nrow(dfAanmeldingenM_train))

rf_modelM <- rand_forest(mode = "classification",
                         trees = 223,
                         mtry = 8) %>%
  set_engine("ranger",
             class.weights = vClass_weightsM,
             max.depth = 11,
             #always.split.variables = "INS_Opleidingsnaam_2002",
             ## Documentation lies, uses only 1 thread by default
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
  "Conv_groep_lag_fac",
  "Test_set",
  "OPL_Numerus_fixus_selectie_capaciteit_max"
)


rf_recipeM <-
  recipe(
    Ingestroomd ~ .,
    data = dfAanmeldingenM_train) %>%
  step_novel(AAN_Substatus, AAN_Status) %>%
  step_string2factor(all_nominal_predictors()) %>%
  step_unknown(c(AAN_Substatus, INS_Opleidingsnaam_2002, INS_Hoogste_vooropleiding_soort,
                 AAN_Soort_aanmelding)) %>%
  step_rm(all_of(vRemovalsM))

rf_wflowM <- workflow() %>%
  add_model(rf_modelM) %>%
  add_recipe(rf_recipeM)

set.seed(652)
rf_fitM <- fit(rf_wflowM, dfAanmeldingenM_train)

dfResultM <- rf_fitM %>%
  predict(dfAanmeldingenM_test, type = "class") %>%
  cbind(predict(rf_fitM, dfAanmeldingenM_test, type = "prob"))

dfCombined_testM <- dfAanmeldingenM_test %>%
  select(INS_Studentnummer, INS_Opleidingsnaam_2002, INS_Inschrijvingsjaar) %>%
  cbind(dfResultM)

dfOutputM <- dfAanmeldingenM %>%
  left_join(dfCombined_testM, by = c("INS_Studentnummer", "INS_Opleidingsnaam_2002",
                                     "INS_Inschrijvingsjaar"), relationship = "one-to-one")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Combineren ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
dfOutput_combined <- dfOutputB %>%
  rbind(dfOutputM) %>%
  arrange(desc(INS_Inschrijvingsjaar)) %>%
  mutate(INS_Studentnummer = hash_var(INS_Studentnummer))


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## BEWAAR & RUIM OP ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

write_file(dfOutput_combined,
           "Conversieprognose_resultaat",
           "Tableau/Git voor Tableau/Data/F. Prognoses/VU-Instroomprognose/",
           save_csv = TRUE)

clear_script_objects()

