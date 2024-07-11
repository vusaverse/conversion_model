## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Prognose doorstroom bachelor jaar 1 - RF.R ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code voor Student Analytics Vrije Universiteit Amsterdam
## Copyright 2022 VU
## Web Page: http://www.vu.nl
## Contact: vu-analytics@vu.nl
## Verspreiding buiten de VU: Ja
##
## Doel: Voorspellen van doorstroom in jaar 1 bachelor na 1 februari met random forest
##
## Afhankelijkheden: Afhankelijkheid
##
## Datasets: AS, resultaten, UAS
##
## Opmerkingen:
## 1) Gebaseerd op testscript 20. Test Timo_3420_rf_model_full.R
## 2) ___
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## TODO:
## 1) ___.d
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Geschiedenis:
## 14-02-2023: TK: Aanmaak bestand
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Settings

model = "B1"
wd = getwd()

source(paste0(wd, "/models/Doorstroom/Doorstroom - config.R"))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. INLEZEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Lees alle benodigde bestanden in:
library(tidymodels)

## Variables included in model (some removed later)
vModelvariabelen <- c("DEM_Geslacht",
                      "DEM_Leeftijd_peildatum_1_oktober",

                      "INS_Aansluiting_cat",
                      "INS_Aantal_inschrijvingen_in_HO",
                      "INS_Dagen_tussen_aanmelding_en_1_september",
                      "INS_Direct",
                      "INS_Dubbele_studie_VU",
                      "INS_Hoofdneven",
                      "INS_Hoogste_vooropleiding_soort_cat",
                      "INS_Opleiding_uitgeloot_ja_nee",
                      "INS_Opleidingsnaam_2002",
                      "INS_Opleidingsvorm_naam",
                      "INS_Opleiding_uitgeloot_naam",
                      "INS_Soort_eerstejaars",
                      "INS_Uitwonend",
                      "INS_Vooropleiding_voor_HO_soort_SAP",
                      "INS_Vooropleiding_voor_HO_profiel_standaard",
                      "INS_Vooropleiding_voor_HO_VWO_examen_kans",

                      "MVR_OBK_Keuzezekerheid",
                      "MVR_AI_Keuzezekerheid",
                      "MVR_Studiesituatie_Omgeving_hulp",

                      #"OPL_BSA_EC_eis_aug_jr1",
                      "OPL_VU_alfa_beta_gamma",
                      "OPL_Cluster",
                      #"OPL_Numerus_fixus_selectie",
                      #"OPL_Instructietaal",

                      "ORI_Orientatie_komt_voor",
                      "ORI_Orientatie_vooraf_verschillend_totaal",

                      "PUC_Deelgenomen",

                      "SUC_Uitval_na_jaar_1_herberekend",


                      "VOP_School_provincie_vestiging",

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

## For analysis in Tableau or feature engineering
vExtravariabelen <- c("INS_Faculteit",
                      "RES_Aantal_EC_tm_jaar_1",
                      "RES_Aantal_herkansingen_tm_jaar_1",
                      "INS_IO_Herkomst_EER_naam")

vUVA_first_opleidingen = c(
  "B Natuur- en Sterrenkunde (joint degree)",
  "B Scheikunde (joint degree)",
  "M Chemistry (joint degree)",
  "M Physics and Astronomy (joint degree)",
  "M Computational Science (joint degree)"
)




dfAS_raw <- readrds_csv(output = "3. Analyseset/Analysis_set_1.fst", columns = c(vModelvariabelen, vFiltervariabelen, vExtravariabelen)) %>%
  filter(INS_Studiejaar == 1,
         INS_Inschrijvingsjaar %in% c(vTrain_years, vFeature_creation_years, vTest_years),
         INS_Opleidingsfase_BPM == "B",
         !INS_Uitschrijving_voor_1_feb,
         INS_Hoofdneven == "Hoofdinschrijving",
         !INS_Opleidingsnaam_2002 %in% vUVA_first_opleidingen)


## Aanmeldingen
lAanmeldingen_bestandspaden <- list.files(
  paste0(Sys.getenv("NETWORK_DIR"),
         "Output/", Sys.getenv("BRANCH"), "/",
         "2. Geprepareerde data/"),
  pattern = "AAN_Aanmeldingen_per_dag_2",
  full.names = T)

## Today
nDagen_tot_1_sept <- as.numeric(as.Date(paste0(year(today()), "-09-01")) - today())

read_files <- function(bestand) {
  print(bestand)
  readRDS(bestand) %>%
    filter(!is.na(INS_Opleidingsnaam_2002),
           INS_Opleidingsfase_BPM == "B",
           INS_Faculteit != "AUC") %>%
    mutate(Laatste_datum = AAN_Dagen_tot_1_sept == min(AAN_Dagen_tot_1_sept),
           .by =  c(INS_Studentnummer, INS_Opleidingsnaam_2002, INS_Opleidingsfase_BPM, INS_Inschrijvingsjaar)) %>%
    filter(AAN_Dagen_tot_1_sept >= nDagen_tot_1_sept | Laatste_datum) %>%
    filter(AAN_Dagen_tot_1_sept == min(AAN_Dagen_tot_1_sept) | Laatste_datum,
           .by = c(INS_Studentnummer, INS_Opleidingsnaam_2002, INS_Opleidingsfase_BPM, INS_Inschrijvingsjaar)) %>%
    select(
      INS_Studentnummer,
      INS_Inschrijvingsjaar,
      INS_Opleidingsnaam_2002,
      AAN_Dagen_tot_1_sept,
      AAN_Indicatie_EOI,
      AAN_Indicatie_eerdere_inschrijving,
      AAN_Status,
      Laatste_datum
    ) %>%
    distinct()
}


dfAanmeldingen <-
  map_dfr(lAanmeldingen_bestandspaden, read_files) %>%
  filter(row_number() == 1,
         .by = c(INS_Studentnummer, INS_Opleidingsnaam_2002, INS_Inschrijvingsjaar))


## Alle Resultaten
dfPogingen_raw <- readrds_csv(output = "3. Analyseset/03. Resultaten/RES_Pogingen_geprepareerd.rds") %>%
  filter(grepl("^B", INS_Opleidingsnaam_2002),
         RES_Beoordeling_soort == 1) %>%
  ## Quite some duplicates with differing bestanddatums
  select(-RES_Bestand_datum) %>%
  distinct() %>%
  ## For keeping track of specific results
  mutate(.id = row_number())

## OPLAS for NF data
dfOpleidingen_raw <- read_file_proj("OPLAS_VU",
                                   base_dir = Sys.getenv("OUTPUT_DIR"),
                                   add_branch = FALSE,
                                   dir = "_REPOSITORIES/opleidingen-analyseset/main/3. Analyseset/")  %>%
  select(INS_Opleidingsnaam_2002, INS_Inschrijvingsjaar, OPL_Numerus_fixus_selectie, OPL_BSA_EC_eis_aug_jr1,
         OPL_Instructietaal) %>%
  distinct() %>%
  filter(INS_Inschrijvingsjaar <= 2022) %>%
  filter(row_number() == 1, .by = c(INS_Inschrijvingsjaar, INS_Opleidingsnaam_2002))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. BEWERKEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
dfAS_full <- tibble(dfAS_raw)
dfResultaten <- dfPogingen_raw %>%
  ## Filter out dupiclate results (registered for multiple RES_Inschrijving_soort)
  filter(any(!is.na(RES_Inschrijving_soort)) & !is.na(RES_Inschrijving_soort) |
           !any(!is.na(RES_Inschrijving_soort)),
         .by = c(INS_Studentnummer, INS_Opleidingsnaam_2002,
                 RES_Academisch_jaar_beoordeling)) %>%
  mutate(RES_Max_poging_in_jaar = RES_Poging_nummer == max(RES_Poging_nummer),
         .by = c(INS_Studentnummer, INS_Opleidingsnaam_2002, RES_Academisch_jaar_beoordeling,
                 RES_Module_code))


## Opleidingen in 2023 and 2024 are missing NF data still
dfOpleidingen = tibble(dfOpleidingen_raw)


vNieuwe_NF <- c("B Gezondheid en Leven", "B Computer Science")

max_jaar_NF_Data <- dfOpleidingen %>%
  filter(OPL_Numerus_fixus_selectie) %>%
  summarise(max_jaar = max(INS_Inschrijvingsjaar)) %>%
  pull(max_jaar)

for (jaar in (max_jaar_NF_Data + 1):max(vTest_years)) {
  dfOpleidingen <- dfOpleidingen %>%
    filter(INS_Inschrijvingsjaar == max_jaar_NF_Data) %>%
    mutate(INS_Inschrijvingsjaar = jaar,
           OPL_Numerus_fixus_selectie = if_else(INS_Opleidingsnaam_2002 %in% vNieuwe_NF,
                                                TRUE, OPL_Numerus_fixus_selectie)) %>%
    rbind(dfOpleidingen)
}

dfOpleidingen <- dfOpleidingen %>%
  mutate(OPL_Numerus_fixus_selectie = replace_na(OPL_Numerus_fixus_selectie, FALSE)) %>%
  distinct()


## Use result dataset directly, for consistent results regardless of date of upload of results
dfResultaten_aggr <- dfResultaten %>%
  mutate(RES_periode_eind = as.numeric(str_sub(RES_Academische_periode_vak_omschrijving, -1)),
         RES_periode_eind = ifelse(RES_periode_eind > Periode, Periode, RES_periode_eind),
         RES_Beoordeling_datum_md_str = str_sub(as.character(RES_Beoordeling_datum), 6)) %>%
  group_by(INS_Studentnummer, INS_Opleidingsnaam_2002, RES_Academisch_jaar_beoordeling, RES_Module_code) %>%
  ## Filter on pogingnummer so varying retake dates throughout the years don't affect results
  filter((RES_periode_eind <= peilperiode - 1 & RES_Poging_nummer <= 3) |
           (RES_periode_eind == peilperiode & RES_Poging_nummer <= 1),
         RES_Beoordeling_datum_md_str <= peildatum_beoordeling_md_str | month(RES_Beoordeling_datum) > 8) %>%
  group_by(INS_Studentnummer, INS_Opleidingsnaam_2002, RES_Academisch_jaar_beoordeling) %>%
  summarise(RES_Aantal_EC_tm_P2 = sum(RES_Studiepunten_behaald, na.rm = TRUE),
            RES_Gem_resultaat_tm_P2_all = mean(Beoordeling_numeriek, na.rm = TRUE),
            RES_Gem_resultaat_tm_P2_laatste_poging = mean(ifelse(RES_Max_poging_in_jaar,
                                                                  Beoordeling_numeriek,
                                                                  NA_real_),
                                                                 na.rm = TRUE),
            RES_Aantal_no_shows_tm_P2 = sum(RES_Beoordeling == "NS"),
            RES_Aantal_NVD_tm_P2 = sum(RES_Beoordeling == "NVD"),
            RES_Aantal_vakken = n_distinct(RES_Module_code),
            RES_Aantal_herkansingen = sum(RES_Poging_nummer > 1, na.rm = TRUE)) %>%
  ungroup()

dfAS_full <- dfAS_full %>%
  left_join(dfResultaten_aggr, by = c("INS_Studentnummer",
                                      "INS_Opleidingsnaam_2002",
                                      "INS_Inschrijvingsjaar" = "RES_Academisch_jaar_beoordeling")) %>%
  left_join(dfOpleidingen, by = c("INS_Opleidingsnaam_2002", "INS_Inschrijvingsjaar"),
            relationship = "many-to-one")



get_student_rank_in_year <- function(df, column) {
  result <- df %>%
    group_by(INS_Opleidingsnaam_2002, INS_Inschrijvingsjaar) %>%
    mutate(rank = rank({{column}}, ties.method = "min"),
           rank_relative = rank / n()) %>%
    ungroup()
  return(result$rank_relative )
}


## Rank students
dfAS_full$RES_rank_EC = get_student_rank_in_year(dfAS_full, RES_Aantal_EC_tm_P2)
dfAS_full$RES_rank_grade = get_student_rank_in_year(dfAS_full, RES_Gem_resultaat_tm_P2_all)


## Feature engineering
dfAS_full <- dfAS_full %>%
  ## Voeg kleine groepen van profielen samen
  mutate(INS_Vooropleiding_voor_HO_profiel_standaard = recode(INS_Vooropleiding_voor_HO_profiel_standaard,
                                                              "NG & CM" = "NG",
                                                              "NG & EM" = "NG",
                                                              "NT & EM" = "NT",
                                                              "NT & CM" = "NT"),
         ## Voeg kleine opleidingen samen
         INS_Opleidingsnaam_2002 = recode(INS_Opleidingsnaam_2002,
                                          "B Archeologie" = "Comb oudheid",
                                          "B Griekse en Latijnse Taal en Cultuur" = "Comb oudheid",
                                          "B Oudheidwetenschappen" = "Comb oudheid",
                                          "B Natuur- en Sterrenkunde (joint degree)" = "Comb nask",
                                          "B Scheikunde (joint degree)" = "Comb nask",
                                          "B Theology and Religious Studies" = "comb theologie",
                                          "B Theologie (joint degree)" = "comb theologie"
         ),
         ## Verdubbel EC deeltijdstudenten
         RES_Aantal_EC_tm_P2 = ifelse(INS_Opleidingsvorm_naam == "deeltijd",
                                      RES_Aantal_EC_tm_P2 * 2,
                                      RES_Aantal_EC_tm_P2),
         ## Add lower BSA for 2020 and 2021 (Covid)
         OPL_BSA_EC_eis_aug_jr1 = case_when(
           INS_Inschrijvingsjaar %in% c(2020, 2021) ~ OPL_BSA_EC_eis_aug_jr1 - 6,
           TRUE ~ OPL_BSA_EC_eis_aug_jr1
         ),
         ## Replace old numerical version with new categorical version based on quantiles
         MVR_AI_Keuzezekerheid = cut(MVR_OBK_Keuzezekerheid, breaks = c(0, 4, 6, 9, 15, Inf),
                                     labels = c("Niet van toepassing", "Beetje van toepassing",
                                       "Redelijk van toepassing", "Vrij sterk van toepassing",
                                       "Helemaal van toepassing")),
         ## To ordinal
         MVR_AI_Keuzezekerheid = factor(MVR_AI_Keuzezekerheid,
                                        order = TRUE,
                                        levels = c(
                                          "Niet van toepassing",
                                          "Beetje van toepassing",
                                          "Redelijk van toepassing",
                                          "Vrij sterk van toepassing",
                                          "Helemaal van toepassing")),

         RES_EC_perc_BSA = RES_Aantal_EC_tm_P2 / OPL_BSA_EC_eis_aug_jr1,
         ## Groepeer nationaliteiten
         INS_IO_Herkomst_NL = INS_IO_Herkomst_EER_naam == "Nederlands",
         ## Bepaal wiskundesoort
         VOP_Wiskundesoort = case_when(
           !is.na(VOP_Cijfer_wiskunde_d) ~ "Wiskunde D",
           !is.na(VOP_Cijfer_wiskunde_b) ~ "Wiskunde B",
           !is.na(VOP_Cijfer_wiskunde_a) ~ "Wiskunde A",
           !is.na(VOP_Cijfer_wiskunde_c) ~ "Wiskunde C",
           !is.na(VOP_Cijfer_wiskunde_beta) ~ "Beta",
           !is.na(VOP_Cijfer_wiskunde_alpha) ~ "Alpha",
           TRUE ~ "Onbekend"
         ),
         VOP_School_NH = VOP_School_provincie_vestiging == "Noord-Holland",
         SUC_Uitval_na_jaar_1_herberekend = factor(SUC_Uitval_na_jaar_1_herberekend)
  )


## Include aanmeldingen
dfAanmeldingen <- dfAanmeldingen %>%
  mutate(Aanmeldingsjaar = INS_Inschrijvingsjaar - 1)

dfAanmeldingen_vorig_jaar = dfAanmeldingen %>%
  filter(Laatste_datum,
         AAN_Indicatie_EOI) %>%
  left_join(dfOpleidingen, by = c("INS_Opleidingsnaam_2002", "INS_Inschrijvingsjaar"), relationship =
              "many-to-one") %>%
  summarise(Aantal_aanmeldingen_vorig_jaar_totaal = n(),
            Aantal_aanmeldingen_vorig_jaar_afgewezen = sum(AAN_Status == "Afgewezen"),
            Aantal_aanmeldingen_vorig_jaar_NF = sum(OPL_Numerus_fixus_selectie),
            .by = c(INS_Studentnummer, INS_Inschrijvingsjaar))

dfAanmeldingen_nieuwe_opl <- dfAanmeldingen %>%
  filter(!Laatste_datum,
         AAN_Indicatie_EOI) %>%
  summarise(Aantal_aanmeldingen_totaal = n(),
            Aantal_aanmeldingen_afgewezen = sum(AAN_Status == "Afgewezen"),
            Aantal_aanmeldingen_gecreeerd = sum(AAN_Status == "Gecreeerd"),
            .by = c(INS_Studentnummer, Aanmeldingsjaar))

## Only really useful after May, noone registers before that
dfAanmeldingen_herinschrijvingen <- dfAanmeldingen %>%
  filter(!Laatste_datum,
         AAN_Indicatie_eerdere_inschrijving == "Herinschrijver") %>%
  mutate(Herinschrijving_volgend_jaar = TRUE) %>%
  select(INS_Studentnummer, Aanmeldingsjaar, INS_Opleidingsnaam_2002, Herinschrijving_volgend_jaar)

dfAS_full <- dfAS_full %>%
  left_join(dfAanmeldingen_vorig_jaar, by = c("INS_Studentnummer", "INS_Inschrijvingsjaar"),
            relationship = "many-to-one") %>%
  ## Don't count own opl
  mutate(Aantal_aanmeldingen_vorig_jaar_NF = Aantal_aanmeldingen_vorig_jaar_NF - OPL_Numerus_fixus_selectie) %>%

  left_join(dfAanmeldingen_nieuwe_opl, by = c("INS_Studentnummer", "INS_Inschrijvingsjaar" = "Aanmeldingsjaar"),
            relationship = "many-to-one") %>%
  left_join(dfAanmeldingen_herinschrijvingen, by = c("INS_Studentnummer",
                                                     "INS_Inschrijvingsjaar" = "Aanmeldingsjaar",
                                                     "INS_Opleidingsnaam_2002"),
            relationship = "one-to-one")


## Calculate features based on results of previous years
dfPrev_years_values <- dfAS_full %>%
  group_by(INS_Inschrijvingsjaar, INS_Opleidingsnaam_2002) %>%
  summarise(gem_EC_P2 = mean(RES_Aantal_EC_tm_P2, na.rm = TRUE),
            gem_EC = mean(RES_Aantal_EC_tm_jaar_1, na.rm = TRUE),
            gem_uitval = sum(SUC_Uitval_na_jaar_1_herberekend == "TRUE") / n(),
            doorstroom_laag_bsa = sum(RES_Aantal_EC_tm_jaar_1 < OPL_BSA_EC_eis_aug_jr1 &
                                                  SUC_Uitval_na_jaar_1_herberekend == "FALSE") /
              sum(RES_Aantal_EC_tm_jaar_1 < OPL_BSA_EC_eis_aug_jr1)) %>%
  group_by(INS_Opleidingsnaam_2002) %>%
  mutate(across(c(gem_EC_P2, gem_EC, gem_uitval, doorstroom_laag_bsa),
               ~(lag(.x, n = 1) + lag(.x, n = 2)) / 2,
               .names = "{.col}_lag_mean")) %>%
  ungroup() %>%
  select(-c(gem_EC_P2, gem_EC, gem_uitval, doorstroom_laag_bsa))

dfAS_full <- dfAS_full %>%
  filter(!INS_Inschrijvingsjaar %in% vFeature_creation_years) %>%
  left_join(dfPrev_years_values, by = c("INS_Opleidingsnaam_2002", "INS_Inschrijvingsjaar"),
            relationship = "many-to-one")




## Behandel missende waardes

## Herkomst niet-NL en uitwonend NA dan uitwonend TRUE
dfAS_full <- dfAS_full %>%
  mutate(INS_Uitwonend = ifelse(is.na(INS_Uitwonend) & !INS_IO_Herkomst_NL,
                                TRUE,
                                INS_Uitwonend))


vNA_Mode <- c("INS_Dagen_tussen_aanmelding_en_1_september",
              "INS_Aantal_inschrijvingen_in_HO",
              "INS_Soort_eerstejaars",
              "DEM_Geslacht")

vNA_Median <- c("gem_EC_lag_mean",
                "gem_EC_P2_lag_mean",
                "gem_uitval_lag_mean",
                "doorstroom_laag_bsa_lag_mean"
)

vNA_Median_VOP <- c(
  "VOP_Cijfer_gemiddeld_alfa",
  "VOP_Cijfer_gemiddeld_gamma")


## Fill NA"s with standard values
dfAS_full <- dfAS_full %>%
  mutate(
    across(c(RES_Gem_resultaat_tm_P2_laatste_poging, RES_Gem_resultaat_tm_P2_all), ~replace_na(.x, 1)),
    across(c(INS_Direct), ~replace_na(.x, TRUE)),
    across(c(Herinschrijving_volgend_jaar), ~replace_na(.x, FALSE)),
    across(c(RES_Aantal_herkansingen, RES_Aantal_vakken, RES_Aantal_EC_tm_P2, RES_Aantal_no_shows_tm_P2,
             RES_Aantal_NVD_tm_P2, RES_EC_perc_BSA, Aantal_aanmeldingen_gecreeerd,
             Aantal_aanmeldingen_afgewezen, Aantal_aanmeldingen_totaal, Aantal_aanmeldingen_vorig_jaar_totaal,
             Aantal_aanmeldingen_vorig_jaar_afgewezen, Aantal_aanmeldingen_vorig_jaar_NF), ~replace_na(.x, 0)))




## If we have VOP_gemiddeld_beta, use that for wiskunde
dfAS_full <- dfAS_full %>%
  mutate(VOP_Cijfer_wiskunde = ifelse(is.na(VOP_Cijfer_wiskunde), VOP_Cijfer_gemiddeld_beta, VOP_Cijfer_wiskunde))


## Maybe change imputed values based on other data (knn)? Also beware of data leakage
dfAS_full <- dfAS_full %>%
  fill_df_with_agg_by_group(
    group = c("INS_Opleidingsnaam_2002", "INS_Inschrijvingsjaar"),
    columns = c(vNA_Median, vNA_Median_VOP),
    overwrite_col = TRUE,
    statistic = median,
    fill_empty_group = TRUE) %>%
  fill_df_with_agg_by_group(
    group = c("INS_Opleidingsnaam_2002", "INS_Inschrijvingsjaar"),
    columns = vNA_Mode,
    overwrite_col = TRUE,
    statistic = vvconverter::mode,
    fill_empty_group = TRUE)


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3. Modelleren ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

dfAS_train <- dfAS_full %>%
  filter(INS_Inschrijvingsjaar %in% vTrain_years)

dfAS_test <- dfAS_full %>%
  filter(INS_Inschrijvingsjaar %in% vTest_years)

vClass_weights <- rev(dfAS_train %>%
                        count(SUC_Uitval_na_jaar_1_herberekend) %>%
                        pull(n) / nrow(dfAS_train))

## Based on gridsearch
rf_model <- rand_forest(mode = "classification",
                        trees = trees,
                        mtry = mtry) %>%
  set_engine("ranger",
             class.weights = vClass_weights,
             max.depth = depth,
             always.split.variables = always_split_opl,
             splitrule = "hellinger",
             num.threads = 4)


vRemovals <- c(
  "INS_IO_Herkomst_EER_naam",
  "VOP_Cijfer_wiskunde_alpha",
  "VOP_Cijfer_wiskunde_beta",
  "VOP_Cijfer_gemiddeld_beta",
  "VOP_Cijfer_wiskunde_a",
  "VOP_Cijfer_wiskunde_b",
  "VOP_Cijfer_wiskunde_c",
  "VOP_Cijfer_wiskunde_d",
  "MVR_OBK_Keuzezekerheid",
  "VOP_School_provincie_vestiging"
)


rf_recipe <-
  recipe(
    SUC_Uitval_na_jaar_1_herberekend ~ .,
    data = dfAS_train) %>%
  step_mutate_at(INS_Uitwonend, VOP_School_NH, OPL_Numerus_fixus_selectie,fn = as.character) %>%
  step_novel(INS_Opleidingsnaam_2002, OPL_Numerus_fixus_selectie) %>%
  ## Impute bag is very slow, so use wisely in development
  step_impute_bag(VOP_Cijfer_gemiddeld, VOP_Cijfer_wiskunde, seed_val = 23) %>%
  step_unknown(c(INS_Aansluiting_cat,
                 INS_Opleiding_uitgeloot_naam,
                 INS_Uitwonend,
                 INS_Vooropleiding_voor_HO_soort_SAP,
                 INS_Vooropleiding_voor_HO_profiel_standaard,
                 INS_Vooropleiding_voor_HO_VWO_examen_kans,
                 INS_Hoogste_vooropleiding_soort_cat,
                 OPL_Cluster,
                 OPL_VU_alfa_beta_gamma,
                 OPL_Numerus_fixus_selectie,
                 VOP_School_NH,
                 MVR_Studiesituatie_Omgeving_hulp,
                 MVR_AI_Keuzezekerheid)) %>%
  step_ordinalscore(MVR_AI_Keuzezekerheid) %>%
  step_string2factor(all_nominal_predictors()) %>%
  step_rm(c(all_of(c(vRemovals, vFiltervariabelen, vExtravariabelen))))

rf_wflow <- workflow() %>%
  add_model(rf_model) %>%
  add_recipe(rf_recipe)


set.seed(652)
rf_fit <- fit(rf_wflow, dfAS_train)

dfResult <- rf_fit %>%
  predict(dfAS_test, type = "class") %>%
  cbind(predict(rf_fit, dfAS_test, type = "prob"))

dfCombined_test <- dfAS_test %>%
  cbind(dfResult) %>%
  select(-starts_with("WISKUNDE"))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## BEWAAR & RUIM OP ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

output <- "4. Analyses/Doorstroomprognose/Data/Resultaten/"
date <- gsub("-", "", Sys.Date())

write_file_proj(dfCombined_test,
                "doorstroom_predictions_B1",
                base_dir = paste0(Sys.getenv("NETWORK_DIR"), "Output/"),
                dir = output,
                add_branch = TRUE,
                extensions = "csv")

write_file_proj(dfCombined_test,
                paste0("doorstroom_predictions_B1", date),
                base_dir = paste0(Sys.getenv("NETWORK_DIR"), "Output/"),
                dir = paste0(output, "Archief/"),
                add_branch = TRUE,
                extensions = "csv")

clear_script_objects()
