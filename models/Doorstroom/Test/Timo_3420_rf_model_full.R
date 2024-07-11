## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Titel van het bestand ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code voor Student Analytics Vrije Universiteit Amsterdam
## Copyright 2022 VU
## Web Page: http://www.vu.nl
## Contact: vu-analytics@vu.nl
## Verspreiding buiten de VU: Ja
##
## Doel: Voorspellen van doorstroom in jaar 1 bachelor na 1 februari
##
## Afhankelijkheden: Afhankelijkheid
##
## Datasets: AS, resultaten
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
library(tidymodels)

Dates <- read_file_proj("ACA_Dates", dir = "2. Geprepareerde data")

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
                      "INS_Opleidingsnaam_2002",
                      "INS_Opleidingsvorm_naam",

                      ## Empty since 2017
                      #"INS_Opleiding_uitgeloot_ja_nee",
                      #"INS_Opleiding_uitgeloot_naam",

                      "INS_Soort_eerstejaars",
                      "INS_Uitwonend",
                      "INS_Vooropleiding_voor_HO_soort_SAP",
                      "INS_Vooropleiding_voor_HO_profiel_standaard",
                      "INS_Vooropleiding_voor_HO_VWO_examen_kans",


                      # "MVR_AV_Studiedoelen_stellen",
                      # "MVR_AV_Studieplanning",
                      # "MVR_AV_Zelfdiscipline",
                      # "MVR_OBK_Vertrouwen_studiesucces",
                      # "MVR_Score_ingeschreven_opleiding",
                      "MVR_Studiesituatie_Ouders_universiteit",
                      "MVR_Studiesituatie_Financien_belemmering",
                      "MVR_Studiesituatie_Ondersteuning_nodig",
                      "MVR_Studiesituatie_Goede_werkplek",
                      "MVR_Studiesituatie_Omgeving_hulp",
                      "MVR_Studiesituatie_Zorgtaak_belemmering",
                      # "MVR_Studiesituatie_Studiekeuze",
                      # "MVR_Studiesituatie_Reistijd_minuten",
                      # "MVR_Ingeschreven_voor_hoogste_interesse",
                      #

                      "MVR_OBK_Keuzezekerheid",
                      "MVR_AI_Keuzezekerheid",

                      "OPL_BSA_EC_eis_aug_jr1",
                      "OPL_VU_alfa_beta_gamma",
                      "OPL_Cluster",
                      #"OPL_Numerus_fixus_selectie",
                      #"OPL_Instructietaal",

                      "ORI_Orientatie_komt_voor",
                      "ORI_Orientatie_vooraf_verschillend_totaal",

                      "PUC_Deelgenomen",

                      "SUC_Uitval_na_jaar_1_herberekend",

                      "VOP_School_reistijd_OV_VU",
                      "VOP_School_gemeente_vestiging",
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
         INS_Inschrijvingsjaar >= 2017,
         INS_Opleidingsfase_BPM == "B",
         !INS_Uitschrijving_voor_1_feb,
         INS_Hoofdneven == "Hoofdinschrijving",
         !INS_Opleidingsnaam_2002 %in% vUVA_first_opleidingen)

## 2013 data for creation of features
dfAS2013_raw <- readrds_csv(output = "3. Analyseset/Analysis_set_1.fst", columns = c(vModelvariabelen, vFiltervariabelen, vExtravariabelen)) %>%
  filter(INS_Studiejaar == 1,
         INS_Inschrijvingsjaar == min(dfAS_raw$INS_Inschrijvingsjaar) - 1,
         INS_Opleidingsfase_BPM == "B",
         !INS_Uitschrijving_voor_1_feb)




lAanmeldingen_bestandspaden <- list.files(
  paste0(Sys.getenv("NETWORK_DIR"),
         "Output/", Sys.getenv("BRANCH"), "/",
         "2. Geprepareerde data/"),
  pattern = "AAN_Aanmeldingen_per_dag_2",
  full.names = T)

## Today
#nDagen_tot_1_sept <- as.numeric(as.Date(paste0(year(today()), "-09-01")) - today())
## 1 mei
nDagen_tot_1_sept <- as.numeric(as.Date("2024-09-01") - as.Date("2024-05-02"))

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
dfOpleidingen_raw <- readrds_csv(dataloc = "Output/_REPOSITORIES/opleidingen-analyseset/main/3. Analyseset/OPLAS_VU.rds") %>%
  select(INS_Opleidingsnaam_2002, INS_Inschrijvingsjaar, OPL_Numerus_fixus_selectie,
         OPL_Instructietaal) %>%
  distinct() %>%
  filter(INS_Inschrijvingsjaar <= 2022) %>%
  filter(row_number() == 1, .by = c(INS_Inschrijvingsjaar, INS_Opleidingsnaam_2002))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. BEWERKEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
gc()

dfAS_full <- tibble(dfAS_raw)
dfAS2013 <- tibble(dfAS2013_raw)

dfResultaten <- dfPogingen_raw %>%
  ## Filter out dupiclate results (registered for multiple RES_Inschrijving_soort)
  filter(any(!is.na(RES_Inschrijving_soort)) & !is.na(RES_Inschrijving_soort) |
           !any(!is.na(RES_Inschrijving_soort)),
         .by = c(INS_Studentnummer, INS_Opleidingsnaam_2002,
                 RES_Academisch_jaar_beoordeling)) %>%
  mutate(RES_Max_poging_in_jaar = RES_Poging_nummer == max(RES_Poging_nummer),
         .by = c(INS_Studentnummer, INS_Opleidingsnaam_2002, RES_Academisch_jaar_beoordeling,
                 RES_Module_code))
## Opleidingen in 2023 are missing NF data still
vNieuwe_NF <- c("B Gezondheid en Leven", "B Computer Science")
dfOpleidingen <-  dfOpleidingen_raw %>%
  filter(INS_Inschrijvingsjaar == 2022) %>%
  mutate(INS_Inschrijvingsjaar = 2023,
         OPL_Numerus_fixus_selectie = if_else(INS_Opleidingsnaam_2002 %in% vNieuwe_NF,
                                              TRUE, OPL_Numerus_fixus_selectie)) %>%
  rbind(dfOpleidingen_raw) %>%
  mutate(OPL_Numerus_fixus_selectie = replace_na(OPL_Numerus_fixus_selectie, FALSE)) %>%
  filter() %>%
  distinct()


## Date of prognosis
#peildatum <- today()
peildatum <- as.Date("2024-05-10")

dfPeilperiode <- Dates %>%
  filter(ACA_Peildatum_invoer < peildatum) %>%
  filter(ACA_Peildatum_invoer == max(ACA_Peildatum_invoer))

peilperiode <- dfPeilperiode %>%
  pull(ACA_Periode)

peildatum_beoordeling <- dfPeilperiode %>%
  pull(ACA_Peildatum)

peildatum_beoordeling_md_str <- str_sub(peildatum_beoordeling, 6)

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
                                      "INS_Inschrijvingsjaar" = "RES_Academisch_jaar_beoordeling"),
            relationship = "one-to-one") %>%
  left_join(dfOpleidingen, by = c("INS_Opleidingsnaam_2002", "INS_Inschrijvingsjaar"),
            relationship = "many-to-one")

dfAS2013 <- dfAS2013 %>%
  left_join(dfResultaten_aggr, by = c("INS_Studentnummer",
                                      "INS_Opleidingsnaam_2002",
                                      "INS_Inschrijvingsjaar" = "RES_Academisch_jaar_beoordeling"))

get_student_rank_in_year <- function(df, column) {
  result <- df %>%
    group_by(INS_Opleidingsnaam_2002, INS_Inschrijvingsjaar) %>%
    mutate(rank = rank({{column}}, ties.method = "min"),
           new = rank / n()) %>%
    ungroup()
  return(result$new)
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
         ## Add lower BSA for 2019, 2020 and 2021 (Covid)
         OPL_BSA_EC_eis_aug_jr1 = case_when(
           INS_Inschrijvingsjaar %in% c(2020, 2021) ~ OPL_BSA_EC_eis_aug_jr1 - 6,
           INS_Inschrijvingsjaar == 2019 ~ 0,
           TRUE ~ OPL_BSA_EC_eis_aug_jr1
         ),
         RES_EC_perc_BSA = RES_Aantal_EC_tm_P2 / OPL_BSA_EC_eis_aug_jr1,
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
         VOP_School_Amsterdam_eo = VOP_School_gemeente_vestiging %in% c("AMSTERDAM", "AMSTELVEEN"),
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
  filter(AAN_Indicatie_EOI,
         !Laatste_datum) %>%
  summarise(Aantal_aanmeldingen_totaal = n(),
            Aantal_aanmeldingen_afgewezen = sum(AAN_Status == "Afgewezen"),
            Aantal_aanmeldingen_gecreeerd = sum(AAN_Status == "Gecreeerd"),
              .by = c(INS_Studentnummer, Aanmeldingsjaar))



## Only really useful after May, noone registers before that
dfAanmeldingen_herinschrijvingen <- dfAanmeldingen %>%
  filter(AAN_Indicatie_eerdere_inschrijving == "Herinschrijver",
         !Laatste_datum) %>%
  mutate(Herinschrijving_volgend_jaar = TRUE) %>%
  select(INS_Studentnummer, Aanmeldingsjaar, INS_Opleidingsnaam_2002, Herinschrijving_volgend_jaar)

dfAS_full <- dfAS_full %>%
  left_join(dfAanmeldingen_nieuwe_opl, by = c("INS_Studentnummer", "INS_Inschrijvingsjaar" = "Aanmeldingsjaar"),
            relationship = "many-to-one") %>%
  left_join(dfAanmeldingen_vorig_jaar, by = c("INS_Studentnummer", "INS_Inschrijvingsjaar"),
            relationship = "many-to-one") %>%
  ## Don't count own opl
  mutate(Aantal_aanmeldingen_vorig_jaar_NF = Aantal_aanmeldingen_vorig_jaar_NF - OPL_Numerus_fixus_selectie) %>%
  left_join(dfAanmeldingen_herinschrijvingen, by = c("INS_Studentnummer",
                                                     "INS_Inschrijvingsjaar" = "Aanmeldingsjaar",
                                                     "INS_Opleidingsnaam_2002"),
            relationship = "one-to-one")


## 2013 data by programme to improve programme-specific predictions, only for use in train/val
df2013summarised <- dfAS2013 %>%
  mutate(INS_Opleidingsnaam_2002 = recode(INS_Opleidingsnaam_2002,
                                          "B Archeologie" = "Comb oudheid",
                                          "B Griekse en Latijnse Taal en Cultuur" = "Comb oudheid",
                                          "B Oudheidwetenschappen" = "Comb oudheid",
                                          "B Natuur- en Sterrenkunde (joint degree)" = "Comb nask",
                                          "B Scheikunde (joint degree)" = "Comb nask",
                                          "B Theology and Religious Studies" = "comb theologie",
                                          "B Theologie (joint degree)" = "comb theologie"
  )) %>%
  group_by(INS_Opleidingsnaam_2002) %>%
  summarise(gemEC_2013_P2 = mean(RES_Aantal_EC_tm_P2),
            gemEC_2013 = mean(RES_Aantal_EC_tm_jaar_1),
            uitval_2013 = sum(SUC_Uitval_na_jaar_1_herberekend) / n(),
            doorstroom_laag_bsa_2013 = sum(RES_Aantal_EC_tm_jaar_1 < OPL_BSA_EC_eis_aug_jr1 &
                                                  SUC_Uitval_na_jaar_1_herberekend == "FALSE") /
              sum(RES_Aantal_EC_tm_jaar_1 < OPL_BSA_EC_eis_aug_jr1))

dfAS_full <- dfAS_full %>%
  left_join(df2013summarised, by = "INS_Opleidingsnaam_2002")


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

vNA_Median <- c(
                "gemEC_prev_year_lag_mean",
                "gemEC_prev_year_P2_lag_mean",
                "uitval_prev_year_lag_mean",
                "doorstroom_laag_bsa_prev_year_lag_mean",
                "gemEC_2013",
                "gemEC_2013_P2",
                "uitval_2013",
                "doorstroom_laag_bsa_2013"
)

vNA_Median_VOP <- c(
  "VOP_Cijfer_gemiddeld_alfa",
  "VOP_Cijfer_gemiddeld_gamma")
  #"VOP_Cijfer_wiskunde",
  #"VOP_Cijfer_gemiddeld")


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


## Add values from previous year, only for use in test and actual model
dfPrev_years_values <- dfAS_full %>%
  group_by(INS_Inschrijvingsjaar, INS_Opleidingsnaam_2002) %>%
  summarise(gemEC_prev_year_P2 = mean(RES_Aantal_EC_tm_P2, na.rm = TRUE),
            gemEC_prev_year = mean(RES_Aantal_EC_tm_jaar_1, na.rm = TRUE),
            uitval_prev_year = sum(SUC_Uitval_na_jaar_1_herberekend == "TRUE") / n(),
            doorstroom_laag_bsa_prev_year = sum(RES_Aantal_EC_tm_jaar_1 < OPL_BSA_EC_eis_aug_jr1 &
                                        SUC_Uitval_na_jaar_1_herberekend == "FALSE") /
              sum(RES_Aantal_EC_tm_jaar_1 < OPL_BSA_EC_eis_aug_jr1)) %>%
  group_by(INS_Opleidingsnaam_2002) %>%
  mutate(across(c(gemEC_prev_year_P2, gemEC_prev_year, uitval_prev_year, doorstroom_laag_bsa_prev_year),
                ~(lag(.x, n = 1) + lag(.x, n = 2)) / 2,
                .names = "{.col}_lag_mean")) %>%
  ungroup() %>%
  select(-c(gemEC_prev_year_P2, gemEC_prev_year, uitval_prev_year, doorstroom_laag_bsa_prev_year))

dfAS_full <- dfAS_full %>%
  left_join(dfPrev_years_values, by = c("INS_Opleidingsnaam_2002", "INS_Inschrijvingsjaar"))

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
model = "B1"
source("20. Test/Testscripts doorstroom RF/Doorstroom - config.R")

dfAS_trainval <- dfAS_full %>%
  filter(INS_Inschrijvingsjaar %in% nTrain_years)

dfAS_test <- dfAS_full %>%
  filter(INS_Inschrijvingsjaar %in% nTest_years)

set.seed(10)
## Only use trainset for all models
vClass_weights <- rev(dfAS_trainval %>%
                        count(SUC_Uitval_na_jaar_1_herberekend) %>%
                        pull(n) / nrow(dfAS_trainval))

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

vRemovalsVal <- c(
  "gemEC_prev_year_lag_mean",
  "gemEC_prev_year_P2_lag_mean",
  "uitval_prev_year_lag_mean",
  "doorstroom_laag_bsa_prev_year_lag_mean"
)


rf_recipe_base <-
  recipe(
    SUC_Uitval_na_jaar_1_herberekend ~ .,
    data = dfAS_trainval) %>%
  step_rm(c(all_of(c(vRemovals, vFiltervariabelen, vExtravariabelen)), starts_with("WISK"))) %>%
  step_mutate_at(INS_Uitwonend, VOP_School_NH, OPL_Numerus_fixus_selectie, fn = as.character) %>%
  step_novel(INS_Opleidingsnaam_2002, OPL_Numerus_fixus_selectie) %>%
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

rf_recipe_val <- rf_recipe_base %>%
  step_rm(all_of(vRemovalsVal))

rf_recipe_test <- rf_recipe_base %>%
  step_rm(all_of(vRemovalsTest))

rf_wflow_val <-
  workflow() %>%
  add_model(rf_model) %>%
  add_recipe(rf_recipe_val)


rf_wflow_test <-
  workflow() %>%
  add_model(rf_model) %>%
  add_recipe(rf_recipe_test)


# Train/val
set.seed(10)

data_folds <- group_vfold_cv(dfAS_trainval, group = INS_Inschrijvingsjaar)


set.seed(652)
keep_pred <- control_resamples(save_pred = TRUE, save_workflow = TRUE)
rf_fit_val <- fit_resamples(rf_wflow_val, data_folds, control = keep_pred)
roc_res <- metric_set(roc_auc)


result_val <- rf_fit_val %>%
  collect_predictions()

## Judging
collect_metrics(rf_fit_val)


combined_val <- dfAS_trainval %>%
  select(-SUC_Uitval_na_jaar_1_herberekend) %>%
  cbind(arrange(result_val, .row)) %>%
  select(-starts_with("WISKUNDE"))

names(combined_val) <- gsub("_tm_P[0-9]$", "", names(combined_val))

## Calculate smape
combined_val %>%
  filter(INS_Inschrijvingsjaar != 2019) %>%
  group_by(INS_Opleidingsnaam_2002) %>%
  summarise(preds = round(sum(.pred_FALSE), 1),
            truth = sum(SUC_Uitval_na_jaar_1_herberekend == "FALSE")) %>%
  filter(truth > 75) %>%
  smape(truth = truth, estimate = preds)


# Test
set.seed(652)
rf_fit_test <- fit(rf_wflow_test, dfAS_trainval)
roc_res <- metric_set(roc_auc)


result_test <- rf_fit_test %>%
  predict(dfAS_test, type = "class") %>%
  cbind(predict(rf_fit_test, dfAS_test, type = "prob"))

combined_test <- dfAS_test %>%
  cbind(result_test) %>%
  select(-starts_with("WISKUNDE"))


## Make variable name consistent regardless of period selected
names(combined_test) <- gsub("_tm_P[0-9]$", "", names(combined_test))

## Get smape
combined_test %>%
  filter(INS_Inschrijvingsjaar != 2019) %>%
  group_by(INS_Opleidingsnaam_2002) %>%
  summarise(preds = round(sum(.pred_FALSE), 1),
            truth = sum(SUC_Uitval_na_jaar_1_herberekend == "FALSE")) %>%
  filter(truth > 75) %>%
  smape(truth = truth, estimate = preds)

combined_full <- combined_val %>%
  select(names(combined_test)) %>%
  rbind(combined_test) %>%
  mutate(Testset = INS_Inschrijvingsjaar %in% nTest_years)


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## BEWAAR & RUIM OP ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

date <- gsub("-", "", Sys.Date())

saverds_csv(dfAS_full, paste0("dfAS_prepped_P", peilperiode, "_", date), dataloc = "Tableau/205 VU-Inschrijvingen prognose 2021/Random Forest/Data/", save_csv = TRUE)
#
# saverds_csv(combined_val, "rf_results", dataloc = "Tableau/205 VU-Inschrijvingen prognose 2021/Random Forest/Data/", save_csv = TRUE)
# saverds_csv(combined_val, paste0("rf_results", date), dataloc = "Tableau/205 VU-Inschrijvingen prognose 2021/Random Forest/Data/Archive/", save_csv = TRUE)
#
# saverds_csv(combined_test, "rf_test_results", dataloc = "Tableau/205 VU-Inschrijvingen prognose 2021/Random Forest/Data/", save_csv = TRUE)
#
saverds_csv(combined_full, "rf_full_results", dataloc = "Tableau/205 VU-Inschrijvingen prognose 2021/Random Forest/Data/", save_csv = TRUE)
saverds_csv(combined_full, "rf_full_results", dataloc = "Tableau/205 VU-Inschrijvingen prognose 2021/Random Forest/Data/Archive/", save_csv = TRUE)
#
# saverds_csv(rf_fit_test, "rf_model_test", dataloc = "Tableau/205 VU-Inschrijvingen prognose 2021/Random Forest/Data/")

#Clear_script_objects()



