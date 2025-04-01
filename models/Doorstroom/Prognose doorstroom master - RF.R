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
## Settings
model = "M"

source("models/Doorstroom/Doorstroom - config.R")

source("models/Doorstroom/helpers.R")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. INLEZEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Lees alle benodigde bestanden in:
library(tidymodels)

Dates <- read_file_proj("ACA_Dates", dir = "2. Geprepareerde data")

## Variables included in model (some removed later, RES_variables added later)
vModelvariabelen <- c("DEM_Geslacht",
                      "DEM_Leeftijd_peildatum_1_oktober",

                      "INS_Aansluiting_cat",
                      "INS_Aantal_inschrijvingen_jaar",
                      "INS_Dagen_tussen_aanmelding_en_1_september",
                      "INS_Direct",
                      "INS_Doorstroom_van_bachelor_naar_master",
                      "INS_Doorstroom_van_premaster_naar_master",

                      "INS_Dubbele_studie_VU",
                      "INS_Hoofdneven",
                      "INS_Hoogste_vooropleiding_soort_cat",
                      "INS_IO_Herkomst_EER_naam",
                      "INS_Onderwijsherkomst",
                      "INS_Opleidingsnaam_2002",
                      "INS_Opleidingsvorm_naam",
                      "INS_September_februari_instroom",
                      "INS_Soort_eerstejaars",
                      "INS_Status_Zachte_knip",
                      "INS_Studiejaar",
                      "INS_Tussenjaar_voor_M",
                      "INS_Uitwonend",
                      "INS_Vooropleiding_voor_HO_soort_SAP",
                      "INS_Vooropleiding_voor_HO_profiel_standaard",
                      "INS_Vooropleiding_voor_HO_VWO_examen_kans",
                      "INS_Vooropleiding_binnen_HO_jaar",

                      "OPL_VU_alfa_beta_gamma",
                      "OPL_Cluster",
                      "OPL_Instructietaal",
                      "OPL_Cluster_Researchmaster",
                      "OPL_Studielast_nominaal",

                      "ORI_Orientatie_komt_voor",
                      "ORI_Orientatie_vooraf_master_aanwezig",
                      "ORI_Orientatie_vooraf_verschillend_totaal",


                      "SUC_Type_uitstroom_studiejaar",

                      "VOP_Cijfer_gemiddeld")

## Variables for filtering students
vFiltervariabelen <- c("INS_Studentnummer",
                       "INS_Inschrijvingsjaar",
                       "INS_Inschrijvingsjaar_EOI",
                       "INS_Opleidingsfase_BPM",
                       "INS_Datum_uitschrijving")


## For analysis in Tableau, and/or calculation of other variables
vExtravariabelen <- c("INS_Faculteit",
                      "RES_Aantal_EC_cumulatief",
                      "RES_Aantal_EC_tm_P7",
                      "RES_Aantal_eindresultaten_tm_P7",
                      "RES_Aantal_herkansingen_tm_P7",
                      "RES_Aantal_no_shows_tm_P7",
                      "RES_Gem_resultaat_tm_nu",
                      "VOP_Cijfer_gemiddeld_beta",
                      "VOP_Cijfer_wiskunde",
                      "SUC_Uitval_na_jaar_1_herberekend",
                      "SUC_Diploma_nominaal",
                      "RES_Aantal_EC_tm_jaar_1",
                      "RES_Gem_resultaat_tm_jaar_1")


vUVA_first_opleidingen = c(
  "B Natuur- en Sterrenkunde (joint degree)",
  "B Scheikunde (joint degree)",
  "M Chemistry (joint degree)",
  "M Physics and Astronomy (joint degree)",
  "M Computational Science (joint degree)"
)

dfAS_raw <- readrds_csv(output = "3. Analyseset/Analysis_set_1.fst", columns = c(vModelvariabelen, vFiltervariabelen, vExtravariabelen)) %>%
  filter(INS_Inschrijvingsjaar %in% c(vTrain_years, vTest_years, vFeature_creation_years),
         INS_Opleidingsfase_BPM == "M",
         #!grepl("joint degree", INS_Opleidingsnaam_2002),
         month(INS_Datum_uitschrijving) > 1 & month(INS_Datum_uitschrijving) <= 8,
         INS_Hoofdneven == "Hoofdinschrijving",
         !INS_Opleidingsnaam_2002 %in% vUVA_first_opleidingen)



cFirstyearvariables <- c("INS_Verblijfsjaren_hoger_onderwijs",
                         "INS_Verblijfsjaren_wetenschappelijk_onderwijs",
                         "INS_Dagen_tussen_aanmelding_en_1_september",
                         "INS_Studiejaar",
                         "INS_Opleidingsnaam_2002")

dfFirst_year <- readrds_csv(output = "3. Analyseset/Analysis_set_1.fst", columns = c(vFiltervariabelen, cFirstyearvariables)) %>%
  filter(INS_Studiejaar == 1,
         INS_Opleidingsfase_BPM == 'M') %>%
  rename(INS_Verblijfsjaren_hoger_onderwijs_EJ = INS_Verblijfsjaren_hoger_onderwijs,
         INS_Dagen_tussen_aanmelding_en_1_september_EJ = INS_Dagen_tussen_aanmelding_en_1_september)

cPremaster_variables <- c("INS_Studentnummer",
                          "INS_Inschrijvingsjaar",
                          "RES_Gem_resultaat_tm_P7",
                          "INS_Opleidingsfase_BPM")


dfPremasters_raw <- readrds_csv(output = "3. Analyseset/Analysis_set_1.fst", columns = cPremaster_variables) %>%
  filter(INS_Opleidingsfase_BPM == 'P')


cBachelor_variables <- c("INS_Diplomajaar",
                         "INS_Studentnummer",
                         "INS_Inschrijvingsjaar",
                         "INS_Opleidingsfase_BPM",
                         "RES_Gem_resultaat_tm_voltooid")

dfBachelors_raw <- readrds_csv(output = "3. Analyseset/Analysis_set_1.fst", columns = cBachelor_variables) %>%
  filter(INS_Opleidingsfase_BPM == 'B') %>%
  select(-INS_Opleidingsfase_BPM)


## Bepaal bestandspaden naar alle resultatendatabestanden
Bestandspad <- list.files(
  paste0(Sys.getenv("NETWORK_DIR"), "Output/", Sys.getenv("BRANCH"), "/2. Geprepareerde data/"),
  full.names = TRUE,
  pattern = "^RES_Resultaten_voor_aggregatie.\\d{4}\\.rds$")


## Alle Resultaten
dfPogingen_raw <- readrds_csv(output = "3. Analyseset/03. Resultaten/RES_Pogingen_geprepareerd.rds") %>%
  filter(grepl("^M", INS_Opleidingsnaam_2002),
         RES_Beoordeling_soort == 1) %>%
  ## Quite some duplicates with differing bestanddatums
  select(-RES_Bestand_datum) %>%
  distinct() %>%
  ## For keeping track of specific results
  mutate(.id = row_number())


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
           INS_Opleidingsfase_BPM == "M",
           INS_Faculteit != "AUC") %>%
    filter(AAN_Dagen_tot_1_sept >= nDagen_tot_1_sept) %>%
    filter(AAN_Dagen_tot_1_sept == min(AAN_Dagen_tot_1_sept),
           .by = c(INS_Studentnummer, INS_Opleidingsnaam_2002, INS_Inschrijvingsjaar)) %>%
    select(
      INS_Studentnummer,
      INS_Inschrijvingsjaar,
      INS_Opleidingsnaam_2002,
      AAN_Dagen_tot_1_sept,
      AAN_Indicatie_EOI,
      AAN_Indicatie_eerdere_inschrijving,
      AAN_Status
    ) %>%
    distinct()
}


dfAanmeldingen <-
  map_dfr(lAanmeldingen_bestandspaden, read_files) %>%
  filter(row_number() == 1,
         .by = c(INS_Studentnummer, INS_Opleidingsnaam_2002, INS_Inschrijvingsjaar))

vBestanden_UAS <- list.files(
  paste0(
    Sys.getenv("NETWORK_DIR"),
    "Datasets/UAS/"),
  full.names = TRUE,
  ## Een reguliere expressie waarin gezocht wordt naar:
  pattern = "20[1-2][0-9]",
  recursive = FALSE)

vBestanden_UAS_Vakken <- list.files(
  paste0(
    vBestanden_UAS,
    "/Modules"),
  full.names = TRUE,
  ## Een reguliere expressie waarin gezocht wordt naar:
  pattern = ".csv",
  recursive = FALSE)


dfBestanden_UAS_Vakken <- tibble(Bestandspad = vBestanden_UAS_Vakken) %>%
  ## Voeg jaar toe
  mutate(UAS_Jaar = as.numeric(
    str_extract(Bestandspad, "(?<=/)([0-9]+)(?=/)")))

Read_UAS <- function(Bestandspad) {

  ## Lees het bestand in
  Data <- readr::read_csv(
    Bestandspad,
    col_types = cols(
      .default = col_character(),
      "Extern ID" = col_integer(),
    ))

  ## Retourneer de data
  Data
}

dfUAS_Vakken_raw <- dfBestanden_UAS_Vakken %>%
  mutate(data = map(Bestandspad, Read_UAS)) %>%
  unnest(cols = c(data)) %>%
  group_by(Code) %>%
  filter(UAS_Jaar == max(UAS_Jaar)) %>%
  ungroup() %>%
  select(Code, Type, `Extra curriculair`) %>%
  distinct()

vOpleidingvariabelen <- c(
  #"OPL_OPL_Nominale_studieduur_NUM",
  "OPL_UAS_Opleiding_Jaar_NUM",
  "OPL_UAS_Opleidingsnaam_2002_CHA",
  "INS_Aantal_Eerstejaars_studenten_INT",
  "INS_SUC_Diploma_nominaal_INT",
  "INS_SUC_Aantal_Uitval_na_jaar_1_INT",
  "INS_RES_Gemiddeld_resultaat_jr1",
  "INS_RES_Gemiddeld_EC_jr1"
)
## Opleiding data is wrong, wait for fix
# dfOpleidingen_raw <- readrds_csv(output = "3. Analyseset/Opleidingen_Analyseset_lang_na_stap_3.rds")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. BEWERKEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Make an actual copy (stracemem(dfAS_full)==tracemem(dfAS_raw) equals FALSE)
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

dfUAS_Vakken <- tibble(dfUAS_Vakken_raw) %>%
  mutate(`Extra curriculair` = recode(`Extra curriculair`,
                                      "Ja" = TRUE,
                                      "Nee" = FALSE),
         `Extra curriculair` = replace_na(`Extra curriculair`, FALSE)) %>%
  distinct() %>%
  filter(row_number() == 1, .by = Code)

# dfOpleidingen <- dfOpleidingen_raw %>%
#   select(all_of(vOpleidingvariabelen)) %>%
#   distinct()


dfOpleidingen_lagged <- dfAS_full %>%
  filter(INS_Studiejaar == 1) %>%
  group_by(INS_Inschrijvingsjaar, INS_Opleidingsnaam_2002) %>%
  mutate(Perc_uitval_jaar_1 = sum(SUC_Uitval_na_jaar_1_herberekend) / n(),
         Perc_nominaal = sum(SUC_Diploma_nominaal) / n(),
         Gem_resultaat_jaar_1 = mean(RES_Gem_resultaat_tm_jaar_1, na.rm = TRUE),
         Gem_EC_jaar_1 = mean(RES_Aantal_EC_tm_jaar_1, na.rm = TRUE)) %>%
  select(INS_Inschrijvingsjaar, INS_Opleidingsnaam_2002, Perc_uitval_jaar_1, Perc_nominaal,
         Gem_resultaat_jaar_1, Gem_EC_jaar_1) %>%
  group_by(INS_Opleidingsnaam_2002) %>%
  distinct() %>%
  arrange(INS_Inschrijvingsjaar) %>%
  mutate(across(c(Perc_uitval_jaar_1, Gem_resultaat_jaar_1, Gem_EC_jaar_1),
                ~lag(.x, n = 1), .names = "{col}_lag1"),
         Perc_nominaal_lag3 = lag(Perc_nominaal, n = 3)) %>%
  fill(c(Perc_uitval_jaar_1_lag1, Gem_resultaat_jaar_1_lag1, Gem_EC_jaar_1_lag1, Perc_nominaal_lag3)) %>%
  ungroup() %>%
  select(INS_Inschrijvingsjaar, INS_Opleidingsnaam_2002, Perc_uitval_jaar_1_lag1,
         Gem_resultaat_jaar_1_lag1, Gem_EC_jaar_1_lag1, Perc_nominaal_lag3)

dfAS_full <- dfAS_full %>%
  left_join(dfOpleidingen_lagged)


set.seed(1)
dfBachelors <- dfBachelors_raw %>%
  filter(INS_Diplomajaar == INS_Inschrijvingsjaar) %>%
  group_by(INS_Studentnummer) %>%
  filter(INS_Inschrijvingsjaar == max(INS_Inschrijvingsjaar)) %>%
  filter(RES_Gem_resultaat_tm_voltooid == max(RES_Gem_resultaat_tm_voltooid)) %>%
  ## Just in case previous filters are not enough (one student still had two programmes)
  slice_sample(n = 1) %>%
  ungroup() %>%
  rename("INS_Inschrijvingsjaar_b" = "INS_Inschrijvingsjaar",
         "RES_Gem_resultaat_tm_voltooid_b" = "RES_Gem_resultaat_tm_voltooid") %>%
  select(-INS_Diplomajaar)

dfAS_full <- dfAS_full %>%
  left_join(dfBachelors, by = "INS_Studentnummer", relationship = "many-to-one") %>%
  mutate(RES_Gem_resultaat_tm_voltooid_b = ifelse(
    INS_Inschrijvingsjaar_b < INS_Inschrijvingsjaar,
    RES_Gem_resultaat_tm_voltooid_b,
    NA_real_
  )) %>%
  select(-INS_Inschrijvingsjaar_b)


## Get premaster grade
set.seed(20)
dfPremasters <- dfPremasters_raw %>%
  group_by(INS_Studentnummer) %>%
  filter(INS_Inschrijvingsjaar == max(INS_Inschrijvingsjaar)) %>%
  slice_sample(n = 1) %>%
  ungroup() %>%
  mutate(RES_Gem_resultaat_tm_P7 =  replace_na(RES_Gem_resultaat_tm_P7, 6),
         Premaster_grade_cat = cut(RES_Gem_resultaat_tm_P7, breaks = c(-Inf, seq(6.5, 8, 1), Inf))) %>%
  select(INS_Studentnummer, Premaster_grade_cat)


dfAS_full <- dfAS_full %>%
  left_join(dfPremasters,
                   by = 'INS_Studentnummer', relationship = "many-to-one")


## RES_variables before year
dfAS_full <- dfAS_full %>%
  group_by(INS_Studentnummer, INS_Opleidingsnaam_2002) %>%
  mutate(RES_Aantal_herkansingen_voor_jaar = cumsum(lag(RES_Aantal_herkansingen_tm_P7, default = 0)),
         RES_Aantal_eindresultaten_voor_jaar = cumsum(lag(RES_Aantal_eindresultaten_tm_P7, default = 0)),
         RES_Aantal_no_shows_voor_jaar = cumsum(lag(RES_Aantal_no_shows_tm_P7, default = 0))) %>%
  ungroup()


dfResultaten <- dfResultaten %>%
  left_join(dfUAS_Vakken, by = c("RES_Module_code" = "Code"),
            relationship = "many-to-one")


## RES_ tm P2, for consistent determination of period
dfResultaten_aggr <- dfResultaten %>%
  arrange(RES_Beoordeling_datum) %>%
  mutate(RES_periode_eind = as.numeric(str_sub(RES_Academische_periode_vak_omschrijving, -1)),
         RES_periode_eind = ifelse(RES_periode_eind > Periode, Periode, RES_periode_eind),
         RES_Beoordeling_datum_md_str = str_sub(as.character(RES_Beoordeling_datum), 6),
         RES_Thesis_stage_behaald = ifelse(Type %in% c("Thesis", "Stage"),
                                           as.logical(RES_Studiepunten_behaald),
                                           FALSE)) %>%
  group_by(INS_Studentnummer, INS_Opleidingsnaam_2002) %>%
  mutate(RES_Gemiddeld_cijfer_cum_all = cumfunc_na.rm(Beoordeling_numeriek, cummean),
         RES_Gemiddeld_cijfer_cum_max_poging = cumfunc_na.rm(ifelse(RES_Max_poging_in_jaar,
                                                                    Beoordeling_numeriek,
                                                                    NA_real_), cummean),
         RES_Gemiddeld_poging_cum = cumfunc_na.rm(ifelse(RES_Max_poging_in_jaar,
                                                         RES_Poging_nummer_tm_nu,
                                                         NA_real_), cummean),
         RES_Aantal_no_shows_cum = cumfunc_na.rm(RES_Beoordeling == 'NS', cumsum),
         RES_Aantal_NVD_cum = cumfunc_na.rm(RES_Beoordeling == 'NVD', cumsum),
         RES_Aantal_EC_totaal = cumfunc_na.rm(RES_Studiepunten_behaald, cumsum),
         RES_Aantal_herkansingen_totaal = cumfunc_na.rm(RES_Poging_nummer > 1, cumsum),
         RES_Aantal_vakken_cum = row_number(),
         RES_Aantal_vakken_niet_gehaald_cum = cumfunc_na.rm(RES_Studiepunten_behaald == 0 &
                                                       RES_Studiepunten_geboekt != 0,
                                                       cumsum),
         RES_Thesis_stage_behaald = cumfunc_na.rm(RES_Thesis_stage_behaald, cumany),
         RES_Aantal_EC_excl_extracurriculair_cum = cumfunc_na.rm(ifelse(`Extra curriculair`, 0,
                                                                 RES_Studiepunten_behaald), cumsum)) %>%
  ungroup() %>%
  ## Filter on pogingnummer so varying retake dates throughout the years don't affect results
  filter((RES_periode_eind <= peilperiode - 1 & RES_Poging_nummer <= 3) |
           (RES_periode_eind == peilperiode & RES_Poging_nummer <= 1),
         RES_Beoordeling_datum_md_str <= peildatum_beoordeling_md_str | month(RES_Beoordeling_datum) > 8) %>%
  group_by(INS_Studentnummer, INS_Opleidingsnaam_2002, RES_Academisch_jaar_beoordeling) %>%
  summarise(RES_Aantal_EC_tm_P2 = sum(RES_Studiepunten_behaald, na.rm = TRUE),
            RES_Gem_resultaat_tm_P2 = mean(Beoordeling_numeriek, na.rm = TRUE),
            RES_Aantal_no_shows_tm_P2 = sum(RES_Beoordeling == "NS"),
            RES_Aantal_vakken_tm_P2 = n_distinct(RES_Module_code),
            RES_Aantal_herkansingen_tm_P2 = sum(RES_Poging_nummer > 1, na.rm = TRUE),
            across(c(RES_Thesis_stage_behaald, RES_Gemiddeld_cijfer_cum_max_poging,
                   RES_Gemiddeld_cijfer_cum_all, RES_Aantal_no_shows_cum,
                   RES_Aantal_NVD_cum, RES_Gemiddeld_poging_cum, RES_Aantal_EC_totaal,
                   RES_Aantal_herkansingen_totaal, RES_Aantal_vakken_cum,
                   RES_Aantal_vakken_niet_gehaald_cum, RES_Aantal_EC_excl_extracurriculair_cum), ~last(.x))) %>%
  ungroup() %>%
  mutate(RES_Aantal_NVD_relative = RES_Aantal_NVD_cum / RES_Aantal_vakken_cum)


dfAS_full <- dfAS_full %>%
  left_join(dfResultaten_aggr, by = c("INS_Studentnummer",
                                      "INS_Opleidingsnaam_2002",
                                      "INS_Inschrijvingsjaar" = "RES_Academisch_jaar_beoordeling")) %>%
  ## Fill cumulative results with previous year values in case no results in year
  arrange(INS_Inschrijvingsjaar) %>%
  group_by(INS_Studentnummer, INS_Opleidingsnaam_2002) %>%
  fill(RES_Gemiddeld_cijfer_cum_all, RES_Aantal_no_shows_cum,
       RES_Aantal_NVD_cum, RES_Gemiddeld_poging_cum, RES_Aantal_EC_totaal, RES_Aantal_herkansingen_totaal,
       RES_Aantal_vakken_cum, RES_Aantal_vakken_niet_gehaald_cum, RES_Aantal_EC_excl_extracurriculair_cum,
       .direction = "down") %>%
  ungroup()



## Overige RES_ toevoegen
dfAS_full <- dfAS_full %>%
  mutate(
    RES_Aantal_EC_voor_jaar = ifelse(!is.na(RES_Aantal_EC_cumulatief), RES_Aantal_EC_cumulatief, 0) - RES_Aantal_EC_tm_P7,
    RES_EC_perc_studielast = RES_Aantal_EC_totaal / OPL_Studielast_nominaal
  )


## Add first year variables
dfAS_full <- dfAS_full %>%
  left_join(
    dfFirst_year %>%
      select(INS_Studentnummer,
             INS_Opleidingsnaam_2002,
             INS_Verblijfsjaren_hoger_onderwijs_EJ,
             INS_Dagen_tussen_aanmelding_en_1_september_EJ),
    by = c('INS_Studentnummer', 'INS_Opleidingsnaam_2002'),
    relationship = "many-to-one"
  )




get_student_rank_in_cohort <- function(df, column) {
  result <- df %>%
    group_by(INS_Opleidingsnaam_2002, INS_Inschrijvingsjaar_EOI, INS_Studiejaar) %>%
    mutate(rank = rank({{column}}, ties.method = "min"),
           new = rank / n()) %>%
    ungroup()
  return(result$new)
}


## Rank students
dfAS_full$RES_rank_EC = get_student_rank_in_cohort(dfAS_full, RES_Aantal_EC_totaal)
dfAS_full$RES_rank_grade_p2 = get_student_rank_in_cohort(dfAS_full, RES_Gem_resultaat_tm_P2)
dfAS_full$RES_rank_avg_grade = get_student_rank_in_cohort(dfAS_full, RES_Gemiddeld_cijfer_cum_all)
dfAS_full$RES_rank_NG = get_student_rank_in_cohort(dfAS_full, RES_Aantal_vakken_niet_gehaald_cum)


dfAS_full <- dfAS_full %>%
  arrange(INS_Inschrijvingsjaar_EOI) %>%
  group_by(INS_Opleidingsnaam_2002) %>%
  fill(OPL_Studielast_nominaal, .direction = "down") %>%
  ungroup()

## Feature engineering
dfAS_full <- dfAS_full %>%
  ## Voeg kleine groepen van profielen samen
  mutate(INS_Vooropleiding_voor_HO_profiel_standaard = recode(INS_Vooropleiding_voor_HO_profiel_standaard,
                                                              "NG & CM" = "NG",
                                                              "NG & EM" = "NG",
                                                              "NT & EM" = "NT",
                                                              "NT & CM" = "NT"),
         ## Voeg kleine opleidingen samen TODO fix voor master
         # INS_Opleidingsnaam_2002 = recode(INS_Opleidingsnaam_2002,
         #                                  "B Archeologie" = "Comb oudheid",
         #                                  "B Griekse en Latijnse Taal en Cultuur" = "Comb oudheid",
         #                                  "B Oudheidwetenschappen" = "Comb oudheid",
         #                                  "B Natuur- en Sterrenkunde (joint degree)" = "Comb nask",
         #                                  "B Scheikunde (joint degree)" = "Comb nask",
         #                                  "B Theology and Religious Studies" = "comb theologie",
         #                                  "B Theologie (joint degree)" = "comb theologie"
         # ),
         ## Verdubbel EC deeltijdstudenten
         RES_Aantal_EC_tm_P2 = ifelse(INS_Opleidingsvorm_naam == "deeltijd",
                                      RES_Aantal_EC_tm_P2 * 2,
                                      RES_Aantal_EC_tm_P2),
         OPL_Studielast_nominaal = ifelse(
           INS_Opleidingsnaam_2002 %in% c("M Educatieve Master Primair Onderwijs (joint degree)",
                                          "M Tinbergen Institute Research Master in Economics (joint degree)"),
           120,
           OPL_Studielast_nominaal
         ),
         EC_to_go = OPL_Studielast_nominaal - RES_Aantal_EC_totaal,
         RES_gem_EC_per_jaar = RES_Aantal_EC_voor_jaar / (INS_Studiejaar - 1),
         INS_Studiejaar_relatief = INS_Studiejaar / OPL_Studielast_nominaal,
         INS_Vooropleiding_binnen_HO_jaar_diff = INS_Inschrijvingsjaar_EOI - INS_Vooropleiding_binnen_HO_jaar,
         ## If we have VOP_gemiddeld_beta, use that for wiskunde
         VOP_Cijfer_wiskunde = coalesce(VOP_Cijfer_wiskunde, VOP_Cijfer_gemiddeld_beta),
         VOP_Wiskundecijfer_cat = cut(VOP_Cijfer_wiskunde, c(0, seq(5.5, 8.5, 1), 10)),
         RES_Gem_resultaat_tm_voltooid_b_cat = cut(RES_Gem_resultaat_tm_voltooid_b, c(0, seq(6.5, 8.5, 1), 10)),
         RES_gem_EC_per_jaar_cat = cut(RES_gem_EC_per_jaar, c(0, 30, 45, 61, Inf)),
         SUC_Type_uitstroom_studiejaar = factor(SUC_Type_uitstroom_studiejaar),
         INS_Status_Zachte_knip = gsub(" [0-9]+", "", INS_Status_Zachte_knip),
         OPL_Cluster_Researchmaster = as.logical(OPL_Cluster_Researchmaster)
  )


## Include aanmeldingen
dfAanmeldingen <- dfAanmeldingen %>%
  mutate(Aanmeldingsjaar = INS_Inschrijvingsjaar - 1)

dfAanmeldingen_nieuwe_opl <- dfAanmeldingen %>%
  filter(AAN_Indicatie_EOI) %>%
  summarise(Aantal_aanmeldingen_totaal = n(),
            Aantal_aanmeldingen_afgewezen = sum(AAN_Status == "Afgewezen"),
            Aantal_aanmeldingen_gecreeerd = sum(AAN_Status == "Gecreeerd"),
            .by = c(INS_Studentnummer, Aanmeldingsjaar))

## Only really useful after May, noone registers before that
dfAanmeldingen_herinschrijvingen <- dfAanmeldingen %>%
  filter(AAN_Indicatie_eerdere_inschrijving == "Herinschrijver") %>%
  mutate(Herinschrijving_volgend_jaar = TRUE) %>%
  select(INS_Studentnummer, Aanmeldingsjaar, INS_Opleidingsnaam_2002, Herinschrijving_volgend_jaar)


dfAS_full <- dfAS_full %>%
  left_join(dfAanmeldingen_nieuwe_opl, by = c("INS_Studentnummer", "INS_Inschrijvingsjaar" = "Aanmeldingsjaar"),
            relationship = "many-to-one") %>%
  left_join(dfAanmeldingen_herinschrijvingen, by = c("INS_Studentnummer",
                                                     "INS_Inschrijvingsjaar" = "Aanmeldingsjaar",
                                                     "INS_Opleidingsnaam_2002"),
            relationship = "one-to-one")




## Behandel missende waardes

## Herkomst niet-NL en uitwonend NA dan uitwonend TRUE
dfAS_full <- dfAS_full %>%
  mutate(INS_Uitwonend = ifelse(is.na(INS_Uitwonend) & INS_IO_Herkomst_EER_naam != "Nederlands",
                                TRUE,
                                INS_Uitwonend))

vNA_Mode <- c("INS_Dagen_tussen_aanmelding_en_1_september_EJ",
              "INS_Soort_eerstejaars",
              "INS_Verblijfsjaren_hoger_onderwijs_EJ",
              "INS_Vooropleiding_binnen_HO_jaar",
              "OPL_Instructietaal",
              "INS_Vooropleiding_binnen_HO_jaar_diff")

vNA_Median <- c(
  "Perc_uitval_jaar_1_lag1",
  "Perc_nominaal_lag3",
  "Gem_resultaat_jaar_1_lag1",
  "Gem_EC_jaar_1_lag1",
  "RES_Gemiddeld_cijfer_cum_all",
  "RES_Gemiddeld_cijfer_cum_max_poging",
  "RES_Gemiddeld_poging_cum"
)

vNA_Median_VOP <- c()



## Fill NA's with standard values
dfAS_full <- dfAS_full %>%
  mutate(
    across(c(RES_Gem_resultaat_tm_P2), ~replace_na(.x, 1)),
    across(c(RES_Thesis_stage_behaald,  Herinschrijving_volgend_jaar), ~replace_na(.x, FALSE)),
    across(c(RES_Aantal_herkansingen_tm_P2, RES_Aantal_vakken_tm_P2, RES_Aantal_EC_tm_P2,
             RES_Aantal_no_shows_tm_P2, RES_Aantal_EC_totaal, RES_Gemiddeld_cijfer_cum_all,
             RES_Gemiddeld_cijfer_cum_max_poging,
             RES_EC_perc_studielast, RES_Aantal_herkansingen_totaal, RES_Aantal_EC_voor_jaar,
             RES_gem_EC_per_jaar, RES_Aantal_no_shows_cum, RES_Aantal_NVD_cum,
             RES_Aantal_vakken_niet_gehaald_cum, RES_Aantal_EC_excl_extracurriculair_cum,
             RES_Aantal_NVD_relative, RES_Gemiddeld_poging_cum, Aantal_aanmeldingen_totaal,
             Aantal_aanmeldingen_afgewezen, Aantal_aanmeldingen_gecreeerd, RES_Aantal_herkansingen_voor_jaar,
             RES_Aantal_eindresultaten_voor_jaar, RES_Aantal_no_shows_voor_jaar), ~replace_na(.x, 0)),
    EC_to_go = coalesce(EC_to_go, OPL_Studielast_nominaal))




## Maybe change imputed values based on other data (knn)? Also beware of data leakage
dfAS_full <- dfAS_full %>%
  vvfiller::fill_df_with_agg_by_group(
    group = c("INS_Opleidingsnaam_2002", "INS_Inschrijvingsjaar"),
    columns = c(vNA_Median, vNA_Median_VOP),
    overwrite_col = TRUE,
    statistic = median,
    fill_empty_group = TRUE) %>%
  vvfiller::fill_df_with_agg_by_group(
    group = c("INS_Opleidingsnaam_2002", "INS_Inschrijvingsjaar"),
    columns = vNA_Mode,
    overwrite_col = TRUE,
    statistic = vvconverter::mode,
    fill_empty_group = TRUE)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3. Modelleren ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
dfAS_full <- dfAS_full %>%
  select(-OPL_VU_alfa_beta_gamma)

dfAS_trainval <- dfAS_full %>%
  filter(INS_Inschrijvingsjaar %in% vTrain_years)

dfAS_test <- dfAS_full %>%
  filter(INS_Inschrijvingsjaar %in% vTest_years)

set.seed(10)
vClass_weights <- rev(dfAS_trainval %>%
                        count(SUC_Type_uitstroom_studiejaar) %>%
                        pull(n) / nrow(dfAS_trainval))

rf_model <- rand_forest(mode = "classification",
                        trees = trees,
                        mtry = mtry) %>%
  set_engine("ranger",
             class.weights = vClass_weights,
             max.depth = depth,
             always.split.variables = always_split_opl,
             num.threads = 8)


vRemovals <- c(
  "INS_Dagen_tussen_aanmelding_en_1_september",
  "RES_Gem_resultaat_tm_voltooid_b",
  "RES_Aantal_vakken_cum"
)

rf_recipe <-
  recipe(
    SUC_Type_uitstroom_studiejaar ~ .,
    data = dfAS_trainval) %>%
  step_novel(INS_Opleidingsnaam_2002,
             INS_Faculteit,
             INS_Status_Zachte_knip,
             OPL_Cluster,
             OPL_Instructietaal,
             DEM_Geslacht,
             INS_Hoogste_vooropleiding_soort_cat
  ) %>%
  step_mutate_at(INS_Uitwonend, INS_Direct, INS_Tussenjaar_voor_M, OPL_Cluster_Researchmaster, fn = as.character) %>%
  ## Slow, but improves results. Use carefully (max two numerical columns, depending on amount of NAs).
  step_impute_bag(VOP_Cijfer_gemiddeld,
                  trees = 25, seed_val = 23) %>%
  step_unknown(c(DEM_Geslacht,
                 INS_Opleidingsnaam_2002,
                 INS_Aansluiting_cat,
                 INS_Hoogste_vooropleiding_soort_cat,
                 INS_Direct,
                 INS_Tussenjaar_voor_M,
                 INS_Uitwonend,
                 INS_Vooropleiding_voor_HO_soort_SAP,
                 INS_Vooropleiding_voor_HO_profiel_standaard,
                 INS_Vooropleiding_voor_HO_VWO_examen_kans,
                 INS_Hoogste_vooropleiding_soort_cat,
                 INS_Status_Zachte_knip,
                 Premaster_grade_cat,
                 OPL_Cluster,
                 OPL_Cluster_Researchmaster,
                 VOP_Wiskundecijfer_cat,
                 RES_gem_EC_per_jaar_cat,
                 RES_Gem_resultaat_tm_voltooid_b_cat,
                 INS_Onderwijsherkomst)) %>%
  step_string2factor(all_nominal_predictors()) %>%
  step_rm(c(all_of(c(vRemovals, vFiltervariabelen, vExtravariabelen)), starts_with("WISK")))


rf_wflow <-
  workflow() %>%
  add_model(rf_model) %>%
  add_recipe(rf_recipe)


## Test

set.seed(10)
lAS_trainval_year_split <- dfAS_trainval %>%
  group_by(OPL_Studielast_nominaal > 60) %>%
  group_split()

## Split data into groups
lAS_test_year_split <- dfAS_test %>%
  group_by(OPL_Studielast_nominaal > 60) %>%
  group_split()

set.seed(652)
rf_fit <- map(lAS_trainval_year_split, ~fit(rf_wflow, .x))

dfResult_test_class <- rf_fit %>%
  map2(lAS_test_year_split, ~predict(.x, .y, type = "class"))

dfResult_test_prob <- rf_fit %>%
  map2(lAS_test_year_split, ~predict(.x, .y, type = "prob"))

## Bind both groups
dfCombined_test <- map2(lAS_test_year_split,
                      dfResult_test_prob,
                      ~cbind(.x, .y)) %>%
  bind_rows()
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## BEWAAR & RUIM OP ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

output <- "4. Analyses/Doorstroomprognose/Data/Resultaten/"
date <- gsub("-", "", Sys.Date())

write_file_proj(dfCombined_test,
                "doorstroom_predictions_M",
                base_dir = paste0(Sys.getenv("NETWORK_DIR"), "Output/"),
                dir = output,
                add_branch = TRUE,
                extensions = "csv")

write_file_proj(dfCombined_test,
                paste0("doorstroom_predictions_M_P",peilperiode, "_", date),
                base_dir = paste0(Sys.getenv("NETWORK_DIR"), "Output/"),
                dir = paste0(output, "Archief/"),
                add_branch = TRUE,
                extensions = "csv")

clear_script_objects()
