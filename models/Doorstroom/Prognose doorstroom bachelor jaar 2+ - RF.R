## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Prognose doorstroom bachelor jaar 2+ ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code voor Student Analytics Vrije Universiteit Amsterdam
## Copyright 2022 VU
## Web Page: http://www.vu.nl
## Contact: vu-analytics@vu.nl
## Verspreiding buiten de VU: Ja
##
## Doel: Voorspellen van doorstroom in jaar 2 en hoger bachelor na 1 februari
##
## Afhankelijkheden: Afhankelijkheid
##
## Datasets: AS, resultaten, UAS
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
## 16-02-2023: TK: Aanmaak bestand
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Settings
model = "B2"

source("models/Doorstroom/Doorstroom - config.R")

source("models/Doorstroom/helpers.R")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. INLEZEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(tidymodels)
library(zoo) ## Voor vullen vectors met lege waarden

Dates <- read_file_proj("ACA_Dates", dir = "2. Geprepareerde data")

## Variables included in model (some removed later, RES_variables added later)
vModelvariabelen <- c("DEM_Geslacht",
                      "DEM_Leeftijd_peildatum_1_oktober",

                      "HNP_Honours_programma_begonnen",

                      "INS_Aansluiting_cat",
                      "INS_Aantal_inschrijvingen_in_HO",
                      "INS_Dagen_tussen_aanmelding_en_1_september",
                      "INS_Faculteit",
                      "INS_Opleiding_uitgeloot_ja_nee",
                      "INS_Opleidingsnaam_2002",
                      "INS_Soort_eerstejaars",
                      "INS_Studiejaar",
                      "INS_Tussenjaren_tijdens_B",
                      "INS_Uitwonend",
                      "INS_Vooropleiding_voor_HO_soort_SAP",
                      "INS_Vooropleiding_voor_HO_profiel_standaard",
                      "INS_Vooropleiding_voor_HO_VWO_examen_kans",

                      ## MVR no longer exists
                      #"MVR_Ingeschreven_voor_hoogste_interesse",
                      #"MVR_OBK_Keuzezekerheid",


                      "OPL_VU_alfa_beta_gamma",
                      "OPL_Cluster",
                      #"OPL_Numerus_fixus_selectie",
                      #"OPL_Instructietaal",

                      "ORI_Orientatie_komt_voor",

                      "PUC_Deelgenomen",

                      "RES_Aantal_EC_tm_jaar_1",

                      "SUC_Type_uitstroom_studiejaar",
                      "VOP_Cijfer_gemiddeld",
                      "VOP_Cijfer_wiskunde")

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

vUVA_first_opleidingen = c(
  "B Natuur- en Sterrenkunde (joint degree)",
  "B Scheikunde (joint degree)",
  "M Chemistry (joint degree)",
  "M Physics and Astronomy (joint degree)",
  "M Computational Science (joint degree)"
)

dfAS_full_raw <- readrds_csv(output = "3. Analyseset/Analysis_set_1.fst", columns = c(vModelvariabelen, vFiltervariabelen, vExtravariabelen)) %>%
  filter(INS_Studiejaar > 1,
         INS_Inschrijvingsjaar %in% c(vTrain_years, vTest_years, vFeature_creation_years),
         INS_Opleidingsfase_BPM == "B",
         ## Filter out students who unregistered before February, as we predict in February
         month(INS_Datum_uitschrijving) > 1 & month(INS_Datum_uitschrijving) <= 8,
         INS_Hoofdneven == "Hoofdinschrijving",
         !INS_Opleidingsnaam_2002 %in% vUVA_first_opleidingen)


cFirstyearvariables <- c("BSA_Status_omschrijving_eindejaars",
                         "INS_Verblijfsjaren_hoger_onderwijs",
                         "INS_Dagen_tussen_aanmelding_en_1_september",
                         "INS_Studiejaar",
                         "INS_Opleidingsnaam_2002",
                         "SUC_Diploma_nominaal",
                         "RES_Aantal_no_shows_tm_P7")

dfFirst_year <- readrds_csv(output = "3. Analyseset/Analysis_set_1.fst", columns = c(vFiltervariabelen, cFirstyearvariables)) %>%
  filter(INS_Studiejaar == 1,
         INS_Opleidingsfase_BPM == 'B') %>%
  rename(BSA_Status_EJ = BSA_Status_omschrijving_eindejaars,
         INS_Dagen_tussen_aanmelding_en_1_september_EJ = INS_Dagen_tussen_aanmelding_en_1_september)

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
           INS_Faculteit != "AUC") %>%
    filter(AAN_Dagen_tot_1_sept >= nDagen_tot_1_sept) %>%
    filter(AAN_Dagen_tot_1_sept == min(AAN_Dagen_tot_1_sept),
           .by = c(INS_Studentnummer, INS_Opleidingsnaam_2002, INS_Opleidingsfase_BPM, INS_Inschrijvingsjaar)) %>%
    select(
      INS_Studentnummer,
      INS_Inschrijvingsjaar,
      INS_Opleidingsnaam_2002,
      AAN_Dagen_tot_1_sept,
      AAN_Indicatie_EOI,
      AAN_Indicatie_eerdere_inschrijving,
      AAN_Status,
      INS_Opleidingsfase_BPM
    ) %>%
    distinct()
}


## Map gives errors when using web drive
# dfAanmeldingen <- tibble()
# for (bestand in lAanmeldingen_bestandspaden) {
#   gc()
#   dfAanmeldingen <- bind_rows(test, read_files(bestand))
# }


dfAanmeldingen <-
  map_dfr(lAanmeldingen_bestandspaden, read_files) %>%
  filter(row_number() == 1,
         .by = c(INS_Studentnummer, INS_Opleidingsnaam_2002, INS_Inschrijvingsjaar))


## Alle Resultaten
dfPogingen_raw <- read_file_proj("RES_Pogingen_geprepareerd",
                                 dir = "3. Analyseset/03. Resultaten/") %>%
  filter(grepl("^B", INS_Opleidingsnaam_2002),
         RES_Beoordeling_soort == 1) %>%
  ## Quite some duplicates with differing bestanddatums
  select(-RES_Bestand_datum) %>%
  distinct() %>%
  ## For keeping track of specific results
  mutate(.id = row_number())


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
  "OPL_UAS_Opleiding_Jaar_NUM",
  "OPL_UAS_Opleidingsnaam_2002_CHA",
  "OPL_UAS_Opleiding_Fase_CHA",
  "INS_Aantal_Eerstejaars_studenten_INT",
  "INS_SUC_Diploma_nominaal_INT"
)

## Don't use opleidingen dataset yet, because of wrong values
# dfOpleidingen_raw <- readrds_csv(output = "3. Analyseset/Opleidingen_Analyseset_lang_na_stap_3.rds") %>%
#   select(vOpleidingvariabelen) %>%
#   filter(OPL_UAS_Opleiding_Fase_CHA == "Bachelor")

## OPLAS for NF data
dfOpleidingen_raw <- read_file_proj("OPLAS_VU",
                                   base_dir = Sys.getenv("OUTPUT_DIR"),
                                   add_branch = FALSE,
                                   dir = "_REPOSITORIES/opleidingen-analyseset/main/3. Analyseset/") %>%
  select(INS_Opleidingsnaam_2002, INS_Inschrijvingsjaar, OPL_Numerus_fixus_selectie,
         OPL_Instructietaal) %>%
  distinct() %>%
  filter(INS_Inschrijvingsjaar <= 2022) %>%
  filter(row_number() == 1, .by = c(INS_Inschrijvingsjaar, INS_Opleidingsnaam_2002))


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. BEWERKEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
dfAS_full <- tibble(dfAS_full_raw)
dfResultaten <- dfPogingen_raw %>%
  ## Filter out dupiclate results (registered for multiple RES_Inschrijving_soort)
  filter(any(!is.na(RES_Inschrijving_soort)) & !is.na(RES_Inschrijving_soort) |
           !any(!is.na(RES_Inschrijving_soort)),
         .by = c(INS_Studentnummer, INS_Opleidingsnaam_2002,
                 RES_Academisch_jaar_beoordeling)) %>%
  select(INS_Studentnummer, RES_Module_code, RES_Academisch_jaar_beoordeling, Periode,
         Beoordeling_numeriek, RES_Beoordeling, RES_Beoordeling_datum, RES_Studiepunten_behaald,
         RES_Poging_nummer, RES_Academische_periode_vak_omschrijving, INS_Opleidingsnaam_2002,
         RES_Studiepunten_geboekt, RES_Poging_nummer_tm_nu) %>%
  distinct() %>%
  mutate(RES_Max_poging_in_jaar = RES_Poging_nummer == max(RES_Poging_nummer),
         .by = c(INS_Studentnummer, INS_Opleidingsnaam_2002, RES_Academisch_jaar_beoordeling,
                 RES_Module_code))

dfUAS_Vakken <- tibble(dfUAS_Vakken_raw) %>%
  mutate(`Extra curriculair` = recode(`Extra curriculair`,
                                      "Ja" = TRUE,
                                      "Nee" = FALSE,
                                      .default = FALSE),
         `Extra curriculair` = replace_na(`Extra curriculair`, FALSE)) %>%
  mutate(`Extra curriculair` = any(`Extra curriculair`), .by = Code) %>%
  distinct() %>%
  filter(row_number() == 1, .by = Code)

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



## Add number of nominaal students
dfAS1_agg_nominaal <- dfFirst_year %>%
  group_by(INS_Inschrijvingsjaar, INS_Opleidingsnaam_2002) %>%
  summarise(Aantal_eerstejaars = n(),
            Aantal_nominaal = sum(SUC_Diploma_nominaal),
            Nominaal_perc = Aantal_nominaal / Aantal_eerstejaars) %>%
  group_by(INS_Opleidingsnaam_2002) %>%
  ## Lag to prevent looking into the future
  mutate(Nominaal_perc_lag2 = lag(Nominaal_perc, 2)) %>%
  fill(Nominaal_perc_lag2) %>%
  select(INS_Opleidingsnaam_2002, INS_Inschrijvingsjaar, Nominaal_perc_lag2)

dfAS_full <- dfAS_full %>%
  left_join(dfAS1_agg_nominaal, by = c("INS_Inschrijvingsjaar", "INS_Opleidingsnaam_2002")) %>%
  left_join(dfOpleidingen, by = c("INS_Opleidingsnaam_2002", "INS_Inschrijvingsjaar"),
            relationship = "many-to-one")


## Join course type for determining thesis courses
dfResultaten <- dfResultaten %>%
  left_join(dfUAS_Vakken, by = c("RES_Module_code" = "Code"), relationship = "many-to-one")

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

dfResultaten_aggr <- dfResultaten %>%
  filter(INS_Studentnummer %in% dfAS_full$INS_Studentnummer) %>%
  arrange(RES_Beoordeling_datum) %>%
  ## Custom determining of course period to be consistent throughout years
  mutate(RES_periode_eind = as.numeric(str_sub(RES_Academische_periode_vak_omschrijving, -1)),
         RES_periode_eind = ifelse(RES_periode_eind > Periode, Periode, RES_periode_eind),
         RES_Beoordeling_datum_md_str = str_sub(as.character(RES_Beoordeling_datum), 6),
         RES_Thesis_stage_behaald = ifelse(Type %in% c("Thesis", "Stage"),
                                           as.logical(RES_Studiepunten_behaald),
                                           FALSE),
         ## Determine whether student has failed course that probably can't be retaken that year
         ## TODO deal with 0 EC courses
         RES_Poging_2_and_fail = RES_Poging_nummer >= 2 & RES_Studiepunten_behaald == 0 &
           RES_Studiepunten_geboekt != 0) %>%
  ## Calculate variables up to specific course
  group_by(INS_Studentnummer, INS_Opleidingsnaam_2002) %>%
  mutate(RES_Gemiddeld_cijfer_cum_all = cumfunc_na.rm(Beoordeling_numeriek, cummean),
         RES_Gemiddeld_cijfer_cum_laatste_poging = cumfunc_na.rm(ifelse(RES_Max_poging_in_jaar,
                                                                  Beoordeling_numeriek,
                                                                  NA_real_),
                                                                 cummean),
         RES_Gemiddeld_poging_cum = cumfunc_na.rm(ifelse(RES_Max_poging_in_jaar,
                                                   RES_Poging_nummer_tm_nu,
                                                   NA_real_),
                                                  cummean),
         RES_Aantal_no_shows_cum = cumfunc_na.rm(RES_Beoordeling == 'NS', cumsum),
         RES_Aantal_NVD_cum = cumfunc_na.rm(RES_Beoordeling == 'NVD', cumsum),
         RES_Aantal_EC_totaal = cumfunc_na.rm(RES_Studiepunten_behaald, cumsum),
         RES_Aantal_herkansingen_totaal = cumfunc_na.rm(RES_Poging_nummer > 1, cumsum),
         RES_Aantal_vakken_cum = row_number(),
         RES_Aantal_vakken_niet_gehaald_cum = cumfunc_na.rm(RES_Studiepunten_behaald == 0 &
                                                            RES_Studiepunten_geboekt != 0 &
                                                       RES_Max_poging_in_jaar, cumsum),
         RES_Thesis_stage_behaald = cumfunc_na.rm(RES_Thesis_stage_behaald, cumany),
         RES_Aantal_EC_excl_extracurriculair_cum = cumfunc_na.rm(ifelse(`Extra curriculair`, 0,
                                                                 RES_Studiepunten_behaald), cumsum)) %>%
  ungroup() %>%
  ## Filter on pogingnummer so varying retake dates throughout the years don't affect results
  filter((RES_periode_eind <= peilperiode - 1 & RES_Poging_nummer <= 3) |
           (RES_periode_eind == peilperiode & RES_Poging_nummer <= 1),
         RES_Beoordeling_datum_md_str <= peildatum_beoordeling_md_str | month(RES_Beoordeling_datum) > 8) %>%
  ## Calculate variables within year, and keep previously calculated variables with last, aggregate to student/year level
  group_by(INS_Studentnummer, INS_Opleidingsnaam_2002, RES_Academisch_jaar_beoordeling) %>%
  summarise(RES_Aantal_EC_tm_P2 = sum(RES_Studiepunten_behaald, na.rm = TRUE),
            RES_Gem_resultaat_tm_P2 = mean(Beoordeling_numeriek[RES_Max_poging_in_jaar], na.rm = TRUE),
            RES_Aantal_no_shows_tm_P2 = sum(RES_Beoordeling == "NS"),
            RES_Aantal_herkansingen_tm_P2 = sum(RES_Poging_nummer > 1, na.rm = TRUE),
            RES_Poging_2_and_fail_any = any(RES_Poging_2_and_fail),
            across(c(RES_Thesis_stage_behaald, RES_Gemiddeld_cijfer_cum_all,
                   RES_Gemiddeld_cijfer_cum_laatste_poging, RES_Aantal_no_shows_cum,
                   RES_Aantal_NVD_cum, RES_Gemiddeld_poging_cum, RES_Aantal_EC_totaal,
                   RES_Aantal_herkansingen_totaal, RES_Aantal_vakken_cum, RES_Poging_2_and_fail_any,
                   RES_Aantal_vakken_niet_gehaald_cum, RES_Aantal_EC_excl_extracurriculair_cum),
                   .fns = ~last(.))) %>%
  ungroup() %>%
  mutate(RES_Aantal_NVD_relative = RES_Aantal_NVD_cum / RES_Aantal_vakken_cum) %>%
  ungroup()


dfAS_full <- dfAS_full %>%
  left_join(dfResultaten_aggr, by = c("INS_Studentnummer",
                                      "INS_Opleidingsnaam_2002",
                                      "INS_Inschrijvingsjaar" = "RES_Academisch_jaar_beoordeling"),
            relationship = "many-to-one")



## Add first year variables
dfFirst_year <- dfFirst_year %>%
  group_by(INS_Studentnummer, INS_Opleidingsnaam_2002) %>%
  mutate(RES_Aantal_no_shows_voor_jaar = cumsum(lag(RES_Aantal_no_shows_tm_P7, default = 0))) %>%
  ungroup()


dfAS_full <- dfAS_full %>%
  left_join(
    dfFirst_year %>%
      select(INS_Studentnummer,
             INS_Opleidingsnaam_2002,
             BSA_Status_EJ,
             INS_Dagen_tussen_aanmelding_en_1_september_EJ,
             RES_Aantal_no_shows_voor_jaar),
    by = c('INS_Studentnummer', 'INS_Opleidingsnaam_2002'),
    relationship = "many-to-one"
  )

## Add OPL variables

## Too many errors in OPL dataset. Use AS for now
# dfOpleidingen <- dfOpleidingen %>%
#   mutate(Nominaal_perc = INS_SUC_Diploma_nominaal_INT / INS_Aantal_Eerstejaars_studenten_INT) %>%
#   group_by(OPL_UAS_Opleidingsnaam_2002_CHA) %>%
#   mutate(Nominaal_perc_lag2 = lag(Nominaal_perc, 2)) %>%
#   ## If missing, use most recent
#   fill(Nominaal_perc_lag2, .direction = "down") %>%
#   select(OPL_UAS_Opleiding_Jaar_NUM, OPL_UAS_Opleidingsnaam_2002_CHA, Nominaal_perc_lag2)
#
# dfAS_full <- dfAS_full %>%
#   left_join(dfOpleidingen,
#             by = c("INS_Opleidingsnaam_2002" = OPL_UAS_Opleidingsnaam_2002_CHA))




get_student_rank_in_cohort <- function(df, column, ties = "min") {
  result <- df %>%
    group_by(INS_Opleidingsnaam_2002, INS_Eerste_jaar_opleiding_en_instelling) %>%
    mutate(rank = rank({{column}}, ties.method = ties),
           new = rank / n()) %>%
    ungroup()
  return(result$new)
}


## Rank students
dfAS_full$RES_rank_EC = get_student_rank_in_cohort(dfAS_full, RES_Aantal_EC_totaal)
dfAS_full$RES_rank_grade = get_student_rank_in_cohort(dfAS_full, RES_Gemiddeld_cijfer_cum_laatste_poging)
dfAS_full$RES_rank_NG = get_student_rank_in_cohort(dfAS_full, RES_Aantal_vakken_niet_gehaald_cum)


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
           INS_Eerste_jaar_opleiding_en_instelling %in% c(2020, 2021) ~ OPL_BSA_EC_eis_aug_jr1 - 6,
           INS_Eerste_jaar_opleiding_en_instelling == 2019 ~ 0,
           TRUE ~ OPL_BSA_EC_eis_aug_jr1
         ),
         RES_EC_perc_BSA = RES_Aantal_EC_cumulatief / OPL_BSA_EC_eis_aug_jr1,

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
         SUC_Type_uitstroom_studiejaar = factor(SUC_Type_uitstroom_studiejaar)
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
  select(INS_Studentnummer, Aanmeldingsjaar, INS_Opleidingsnaam_2002, Herinschrijving_volgend_jaar) %>%
  distinct()

dfAanmeldingen_master <- dfAanmeldingen %>%
  filter(INS_Opleidingsfase_BPM == "M") %>%
  mutate(Masteraanmelding = TRUE) %>%
  select(INS_Studentnummer, Aanmeldingsjaar, Masteraanmelding) %>%
  distinct()

dfAS_full <- dfAS_full %>%
  left_join(dfAanmeldingen_nieuwe_opl, by = c("INS_Studentnummer", "INS_Inschrijvingsjaar" = "Aanmeldingsjaar"),
            relationship = "many-to-one") %>%
  left_join(dfAanmeldingen_herinschrijvingen, by = c("INS_Studentnummer",
                                                     "INS_Inschrijvingsjaar" = "Aanmeldingsjaar",
                                                     "INS_Opleidingsnaam_2002"),
            relationship = "one-to-one") %>%
  left_join(dfAanmeldingen_master, by = c("INS_Studentnummer",
                                          "INS_Inschrijvingsjaar" = "Aanmeldingsjaar"),
            relationship = "many-to-one")

## Behandel missende waardes

## Herkomst niet-NL en uitwonend NA dan uitwonend TRUE
dfAS_full <- dfAS_full %>%
  mutate(INS_Uitwonend = ifelse(is.na(INS_Uitwonend) & !INS_IO_Herkomst_NL,
                                TRUE,
                                INS_Uitwonend))





## Fill NA"s with standard values
dfAS_full <- dfAS_full %>%
  mutate(
    across(c(RES_Gem_resultaat_tm_P2, RES_Gemiddeld_cijfer_cum_laatste_poging, RES_Gemiddeld_cijfer_cum_all), ~replace_na(.x, 1)),
    across(c(RES_Thesis_stage_behaald, RES_Poging_2_and_fail_any, Herinschrijving_volgend_jaar,
             Masteraanmelding), ~replace_na(.x, FALSE)),
    across(c(RES_Aantal_herkansingen_totaal, RES_Aantal_EC_tm_P2, RES_Aantal_no_shows_tm_P2,
             RES_Aantal_NVD_cum, RES_EC_perc_BSA, RES_Aantal_herkansingen_tm_P2, RES_Aantal_no_shows_cum,
             RES_Gemiddeld_poging_cum, RES_Aantal_EC_totaal, RES_Aantal_no_shows_voor_jaar,
             RES_Aantal_EC_tm_jaar_1, RES_Aantal_NVD_relative, RES_Aantal_vakken_niet_gehaald_cum,
             RES_Aantal_EC_excl_extracurriculair_cum, Aantal_aanmeldingen_gecreeerd, Aantal_aanmeldingen_afgewezen,
             Aantal_aanmeldingen_totaal),
             ~replace_na(.x, 0)))


## If we have VOP_gemiddeld_beta, use that for wiskunde
dfAS_full <- dfAS_full %>%
  mutate(VOP_Cijfer_wiskunde = coalesce(VOP_Cijfer_wiskunde, VOP_Cijfer_gemiddeld_beta))

## Impute by aggregate function per year and programme
vNA_Mode <- c("INS_Dagen_tussen_aanmelding_en_1_september",
              "INS_Dagen_tussen_aanmelding_en_1_september_EJ",
              "INS_Aantal_inschrijvingen_in_HO",
              "INS_Soort_eerstejaars", "DEM_Geslacht")

vNA_Median <- c("Nominaal_perc_lag2")

dfAS_full <- dfAS_full %>%
  vvfiller::fill_df_with_agg_by_group(
    group = c("INS_Opleidingsnaam_2002", "INS_Eerste_jaar_opleiding_en_instelling"),
    columns = c(vNA_Median),
    overwrite_col = TRUE,
    statistic = median,
    fill_empty_group = TRUE) %>%
  vvfiller::fill_df_with_agg_by_group(
    group = c("INS_Opleidingsnaam_2002", "INS_Eerste_jaar_opleiding_en_instelling"),
    columns = vNA_Mode,
    overwrite_col = TRUE,
    statistic = vvconverter::mode,
    fill_empty_group = TRUE)


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3. Modelleren ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

set.seed(3)
dfAS_train <- dfAS_full %>%
  filter(INS_Inschrijvingsjaar %in% vTrain_years)

dfAS_test <- dfAS_full %>%
  filter(INS_Inschrijvingsjaar %in% vTest_years)


vClass_weights <- rev(dfAS_train %>%
                        count(SUC_Type_uitstroom_studiejaar) %>%
                        pull(n) / nrow(dfAS_train))

rf_model <- rand_forest(mode = "classification",
                        trees = trees,
                        mtry = mtry) %>%
  set_engine("ranger",
             class.weights = vClass_weights,
             max.depth = depth,
             always.split.variables = always_split_opl,
             num.threads = 6)

vRemovals <- c(
  "RES_Aantal_vakken_cum"
)

rf_recipe <-
  recipe(
    SUC_Type_uitstroom_studiejaar ~ .,
    data = dfAS_train) %>%
  step_novel(INS_Opleidingsnaam_2002) %>%
  step_mutate_at(INS_Uitwonend, OPL_Numerus_fixus_selectie, fn = as.character) %>%
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
  step_string2factor(all_nominal_predictors()) %>%
  step_rm(c(all_of(c(vRemovals, vFiltervariabelen, vExtravariabelen))))


rf_wflow <-
  workflow() %>%
  add_model(rf_model) %>%
  add_recipe(rf_recipe)


## Test
set.seed(652)
rf_fit_test <- fit(rf_wflow, dfAS_train)


dfResult_test <- rf_fit_test %>%
  predict(dfAS_test, type = "class") %>%
  cbind(predict(rf_fit_test, dfAS_test, type = "prob"))

dfCombined_test <- dfAS_test %>%
  cbind(dfResult_test)


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## BEWAAR & RUIM OP ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

output <- "4. Analyses/Doorstroomprognose/Data/Resultaten/"
date <- gsub("-", "", Sys.Date())

write_file_proj(dfCombined_test,
                "doorstroom_predictions_B2+",
                base_dir = paste0(Sys.getenv("NETWORK_DIR"), "Output/"),
                dir = output,
                add_branch = TRUE,
                extensions = "csv")

write_file_proj(dfCombined_test,
                paste0("doorstroom_predictions_B2+_", date),
                base_dir = paste0(Sys.getenv("NETWORK_DIR"), "Output/"),
                dir = paste0(output, "Archief/"),
                add_branch = TRUE,
                extensions = "csv")

clear_script_objects()

