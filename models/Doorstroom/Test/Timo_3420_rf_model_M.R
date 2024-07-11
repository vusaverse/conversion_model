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

one_row_per_student = FALSE

source("04. Analyseren/01. Structurele analyses/Inschrijvingenprognose/Random Forest/Doorstroom - config.R")

source("04. Analyseren/01. Structurele analyses/Inschrijvingenprognose/Random Forest/helpers.R")

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
                      "INS_Aantal_inschrijvingen_in_HO",
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
                      "INS_Vooropleiding_binnen_HO_sector",

                      "OPL_VU_alfa_beta_gamma",
                      "OPL_Cluster",
                      "OPL_Instructietaal",
                      "OPL_Cluster_Researchmaster",
                      "OPL_Studielast_nominaal",

                      "ORI_Orientatie_komt_voor",
                      "ORI_Orientatie_vooraf_master_aanwezig",
                      "ORI_Orientatie_vooraf_verschillend_totaal",


                      "SUC_Type_uitstroom_studiejaar",

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
                      "RES_Gem_resultaat_tm_jaar_1",
                      "RES_Gem_resultaat_tm_jaar_2",
                      "RES_Gem_resultaat_tm_jaar_3",
                      "RES_Gem_resultaat_tm_jaar_4",
                      "RES_Gem_resultaat_tm_jaar_5",
                      "RES_Gem_resultaat_tm_jaar_6",
                      "RES_Gem_resultaat_tm_jaar_7",
                      "RES_Gem_resultaat_tm_jaar_8")


dfAS_raw <- readrds_csv(output = "3. Analyseset/Analysis_set_1.fst", columns = c(vModelvariabelen, vFiltervariabelen, vExtravariabelen)) %>%
  filter(INS_Inschrijvingsjaar %in% c(vTest_years, vTrain_years),
    INS_Opleidingsfase_BPM == "M",
    !grepl("joint degree", INS_Opleidingsnaam_2002),
    ## TODO: Check if this influences herinschrijvers, maybe filter later
    month(INS_Datum_uitschrijving) > 1 & month(INS_Datum_uitschrijving) <= 8)


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


## Alle Resultaten
dfPogingen_raw <- readrds_csv(output = "3. Analyseset/03. Resultaten/RES_Pogingen_geprepareerd.rds") %>%
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
  #"OPL_OPL_Nominale_studieduur_NUM",
  "OPL_UAS_Opleiding_Jaar_NUM",
  "OPL_UAS_Opleidingsnaam_2002_CHA",
  "INS_Aantal_Eerstejaars_studenten_INT",
  "INS_SUC_Diploma_nominaal_INT",
  "INS_SUC_Aantal_Uitval_na_jaar_1_INT",
  "INS_RES_Gemiddeld_resultaat_jr1",
  "INS_RES_Gemiddeld_EC_jr1"
)

dfOpleidingen_raw <- readrds_csv(output = "3. Analyseset/Opleidingen_Analyseset_lang_na_stap_3.rds")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. BEWERKEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Make an actual copy (tracemem(dfAS_full)==tracemem(dfAS_raw) equals FALSE)
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

dfUAS_Vakken <- tibble(dfUAS_Vakken_raw)


dfUAS_Vakken <- tibble(dfUAS_Vakken_raw) %>%
  mutate(`Extra curriculair` = recode(`Extra curriculair`,
                                      "Ja" = TRUE,
                                      "Nee" = FALSE),
         `Extra curriculair` = replace_na(`Extra curriculair`, FALSE))

dfOpleidingen <- dfOpleidingen_raw %>%
  select(all_of(vOpleidingvariabelen)) %>%
  distinct()



## If years are mixed in training data, we can't just use lag 1 (or 2)
## TODO make sure test set uses more recent values
if (one_row_per_student) {
  dfOpleidingen <- dfOpleidingen %>%
    filter(OPL_UAS_Opleiding_Jaar_NUM == start_year - 1) %>%
    mutate(Perc_uitval_jaar1 = INS_SUC_Aantal_Uitval_na_jaar_1_INT / INS_Aantal_Eerstejaars_studenten_INT,
           Perc_nominaal = INS_SUC_Diploma_nominaal_INT / INS_Aantal_Eerstejaars_studenten_INT) %>%
    select(-c(INS_SUC_Aantal_Uitval_na_jaar_1_INT, INS_SUC_Diploma_nominaal_INT,
              INS_Aantal_Eerstejaars_studenten_INT, OPL_UAS_Opleiding_Jaar_NUM))

  dfAS_full <- dfAS_full %>%
    strict_left_join(dfOpleidingen,
                     by = c("INS_Opleidingsnaam_2002" = "OPL_UAS_Opleidingsnaam_2002_CHA"))
} else {
  dfOpleidingen <- dfOpleidingen %>%
    mutate(Perc_uitval_jaar1 = INS_SUC_Aantal_Uitval_na_jaar_1_INT / INS_Aantal_Eerstejaars_studenten_INT,
           Perc_nominaal = INS_SUC_Diploma_nominaal_INT / INS_Aantal_Eerstejaars_studenten_INT) %>%
    select(-c(INS_SUC_Aantal_Uitval_na_jaar_1_INT, INS_SUC_Diploma_nominaal_INT, INS_Aantal_Eerstejaars_studenten_INT)) %>%
    group_by(OPL_UAS_Opleidingsnaam_2002_CHA) %>%
    mutate(across(c(Perc_uitval_jaar1,
                  INS_RES_Gemiddeld_resultaat_jr1,
                  INS_RES_Gemiddeld_EC_jr1), ~lag(.x, 1, order_by = OPL_UAS_Opleiding_Jaar_NUM)),
           # TODO base amount of lag on nominale studieduur
           Perc_nominaal = lag(Perc_nominaal,
                                              n = 2,
                                              order_by = OPL_UAS_Opleiding_Jaar_NUM)) %>%
    ungroup()

  dfAS_full <- dfAS_full %>%
    strict_left_join(dfOpleidingen,
                     by = c("INS_Opleidingsnaam_2002" = "OPL_UAS_Opleidingsnaam_2002_CHA",
                            "INS_Inschrijvingsjaar" = "OPL_UAS_Opleiding_Jaar_NUM"))
}


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
  strict_left_join(dfBachelors, by = "INS_Studentnummer") %>%
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
  strict_left_join(dfPremasters,
                   by = 'INS_Studentnummer')


## RES_variables before year
dfAS_full <- dfAS_full %>%
  group_by(INS_Studentnummer, INS_Opleidingsnaam_2002) %>%
  mutate(RES_Aantal_herkansingen_voor_jaar = cumsum(lag(RES_Aantal_herkansingen_tm_P7, default = 0)),
         RES_Aantal_eindresultaten_voor_jaar = cumsum(lag(RES_Aantal_eindresultaten_tm_P7, default = 0)),
         RES_Aantal_no_shows_voor_jaar = cumsum(lag(RES_Aantal_no_shows_tm_P7, default = 0))) %>%
  ungroup() %>%
  mutate(
    RES_Gem_cijfer_tm_jaar = case_when(
      INS_Studiejaar == 1 ~ RES_Gem_resultaat_tm_jaar_1,
      INS_Studiejaar == 2 ~ RES_Gem_resultaat_tm_jaar_2,
      INS_Studiejaar == 3 ~ RES_Gem_resultaat_tm_jaar_3,
      INS_Studiejaar == 4 ~ RES_Gem_resultaat_tm_jaar_4,
      INS_Studiejaar == 5 ~ RES_Gem_resultaat_tm_jaar_5,
      INS_Studiejaar == 6 ~ RES_Gem_resultaat_tm_jaar_6,
      INS_Studiejaar == 7 ~ RES_Gem_resultaat_tm_jaar_7,
      INS_Studiejaar == 8 ~ RES_Gem_resultaat_tm_jaar_8,
      TRUE ~ RES_Gem_resultaat_tm_jaar_8)) %>%
  group_by(INS_Studentnummer, INS_Opleidingsnaam_2002) %>%
  mutate(
    RES_Gem_cijfer_voor_jaar = lag(RES_Gem_cijfer_tm_jaar)
  ) %>%
  ungroup() %>%
  select(-RES_Gem_cijfer_tm_jaar)


dfResultaten <- dfResultaten %>%
  strict_left_join(dfUAS_Vakken, by = c("RES_Module_code" = "Code"))

## RES_ tm P2, for consistent determination of period
dfResultaten_aggr <- dfResultaten %>%
  arrange(RES_Beoordeling_datum) %>%
  mutate(RES_periode_eind = as.numeric(str_sub(RES_Academische_periode_vak_omschrijving, -1)),
         RES_periode_eind = ifelse(RES_periode_eind > Periode, Periode, RES_periode_eind),
         RES_Beoordeling_datum_md_str = str_sub(as.character(RES_Beoordeling_datum), 6),
         RES_Thesis_stage_behaald = ifelse(Type %in% c("Thesis", "Stage"),
                                           as.logical(RES_Studiepunten_behaald),
                                           NA)) %>%
  group_by(INS_Studentnummer, INS_Opleidingsnaam_2002, RES_Academisch_jaar_beoordeling, RES_Module_code) %>%
  filter(RES_Poging_nummer == max(RES_Poging_nummer)) %>%
  ungroup() %>%
  group_by(INS_Studentnummer, INS_Opleidingsnaam_2002) %>%
  mutate(RES_Gemiddeld_cijfer_cum = cumfunc_na.rm(Beoordeling_numeriek, cummean),
         RES_Gemiddeld_poging_cum = cumfunc_na.rm(RES_Poging_nummer_tm_nu, cummean),
         RES_Aantal_no_shows_cum = cumfunc_na.rm(RES_Beoordeling == 'NS', cumsum),
         RES_Aantal_NVD_cum = cumfunc_na.rm(RES_Beoordeling == 'NVD', cumsum),
         RES_Aantal_EC_totaal = cumfunc_na.rm(RES_Studiepunten_behaald, cumsum),
         RES_Aantal_herkansingen_totaal = cumfunc_na.rm(RES_Poging_nummer > 1, cumsum),
         RES_Aantal_vakken_cum = row_number(),
         RES_Aantal_vakken_niet_gehaald_cum = cumfunc_na.rm(RES_Studiepunten_behaald == 0 &
                                                       RES_Studiepunten_geboekt != 0, cumsum),
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
            across(c(RES_Thesis_stage_behaald, RES_Gemiddeld_cijfer_cum, RES_Aantal_no_shows_cum,
                   RES_Aantal_NVD_cum, RES_Gemiddeld_poging_cum, RES_Aantal_EC_totaal,
                   RES_Aantal_herkansingen_totaal, RES_Aantal_vakken_cum,
                   RES_Aantal_vakken_niet_gehaald_cum, RES_Aantal_EC_excl_extracurriculair_cum), ~last(.x))) %>%
  ungroup() %>%
  mutate(RES_Aantal_NVD_relative = RES_Aantal_NVD_cum / RES_Aantal_vakken_cum)


dfAS_full <- dfAS_full %>%
  left_join(dfResultaten_aggr, by = c("INS_Studentnummer",
                                      "INS_Opleidingsnaam_2002",
                                      "INS_Inschrijvingsjaar" = "RES_Academisch_jaar_beoordeling"))


## Overige RES_ toevoegen
dfAS_full <- dfAS_full %>%
  mutate(
    RES_Aantal_EC_voor_jaar = ifelse(!is.na(RES_Aantal_EC_cumulatief), RES_Aantal_EC_cumulatief, 0) - RES_Aantal_EC_tm_P7,
    #RES_Aantal_EC_totaal = RES_Aantal_EC_voor_jaar + RES_Aantal_EC_tm_P2,
    #RES_Aantal_herkansingen_totaal2 = RES_Aantal_herkansingen_voor_jaar + RES_Aantal_herkansingen_tm_P2,
    #RES_Aantal_no_shows_totaal = RES_Aantal_no_shows_voor_jaar + RES_Aantal_no_shows_tm_P2,
    RES_EC_perc_studielast = RES_Aantal_EC_totaal / OPL_Studielast_nominaal
  )


## Add first year variables
dfAS_full <- dfAS_full %>%
  strict_left_join(
    dfFirst_year %>%
      select(INS_Studentnummer,
             INS_Opleidingsnaam_2002,
             INS_Verblijfsjaren_hoger_onderwijs_EJ,
             INS_Dagen_tussen_aanmelding_en_1_september_EJ),
    by = c('INS_Studentnummer', 'INS_Opleidingsnaam_2002')
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
dfAS_full$RES_rank_avg_grade = get_student_rank_in_cohort(dfAS_full, RES_Gemiddeld_cijfer_cum)
dfAS_full$RES_rank_NG = get_student_rank_in_cohort(dfAS_full, RES_Aantal_vakken_niet_gehaald_cum)


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
         INS_Vooropleiding_binnen_HO_sector = gsub("^[wo|hbo](.*?) ", "", INS_Vooropleiding_binnen_HO_sector),
         ## Verdubbel EC deeltijdstudenten
         RES_Aantal_EC_tm_P2 = ifelse(INS_Opleidingsvorm_naam == "deeltijd",
                                      RES_Aantal_EC_tm_P2 * 2,
                                      RES_Aantal_EC_tm_P2),
         EC_to_go = OPL_Studielast_nominaal - RES_Aantal_EC_totaal,
         RES_gem_EC_per_jaar = RES_Aantal_EC_voor_jaar / (INS_Studiejaar - 1),
         INS_Studiejaar_relatief = INS_Studiejaar / OPL_Studielast_nominaal,
         INS_Vooropleiding_binnen_HO_jaar_diff = INS_Inschrijvingsjaar_EOI - INS_Vooropleiding_binnen_HO_jaar,
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
         VOP_Wiskundecijfer_cat = cut(VOP_Cijfer_wiskunde, c(0, seq(5.5, 8.5, 1), 10)),
         VOP_Gemiddelde_cijfer_cat = cut(VOP_Cijfer_gemiddeld, c(0, seq(6.5, 8.5, 1), 10)),
         RES_Gem_resultaat_tm_voltooid_b_cat = cut(RES_Gem_resultaat_tm_voltooid_b, c(0, seq(6.5, 8.5, 1), 10)),
         RES_gem_EC_per_jaar_cat = cut(RES_gem_EC_per_jaar, c(0, 30, 45, 61, Inf)),
         SUC_Type_uitstroom_studiejaar = factor(SUC_Type_uitstroom_studiejaar),
         INS_Status_Zachte_knip = gsub(" [0-9]+", "", INS_Status_Zachte_knip)
  )


## Behandel missende waardes

## Herkomst niet-NL en uitwonend NA dan uitwonend TRUE
dfAS_full <- dfAS_full %>%
  mutate(INS_Uitwonend = ifelse(is.na(INS_Uitwonend) & INS_IO_Herkomst_EER_naam != "Nederlands",
                                TRUE,
                                INS_Uitwonend))

vNA_Mode <- c("INS_Dagen_tussen_aanmelding_en_1_september",
              "INS_Dagen_tussen_aanmelding_en_1_september_EJ",
              "INS_Aantal_inschrijvingen_in_HO",
              "INS_Soort_eerstejaars",
              "INS_Verblijfsjaren_hoger_onderwijs_EJ",
              "INS_Vooropleiding_binnen_HO_jaar",
              "OPL_Instructietaal",
              "INS_Vooropleiding_binnen_HO_jaar_diff")

vNA_Median <- c(
  "Perc_uitval_jaar1",
  "Perc_nominaal",
  "INS_RES_Gemiddeld_resultaat_jr1",
  "INS_RES_Gemiddeld_EC_jr1",
  "RES_Gemiddeld_cijfer_cum",
  "RES_Gemiddeld_poging_cum"
)

vNA_Median_VOP <- c()
  #"VOP_Cijfer_gemiddeld_alfa",
  #"VOP_Cijfer_gemiddeld_gamma",
  #"VOP_Cijfer_wiskunde",
  #"VOP_Cijfer_gemiddeld")


## Fill NA's with standard values
dfAS_full <- dfAS_full %>%
  mutate(
    across(c(RES_Gem_resultaat_tm_P2, RES_Gemiddeld_poging_cum), ~replace_na(.x, 1)),
    across(c(RES_Thesis_stage_behaald), ~replace_na(.x, FALSE)),
    across(c(RES_Aantal_herkansingen_tm_P2, RES_Aantal_vakken_tm_P2, RES_Aantal_EC_tm_P2,
             RES_Aantal_no_shows_tm_P2, RES_Aantal_EC_totaal, RES_Gemiddeld_cijfer_cum,
             RES_EC_perc_studielast, RES_Aantal_herkansingen_totaal, RES_Aantal_EC_voor_jaar,
             RES_gem_EC_per_jaar, RES_Aantal_no_shows_cum, RES_Aantal_NVD_cum, RES_Aantal_vakken_cum,
             RES_Aantal_vakken_niet_gehaald_cum, RES_Aantal_EC_excl_extracurriculair_cum,
             RES_Aantal_NVD_relative), ~replace_na(.x, 0)),
    EC_to_go = coalesce(EC_to_go, OPL_Studielast_nominaal))


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


set.seed(452)
if (one_row_per_student) {
  dfAS_full <- dfAS_full %>%
    group_by(INS_Studentnummer) %>%
    slice_sample(n = 1) %>%
    ungroup
}

date <- date <- gsub("-", "", Sys.Date())
filename <- paste0("dfAS_prepped_M_P", peilperiode, "_", date)

write_file(dfAS_full, filename, destination = "4. Analyses/Doorstroomprognose/Data/Geprepareerd/M/",
           save_rds = TRUE)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3. Modelleren ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

test_cutoff <- 2021

dfAS_trainval <- dfAS_full %>%
  filter(INS_Inschrijvingsjaar < test_cutoff,
         INS_Inschrijvingsjaar != 2019)

dfAS_test <- dfAS_full %>%
  filter(INS_Inschrijvingsjaar >= test_cutoff,
         INS_Inschrijvingsjaar != 2023)


set.seed(10)
## Only use trainset for all models
vClass_weights <- rev(dfAS_trainval %>%
                        count(SUC_Type_uitstroom_studiejaar) %>%
                        pull(n) / nrow(dfAS_trainval))

rf_model <- rand_forest(mode = "classification",
                        trees = 993,
                        mtry = 6) %>%
  set_engine("ranger",
             class.weights = vClass_weights,
             max.depth = 19,
             always.split.variables = c("INS_Opleidingsnaam_2002"),
             num.threads = 8)


vRemovals <- c(
  "INS_Aantal_inschrijvingen_in_HO",
  "INS_Dagen_tussen_aanmelding_en_1_september",
  "VOP_Cijfer_gemiddeld_alfa",
  "VOP_Cijfer_gemiddeld_gamma",
  "VOP_Cijfer_gemiddeld_beta",
  "VOP_Gemiddelde_cijfer_cat",
  "VOP_Cijfer_wiskunde_alpha",
  "VOP_Cijfer_wiskunde_beta",
  "VOP_Cijfer_wiskunde_a",
  "VOP_Cijfer_wiskunde_b",
  "VOP_Cijfer_wiskunde_c",
  "VOP_Cijfer_wiskunde_d",
  "VOP_Cijfer_wiskunde",
  "RES_Gem_resultaat_tm_voltooid_b",
  "RES_Gem_cijfer_voor_jaar",
  #"RES_Aantal_no_shows_voor_jaar",
  #"RES_Aantal_no_shows_totaal",
  "VOP_Wiskundesoort",
  "INS_Vooropleiding_binnen_HO_sector",
  #"Perc_uitval_jaar1",
  #"Perc_nominaal",
  #"INS_RES_Gemiddeld_resultaat_jr1"
  #"INS_RES_Gemiddeld_EC_jr1"

  #"RES_Aantal_EC_excl_extracurriculair_cum",
  #"RES_Aantal_vakken_niet_gehaald_cum",
  "RES_Aantal_vakken_cum"
  #"RES_Aantal_NVD_relative"
)


vRemovalsTest <- c(
  "doorstroom_laag_bsa_2013",
  "uitval_2013",
  "gemEC_2013",
  "gemEC_2013_P2"
)

vRemovalsVal <- c(
  "doorstroom_laag_bsa_prev_year",
  "uitval_prev_year",
  "gemEC_prev_year",
  "gemEC_prev_year_P2"
)

rf_recipe_base <-
  recipe(
    SUC_Type_uitstroom_studiejaar ~ .,
    data = dfAS_trainval) %>%
  step_novel(INS_Opleidingsnaam_2002,
             INS_Faculteit,
             INS_Status_Zachte_knip,
             OPL_Cluster,
             OPL_Instructietaal,
             VOP_Wiskundesoort,
             INS_Vooropleiding_binnen_HO_sector,
             DEM_Geslacht,
             INS_Hoogste_vooropleiding_soort_cat
  ) %>%
  step_mutate_at(INS_Uitwonend, INS_Direct, INS_Tussenjaar_voor_M, fn = as.character) %>%
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
                 INS_Status_Zachte_knip,
                 Premaster_grade_cat,
                 INS_Vooropleiding_binnen_HO_sector,
                 OPL_Cluster,
                 OPL_VU_alfa_beta_gamma,
                 VOP_Wiskundesoort,
                 VOP_Wiskundecijfer_cat,
                 VOP_Gemiddelde_cijfer_cat,
                 RES_gem_EC_per_jaar_cat,
                 #RES_Gem_cijfer_cat,
                 RES_Gem_resultaat_tm_voltooid_b_cat,
                 INS_Onderwijsherkomst)) %>%
  step_string2factor(all_nominal_predictors()) %>%
  step_rm(c(all_of(c(vRemovals, vFiltervariabelen, vExtravariabelen)), starts_with("WISK")))

rf_recipe_val <- rf_recipe_base #%>%
#step_rm(all_of(vRemovalsVal))

rf_recipe_test <- rf_recipe_base# %>%
# step_rm(all_of(vRemovalsTest))

rf_wflow_val <-
  workflow() %>%
  add_model(rf_model) %>%
  add_recipe(rf_recipe_base)


rf_wflow_test <-
  workflow() %>%
  add_model(rf_model) %>%
  add_recipe(rf_recipe_test)

# Train/val
keep_pred <- control_resamples(save_pred = TRUE)

set.seed(10)
lAS_trainval_year_split <- dfAS_trainval %>%
  group_by(OPL_Studielast_nominaal > 60) %>%
  group_split()




## Split data in groups according to variable
lData_folds <- map(lAS_trainval_year_split, ~vfold_cv(.x, v = 6, strata = SUC_Type_uitstroom_studiejaar))
lFits <- map(lData_folds, ~fit_resamples(rf_wflow_val, .x, control = keep_pred))
result_val <- lFits %>%
  map(collect_predictions)
combined_val <- map2(lAS_trainval_year_split,
                     result_val,
                     ~cbind(.x %>%
                              select(-SUC_Type_uitstroom_studiejaar),
                            arrange(.y, .row))) %>%
  bind_rows()

## Metrics
combined_val %>%
  summarise(uitvalp = sum(.pred_Uitval),
            uitvalt = sum(SUC_Type_uitstroom_studiejaar == 'Uitval'),
            nog_studerendp = sum(`.pred_Nog studerend`),
            nog_studerendt = sum(SUC_Type_uitstroom_studiejaar == 'Nog studerend'),
            diplomap = sum(.pred_Diploma),
            diplomat = sum(SUC_Type_uitstroom_studiejaar == 'Diploma'))

combined_val %>%
  group_by(SUC_Type_uitstroom_studiejaar) %>%
  accuracy(truth = SUC_Type_uitstroom_studiejaar, estimate = .pred_class)


combined_val %>%
  roc_auc(truth = SUC_Type_uitstroom_studiejaar,
          estimate = c(.pred_Diploma, `.pred_Nog studerend`, .pred_Uitval),
          estimator = "hand_till")


## Smape
combined_val %>%
  group_by(INS_Opleidingsnaam_2002) %>%
  summarise(truth_doorstroom = sum(SUC_Type_uitstroom_studiejaar != 'Uitval'),
            predicted_doorstroom = sum(1 - .pred_Uitval)) %>%
  ungroup() %>%
  filter(truth_doorstroom > 25) %>%
  smape(truth = truth_doorstroom, estimate = predicted_doorstroom)


## Test
set.seed(10)
## Split data into groups
lAS_test_year_split <- dfAS_test %>%
  group_by(OPL_Studielast_nominaal > 60) %>%
  group_split()

set.seed(652)
rf_fit_test <- map(lAS_trainval_year_split, ~fit(rf_wflow_test, .x))

result_test_class <- rf_fit_test %>%
  map2(lAS_test_year_split, ~predict(.x, .y, type = "class"))

result_test_prob <- rf_fit_test %>%
  map2(lAS_test_year_split, ~predict(.x, .y, type = "prob"))

## Bind both groups
combined_test <- map2(lAS_test_year_split,
                     result_test_prob,
                     ~cbind(.x, .y)) %>%
  bind_rows()

## Metrics
combined_test %>%
  roc_auc(truth = SUC_Type_uitstroom_studiejaar,
          estimate = c(.pred_Diploma, `.pred_Nog studerend`, .pred_Uitval),
          estimator = "hand_till")

## Get smape
combined_test %>%
  group_by(INS_Opleidingsnaam_2002) %>%
  summarise(truth_doorstroom = sum(SUC_Type_uitstroom_studiejaar != 'Uitval'),
            predicted_doorstroom = sum(1 - .pred_Uitval)) %>%
  ungroup() %>%
  filter(truth_doorstroom > 25) %>%
  smape(truth = truth_doorstroom, estimate = predicted_doorstroom)

## Combine Train/val/test
combined_full <- combined_val %>%
  select(names(combined_test)) %>%
  rbind(combined_test) %>%
  mutate(Testset = ifelse(INS_Inschrijvingsjaar >= test_cutoff, TRUE, FALSE))


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## BEWAAR & RUIM OP ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

date <- gsub("-", "", Sys.Date())

#saverds_csv(dfAS_full, "dfAS_prepped_m", dataloc = "Tableau/205 VU-Inschrijvingen prognose 2021/Random Forest/Data/M/", save_csv = TRUE)
#
# saverds_csv(combined_val, "rf_results", dataloc = "Tableau/205 VU-Inschrijvingen prognose 2021/Random Forest/Data/", save_csv = TRUE)
# saverds_csv(combined_val, paste0("rf_results", date), dataloc = "Tableau/205 VU-Inschrijvingen prognose 2021/Random Forest/Data/Archive/", save_csv = TRUE)
#
# saverds_csv(combined_test, "rf_test_results", dataloc = "Tableau/205 VU-Inschrijvingen prognose 2021/Random Forest/Data/", save_csv = TRUE)
#
#saverds_csv(combined_full, "rf_full_results_m", dataloc = "Tableau/205 VU-Inschrijvingen prognose 2021/Random Forest/Data/M/", save_csv = TRUE)
# saverds_csv(combined_full, "rf_full_results", dataloc = "Tableau/205 VU-Inschrijvingen prognose 2021/Random Forest/Data/Archive/", save_csv = TRUE)
#
# saverds_csv(rf_fit_test, "rf_model_test", dataloc = "Tableau/205 VU-Inschrijvingen prognose 2021/Random Forest/Data/")

#clear_script_objects()

