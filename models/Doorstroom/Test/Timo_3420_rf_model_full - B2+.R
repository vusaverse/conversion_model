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

reload_files <- FALSE

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. INLEZEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Lees alle benodigde bestanden in:
library(tidymodels)

source("04. Analyseren/01. Structurele analyses/Inschrijvingenprognose/Random Forest/helpers.R")

if (reload_files) {
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
           INS_Inschrijvingsjaar >= 2017,
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
  #nDagen_tot_1_sept <- as.numeric(as.Date(paste0(year(today()), "-09-01")) - today())
  ## 1 mei
  nDagen_tot_1_sept <- as.numeric(as.Date("2024-09-01") - as.Date("2024-05-02"))

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
        INS_Opleidingsfase_BPM,
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


# vOpleidingvariabelen <- c(
#   "OPL_UAS_Opleiding_Jaar_NUM",
#   "OPL_UAS_Opleidingsnaam_2002_CHA",
#   "OPL_UAS_Opleiding_Fase_CHA",
#   "INS_Aantal_Eerstejaars_studenten_INT",
#   "INS_SUC_Diploma_nominaal_INT"
# )

## Don't use opleidingen dataset yet, because of wrong values
# dfOpleidingen_raw <- readrds_csv(output = "3. Analyseset/Opleidingen_Analyseset_lang_na_stap_3.rds") %>%
#   select(vOpleidingvariabelen) %>%
#   filter(OPL_UAS_Opleiding_Fase_CHA == "Bachelor")
}
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
         `Extra curriculair` = replace_na(`Extra curriculair`, FALSE))

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
  strict_left_join(dfUAS_Vakken, by = c("RES_Module_code" = "Code"))

## Date of prognosis
#peildatum <- today()
peildatum <- as.Date("2024-03-10")

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
                                      "INS_Inschrijvingsjaar" = "RES_Academisch_jaar_beoordeling"))

## Add first year variables
dfFirst_year <- dfFirst_year %>%
  group_by(INS_Studentnummer, INS_Opleidingsnaam_2002) %>%
  mutate(RES_Aantal_no_shows_voor_jaar = cumsum(lag(RES_Aantal_no_shows_tm_P7, default = 0))) %>%
  ungroup()

dfAS_full <- dfAS_full %>%
  strict_left_join(
    dfFirst_year %>%
      select(INS_Studentnummer,
             INS_Opleidingsnaam_2002,
             BSA_Status_EJ,
             INS_Dagen_tussen_aanmelding_en_1_september_EJ,
             RES_Aantal_no_shows_voor_jaar),
    by = c('INS_Studentnummer', 'INS_Opleidingsnaam_2002')
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
           INS_Inschrijvingsjaar %in% c(2020, 2021) ~ OPL_BSA_EC_eis_aug_jr1 - 6,
           INS_Inschrijvingsjaar == 2019 ~ 0,
           TRUE ~ OPL_BSA_EC_eis_aug_jr1
         ),
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
  select(INS_Studentnummer, Aanmeldingsjaar, INS_Opleidingsnaam_2002, Herinschrijving_volgend_jaar)

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

## Impute by aggregate function per year and programme
vNA_Mode <- c("INS_Dagen_tussen_aanmelding_en_1_september",
              "INS_Dagen_tussen_aanmelding_en_1_september_EJ",
              "INS_Aantal_inschrijvingen_in_HO",
              "INS_Soort_eerstejaars", "DEM_Geslacht")

vNA_Median <- c("Nominaal_perc_lag2")



## If we have VOP_gemiddeld_beta, use that for wiskunde
dfAS_full <- dfAS_full %>%
  mutate(VOP_Cijfer_wiskunde = coalesce(VOP_Cijfer_wiskunde, VOP_Cijfer_gemiddeld_beta))


## Maybe change imputed values based on other data (knn)? Also beware of data leakage
dfAS_full <- dfAS_full %>%
  fill_df_with_agg_by_group(
    group = c("INS_Opleidingsnaam_2002", "INS_Inschrijvingsjaar"),
    columns = c(vNA_Median),
    overwrite_col = TRUE,
    statistic = median,
    fill_empty_group = TRUE) %>%
  fill_df_with_agg_by_group(
    group = c("INS_Opleidingsnaam_2002", "INS_Inschrijvingsjaar"),
    columns = vNA_Mode,
    overwrite_col = TRUE,
    statistic = vvconverter::mode,
    fill_empty_group = TRUE)

date <- date <- gsub("-", "", Sys.Date())
filename <- paste0("dfAS_prepped_B2+_P", peilperiode, "_", date)


write_file(dfAS_full, filename, destination = "4. Analyses/Doorstroomprognose/Data/Geprepareerd/B2/",
           save_rds = TRUE)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3. Modelleren ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

test_cutoff <- 2021

set.seed(3)
dfAS_trainval <- dfAS_full %>%
  filter(INS_Inschrijvingsjaar < test_cutoff,
         INS_Inschrijvingsjaar >= 2016,
         INS_Inschrijvingsjaar != 2019) %>%
  group_by(INS_Studentnummer) %>%
  slice_sample(n = 1) %>%
  ungroup()

dfAS_test <- dfAS_full %>%
  filter(INS_Inschrijvingsjaar %in% test_cutoff:2022)


set.seed(10)
## Only use trainset for all models
vClass_weights <- rev(dfAS_trainval %>%
                        count(SUC_Type_uitstroom_studiejaar) %>%
                        pull(n) / nrow(dfAS_trainval))

## TODO: based on gridsearch
rf_model <- rand_forest(mode = "classification",
                        trees = 903,
                        mtry = 8) %>%
  set_engine("ranger",
             class.weights = vClass_weights,
             max.depth = 21,
             always.split.variables = "INS_Opleidingsnaam_2002",
             num.threads = 6)


vRemovals <- c(
  "RES_Aantal_vakken_cum"
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
  step_novel(INS_Opleidingsnaam_2002, OPL_Numerus_fixus_selectie) %>%
  step_mutate_at(INS_Uitwonend, fn = as.character) %>%
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
  #step_ordinalscore(MVR_Studiesituatie_Ondersteuning_nodig) %>%
  step_string2factor(all_nominal_predictors()) %>%
  step_rm(c(all_of(c(vRemovals, vFiltervariabelen, vExtravariabelen))))

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
# lAS_trainval_year_split <- dfAS_trainval %>%
#   group_by(INS_Inschrijvingsjaar) %>%
#   group_split()
#
# set.seed(10)
# lData_folds <- map(lAS_trainval_year_split, ~vfold_cv(.x, v = 4, strata = SUC_Type_uitstroom_studiejaar))
#
# keep_pred <- control_resamples(save_pred = TRUE)
#
# lFits <- map(lData_folds, ~fit_resamples(rf_wflow_val, .x, control = keep_pred))
#
# result_val <- lFits %>%
#   map(collect_predictions)
#
# ## Judging
# lFits %>%
#   map(collect_metrics)
#
# combined_val <- map2(lAS_trainval_year_split,
#            result_val,
#            ~cbind(.x %>%
#                     select(-SUC_Type_uitstroom_studiejaar),
#                   arrange(.y, .row))) %>%
#   bind_rows()
#
#
# combined_val %>%
#   summarise(uitvalp = sum(.pred_Uitval),
#             uitvalt = sum(SUC_Type_uitstroom_studiejaar == 'Uitval'),
#             nog_studerendp = sum(`.pred_Nog studerend`),
#             nog_studerendt = sum(SUC_Type_uitstroom_studiejaar == 'Nog studerend'),
#             diplomap = sum(.pred_Diploma),
#             diplomat = sum(SUC_Type_uitstroom_studiejaar == 'Diploma'))
#
# combined_val %>%
#   filter(INS_Studiejaar != 2) %>%
#   group_by(SUC_Type_uitstroom_studiejaar) %>%
#   accuracy(truth = SUC_Type_uitstroom_studiejaar, estimate = .pred_class)
#
# combined_full <- combined_val
#
# ## Smape
# combined_val %>%
#   group_by(INS_Opleidingsnaam_2002) %>%
#   summarise(truth_doorstroom = sum(SUC_Type_uitstroom_studiejaar != 'Uitval'),
#             predicted_doorstroom = sum(1 - .pred_Uitval)) %>%
#   ungroup() %>%
#   filter(truth_doorstroom > 25) %>%
#   smape(truth = truth_doorstroom, estimate = predicted_doorstroom)

# Train/val
set.seed(10)

data_folds <- vfold_cv(dfAS_trainval, v = 10, strata = SUC_Type_uitstroom_studiejaar)


set.seed(652)
keep_pred <- control_resamples(save_pred = TRUE, save_workflow = TRUE)
rf_fit_val <- fit_resamples(rf_wflow_val, data_folds, control = keep_pred)
roc_res <- metric_set(roc_auc)


result_val <- rf_fit_val %>%
  collect_predictions()

## Judging
collect_metrics(rf_fit_val)


combined_val <- dfAS_trainval %>%
  select(-SUC_Type_uitstroom_studiejaar) %>%
  cbind(arrange(result_val, .row))

#names(combined_val) <- gsub("_tm_P[0-9]$", "", names(combined_val))

## Smape
combined_val %>%
  group_by(INS_Opleidingsnaam_2002) %>%
  summarise(truth_doorstroom = sum(SUC_Type_uitstroom_studiejaar != 'Uitval'),
            predicted_doorstroom = sum(1 - .pred_Uitval)) %>%
  ungroup() %>%
  filter(truth_doorstroom > 25) %>%
  smape(truth = truth_doorstroom, estimate = predicted_doorstroom)



## Test
set.seed(652)
rf_fit_test <- fit(rf_wflow_test, dfAS_trainval)
roc_res <- metric_set(roc_auc)


result_test <- rf_fit_test %>%
  predict(dfAS_test, type = "class") %>%
  cbind(predict(rf_fit_test, dfAS_test, type = "prob"))

combined_test <- dfAS_test %>%
  cbind(result_test) %>%
  select(-starts_with("WISKUNDE"))

## Get smape
combined_test %>%
  group_by(INS_Opleidingsnaam_2002) %>%
  summarise(truth_doorstroom = sum(SUC_Type_uitstroom_studiejaar != 'Uitval'),
            predicted_doorstroom = sum(1 - .pred_Uitval)) %>%
  ungroup() %>%
  filter(truth_doorstroom > 25) %>%
  smape(truth = truth_doorstroom, estimate = predicted_doorstroom)

combined_full <- combined_val %>%
  select(names(combined_test)) %>%
  rbind(combined_test) %>%
  mutate(Testset = ifelse(INS_Inschrijvingsjaar >= test_cutoff, TRUE, FALSE))

## OPLAS for NF data
dfOpleidingen_raw <- readrds_csv(dataloc = "Output/_REPOSITORIES/opleidingen-analyseset/main/3. Analyseset/OPLAS_VU.rds") %>%
  select(INS_Opleidingsnaam_2002, INS_Inschrijvingsjaar, OPL_Numerus_fixus_selectie,
         OPL_Instructietaal) %>%
  distinct() %>%
  filter(INS_Inschrijvingsjaar <= 2022) %>%
  filter(row_number() == 1, .by = c(INS_Inschrijvingsjaar, INS_Opleidingsnaam_2002))


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## BEWAAR & RUIM OP ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

date <- gsub("-", "", Sys.Date())

saverds_csv(dfAS_full, "dfAS_prepped", dataloc = "Tableau/205 VU-Inschrijvingen prognose 2021/Random Forest/Data/B2+/", save_csv = TRUE)

saverds_csv(combined_val, "rf_results", dataloc = "Tableau/205 VU-Inschrijvingen prognose 2021/Random Forest/Data/B2+/", save_csv = TRUE)
#saverds_csv(combined_val, paste0("rf_results", date), dataloc = "Tableau/205 VU-Inschrijvingen prognose 2021/Random Forest/Data/Archive/", save_csv = TRUE)

saverds_csv(combined_test, "rf_test_results", dataloc = "Tableau/205 VU-Inschrijvingen prognose 2021/Random Forest/Data/B2+/", save_csv = TRUE)

saverds_csv(combined_full, "rf_full_results_b2", dataloc = "Tableau/205 VU-Inschrijvingen prognose 2021/Random Forest/Data/B2+/", save_csv = TRUE)
#saverds_csv(combined_full, "rf_full_results", dataloc = "Tableau/205 VU-Inschrijvingen prognose 2021/Random Forest/Data/Archive/", save_csv = TRUE)

saverds_csv(rf_fit_test, "rf_model_test", dataloc = "Tableau/205 VU-Inschrijvingen prognose 2021/Random Forest/Data/B2+/")

#Clear_script_objects()



