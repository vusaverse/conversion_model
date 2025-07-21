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

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. INLEZEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## Read aanmeldingen
lAanmeldingen_bestandspaden <- list.files(
  paste0(Sys.getenv("NETWORK_DIR"),
         "Output/", Sys.getenv("BRANCH"), "/",
         "2. Geprepareerde data/"),
  pattern = "AAN_Aanmeldingen_per_dag_2",
  full.names = T)



read_files <- function(bestand) {
  gc()
  print(bestand)
  readRDS(bestand) %>%
    select(AAN_Indicatie_EOI,
           INS_Opleidingsnaam_2002,
           INS_Opleidingsfase_BPM,
           INS_Faculteit,
           INS_Inschrijvingsjaar,
           INS_Inschrijvingsjaar_EI,
           AAN_Indicatie_EI,
           INS_Hoogste_vooropleiding_soort,
           INS_Studentnummer,
           INS_Opleidingsvorm,
           AAN_Dagen_tot_1_sept,
           AAN_Datum,
           AAN_Soort_aanmelding,
           AAN_Status,
           AAN_Substatus,
           any_of(c("DEM_Nationaliteit_EER_Naam", 'AAN_AD_Groep_Omschrijving'))) %>%
    filter(AAN_Indicatie_EOI == TRUE,
           !is.na(INS_Opleidingsnaam_2002),
           INS_Opleidingsfase_BPM %in% c("B", "M"),
           INS_Faculteit != "AUC",
           INS_Inschrijvingsjaar != 2020) %>%
    select(-AAN_Indicatie_EOI)
}

dfAanmeldingen_raw <-
  map_dfr(lAanmeldingen_bestandspaden, read_files)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. BEWERKEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

vAggregatieniveau = c("INS_Studentnummer", "INS_Opleidingsnaam_2002", "INS_Opleidingsfase_BPM",
                      "INS_Inschrijvingsjaar", "INS_Opleidingsvorm")

dfAanmeldingen <- dfAanmeldingen_raw %>%
  arrange(INS_Inschrijvingsjaar, desc(AAN_Dagen_tot_1_sept)) %>%
  mutate(Dagen_in_substatus = row_number(),
         .by = c(all_of(vAggregatieniveau), AAN_Datum, AAN_Substatus))

rm(dfAanmeldingen_raw)

## Keep rows on test date only
nDagen_tot_1_sept_testdatum <- as.numeric(as.Date(paste0(vvconverter::academic_year(testdatum) + 1, "-09-01")) - testdatum)
## TODO what if day in a year is empty?
dfAanmeldingen_testdag <- dfAanmeldingen %>%
  mutate(Test_set = INS_Inschrijvingsjaar == nTest_year,
         ## Match AS
         INS_Opleidingsvorm = ifelse(INS_Opleidingsvorm == "Voltijd", "voltijd", "deeltijd")) %>%
  filter(!is.na(INS_Opleidingsnaam_2002),
         AAN_Dagen_tot_1_sept == nDagen_tot_1_sept_testdatum) %>%
  distinct() %>%
  mutate(perc_afgewezen = sum(AAN_Status == "Afgewezen") / n(),
         .by = c(INS_Opleidingsnaam_2002, INS_Inschrijvingsjaar))

dfAanmeldingen_oktober <- dfAanmeldingen %>%
  mutate(Test_set = INS_Inschrijvingsjaar == nTest_year,
         ## Match AS
         INS_Opleidingsvorm = ifelse(INS_Opleidingsvorm == "Voltijd", "voltijd", "deeltijd")) %>%
  filter(!is.na(INS_Opleidingsnaam_2002),
         AAN_Dagen_tot_1_sept > -30) %>%
  filter(AAN_Dagen_tot_1_sept == min(AAN_Dagen_tot_1_sept),
         .by = all_of(vAggregatieniveau)) %>%
  # filter(row_number() == 1,
  #        .by = all_of(vAggregatieniveau)) %>%
  distinct() %>%
  mutate(perc_afgewezen_okt = sum(AAN_Status == "Afgewezen") / n(),
         .by = c(INS_Opleidingsnaam_2002, INS_Inschrijvingsjaar))

dfRatios_afwijzingen <- dfAanmeldingen_testdag %>%
  select(INS_Inschrijvingsjaar, INS_Opleidingsnaam_2002, INS_Opleidingsfase_BPM, perc_afgewezen) %>%
  distinct() %>%
  left_join(dfAanmeldingen_oktober %>%
              select(INS_Inschrijvingsjaar, INS_Opleidingsnaam_2002, perc_afgewezen_okt) %>%
              distinct(),
            by = c("INS_Opleidingsnaam_2002", "INS_Inschrijvingsjaar"),
            relationship = "one-to-one") %>%
  arrange(INS_Inschrijvingsjaar) %>%
  mutate(perc_afgewezen_okt_lag = lag(perc_afgewezen_okt),
         .by = c(INS_Opleidingsnaam_2002)) %>%
  filter(INS_Inschrijvingsjaar >= 2018) %>%
  mutate(perc_afgewezen_okt_lag = if_else(is.na(perc_afgewezen_okt_lag), mean(perc_afgewezen_okt_lag, na.rm = TRUE),
                                                                              perc_afgewezen_okt_lag),
         .by = c(INS_Inschrijvingsjaar, INS_Opleidingsfase_BPM)) %>%
  mutate(perc_afgewezen_okt_lag_ratio = perc_afgewezen / perc_afgewezen_okt_lag) %>%
  select(INS_Inschrijvingsjaar, INS_Opleidingsnaam_2002, INS_Opleidingsfase_BPM, perc_afgewezen_okt_lag_ratio)

dfAanmeldingen_testdag <- dfAanmeldingen_testdag %>%
  left_join(dfRatios_afwijzingen,
            by = c("INS_Opleidingsfase_BPM", "INS_Inschrijvingsjaar", "INS_Opleidingsnaam_2002"),
            relationship = "many-to-one")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## BEWAAR & RUIM OP ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
write_file_proj(dfAanmeldingen_testdag,
                "dfAanmeldingen_geprepareerd",
                dir = "4. Analyses/Instroom komend jaar/Conversieprognose/Geprepareerd/Test/")


write_file_proj(dfAanmeldingen_testdag,
                "dfAanmeldingen_geprepareerd",
                dir = "4. Analyses/Instroom komend jaar/Conversieprognose/Geprepareerd/")


write_file_proj(dfAanmeldingen_oktober,
           "dfAanmeldingen_oktober",
           dir = "4. Analyses/Instroom komend jaar/Conversieprognose/Geprepareerd/")

clear_script_objects()

