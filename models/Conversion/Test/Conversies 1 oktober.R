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
  "INS_Hoofdneven",
  "INS_Opleidingsvorm_naam"
)

dfAS_raw <- get_analysisset(columns = vColumns)

dfAanmeldingen_oktober_raw <- read_file_proj("dfAanmeldingen_oktober",
                dir = "4. Analyses/Instroom komend jaar/Conversieprognose/Geprepareerd/")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. BEWERKEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Determine ingestroomde studenten
dfAS <- dfAS_raw %>%
  filter(INS_Studiejaar == 1,
         INS_Indicatie_actief_op_peildatum_status == "actief",
         INS_Hoofdneven == "Hoofdinschrijving",
         INS_Inschrijvingsjaar >= 2017) %>%
  mutate(Ingestroomd = TRUE,
         INS_Opleidingsvorm = ifelse(INS_Opleidingsvorm_naam == "Voltijd", "voltijd", "deeltijd"))%>%
  select(INS_Inschrijvingsjaar, INS_Studentnummer, INS_Opleidingsnaam_2002, INS_Opleidingsfase_BPM,
         INS_Opleidingsvorm_naam, Ingestroomd)

## If student only has differing opleidingsvorm in AS, change it (due to changing opleidingsvorm within year for instance)
dfAanmeldingen_oktober <- dfAanmeldingen_oktober_raw %>%
  mutate(nAanmeldingen_per_opl = n(),
         .by = c(INS_Studentnummer, INS_Inschrijvingsjaar, INS_Opleidingsfase_BPM, INS_Opleidingsnaam_2002)) %>%
  mutate(In_aanmeldingen = TRUE)


## Add AS and opleidingen data, and create features from these
dfAanmeldingen_oktober <- dfAanmeldingen_oktober %>%
  full_join(dfAS, by = c("INS_Studentnummer", "INS_Opleidingsfase_BPM", "INS_Opleidingsnaam_2002",
                         "INS_Inschrijvingsjaar"), relationship = "many-to-one") %>%
  mutate(INS_Opleidingsvorm = if_else(INS_Opleidingsvorm != INS_Opleidingsvorm_naam & n() == 1 & !is.na(INS_Opleidingsvorm_naam),
                                      INS_Opleidingsvorm_naam, INS_Opleidingsvorm),
          .by = c(INS_Studentnummer, INS_Opleidingsfase_BPM, INS_Opleidingsnaam_2002, INS_Inschrijvingsjaar)) %>%
  filter(!(INS_Opleidingsvorm != INS_Opleidingsvorm_naam & any(INS_Opleidingsvorm == INS_Opleidingsvorm_naam)) |
        is.na(Ingestroomd),
         .by = c(INS_Studentnummer, INS_Opleidingsfase_BPM, INS_Opleidingsnaam_2002, INS_Inschrijvingsjaar)) %>%
  mutate(Ingestroomd = replace_na(Ingestroomd, FALSE),
         In_aanmeldingen = replace_na(In_aanmeldingen, FALSE),
         INS_Opleidingsvorm = coalesce(INS_Opleidingsvorm, INS_Opleidingsvorm_naam)) %>%
  select(-INS_Opleidingsvorm_naam)



## Joint degrees are weird
dfAanmeldingen_oktober <- dfAanmeldingen_oktober %>%
  filter(str_detect(INS_Opleidingsnaam_2002, "joint degree", negate = TRUE))


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## BEWAAR & RUIM OP ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

write_file(dfAanmeldingen_oktober,
           "Conversies_oktober",
           "Tableau/Git voor Tableau/Data/F. Prognoses/VU-Instroomprognose/",
           save_csv = TRUE)

clear_script_objects()
