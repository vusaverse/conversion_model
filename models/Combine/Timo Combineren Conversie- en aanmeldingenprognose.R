## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code for Education Analytics Vrije Universiteit Amsterdam
## Copyright 2024 VU
## Web Page: http://www.vu.nl
## Contact: vu-analytics@vu.nl
##
##' *INFO*:
## 1) ___
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. INLEZEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

vColumns <- c(
  "INS_Faculteit",
  "INS_Inschrijvingsjaar",
  "INS_Opleidingsfase_BPM",
  "INS_Opleidingsnaam_2002",
  "INS_Studentnummer",
  "INS_Studiejaar",
  "INS_Datum_aanmelding",
  "INS_Hoofdneven",
  "INS_Opleidingsvorm_naam",
  "INS_Indicatie_actief_op_peildatum_status",
  "INS_September_februari_instroom",
  "DEM_Nationaliteit_EER_naam"
)

dfAS_raw <- get_analysisset(columns = vColumns) %>%
  filter(INS_Studiejaar == 1,
         INS_Hoofdneven == "Hoofdinschrijving",
         INS_Opleidingsvorm_naam == "voltijd",
         INS_Opleidingsfase_BPM != "P",
         INS_Indicatie_actief_op_peildatum_status == "actief",
         INS_September_februari_instroom == "September",
         INS_Inschrijvingsjaar >= 2021)


dfConversieprognose_raw <- read_file_proj(
  "Conversieprognose_resultaat",
  base_dir = paste0(Sys.getenv("NETWORK_DIR"), "Output/"),
  add_branch = TRUE,
  dir = "4. Analyses/Instroom komend jaar/Conversieprognose/Resultaat",
  extension = "rds")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. BEWERKEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
dfConversieprognose <- dfConversieprognose_raw %>%
  filter(INS_Inschrijvingsjaar == 2024,
         INS_Opleidingsvorm == "voltijd") %>%
  mutate(
    Herkomst = DEM_Nationaliteit_EER_Naam  #case_when(
      # ## Niet-EER
      # str_detect(AAN_AD_Groep_Omschrijving, "; INT") ~ DEM_Nationaliteit_EER_Naam,
      # ## NL
      # .default = "NL"
    )

dfConversieprognose_agg <- dfConversieprognose %>%
  summarise(instroom_conversie = sum(.pred_TRUE, na.rm = T),
            .by = c(INS_Opleidingsnaam_2002, INS_Faculteit, Herkomst))


dfAS <- dfAS_raw %>%
  mutate(date_treshold = `year<-`(today(), INS_Inschrijvingsjaar)) %>%
  arrange(INS_Inschrijvingsjaar)

dfPerc_aanmeldingen_na_huidige_datum <- dfAS %>%
  summarise(n_instroom = n(),
            n_instroom_na_huidige_datum = sum(INS_Datum_aanmelding > date_treshold, na.rm = TRUE),
            .by = c(INS_Opleidingsnaam_2002, DEM_Nationaliteit_EER_naam)) %>%
  mutate(perc_instroom_na_huidige_datum = n_instroom_na_huidige_datum / n_instroom)


vHerkomsten <- c("NL", "EER", "NIET-EER")

dfConversieprognose_agg_joined <- dfConversieprognose_agg %>%
  left_join(dfPerc_aanmeldingen_na_huidige_datum,
            by = c("INS_Opleidingsnaam_2002", "Herkomst" = "DEM_Nationaliteit_EER_naam")) %>%
  mutate(across(c(instroom_conversie, perc_instroom_na_huidige_datum), ~replace_na(.x, 0))) %>%
  mutate(Instroom = instroom_conversie + perc_instroom_na_huidige_datum * instroom_conversie) %>%
  select(INS_Opleidingsnaam_2002, INS_Faculteit, Herkomst, Instroom) %>%
  pivot_wider(names_from = Herkomst, values_from = Instroom) %>%
  mutate(across(all_of(vHerkomsten), ~replace_na(.x, 0)),
         across(all_of(vHerkomsten), ~round(.x, 0)),
         Totaal = NL + EER + `NIET-EER`) %>%
  arrange(INS_Opleidingsnaam_2002)


dfConversieprognose_agg_joined_beta <- dfConversieprognose_agg_joined %>%
  filter(INS_Faculteit == "BETA") %>%
  select(-c(NL, `NIET-EER`, EER, INS_Faculteit)) %>%
  rename(Opleiding = "INS_Opleidingsnaam_2002",
         Instroom = "Totaal")



## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## BEWAAR & RUIM OP ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

write_file_proj(dfConversieprognose_agg_joined, "Conversieprognose_nationaliteit",
                base_dir = paste0(Sys.getenv("NETWORK_DIR"), "Output/"),
                dir = "4. Analyses/Instroom komend jaar/Conversieprognose/Overige output",
                extensions = "csv"
)


write_file_proj(dfConversieprognose_agg_joined_beta, "Conversieprognose_beta",
                base_dir = paste0(Sys.getenv("NETWORK_DIR"), "Output/"),
                dir = "4. Analyses/Instroom komend jaar/Conversieprognose/Overige output",
                extensions = "csv"
)


