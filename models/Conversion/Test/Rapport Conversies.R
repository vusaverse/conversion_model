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

dfAS <- get_analysisset(columns = vColumns)



lAanmeldingen_bestandspaden <- list.files(
  paste0(Sys.getenv("NETWORK_DIR"),
         "Output/", Sys.getenv("BRANCH"), "/",
         "2. Geprepareerde data/"),
  pattern = "AAN_Aanmeldingen_per_dag_2",
  full.names = T)

vAggregatieniveau = c("INS_Studentnummer", "INS_Opleidingsnaam_2002", "INS_Opleidingsfase_BPM",
                      "INS_Inschrijvingsjaar")

read_files <- function(bestand) {
  gc()
  readRDS(bestand) %>%
    select(AAN_Indicatie_EOI,
           INS_Opleidingsnaam_2002,
           INS_Opleidingsfase_BPM,
           INS_Faculteit,
           INS_Inschrijvingsjaar,
           INS_Hoogste_vooropleiding_soort,
           INS_Studentnummer,
           AAN_Dagen_tot_1_sept,
           AAN_Datum,
           INS_Datum_inschrijving,
           AAN_Indicatie_EI,
           INS_Opleidingsvorm,
           AAN_Status,
           AAN_Substatus,
           any_of("DEM_Nationaliteit_EER_Naam")) %>%
    filter(AAN_Indicatie_EOI == TRUE,
           !is.na(INS_Opleidingsnaam_2002),
           AAN_Dagen_tot_1_sept > -30,
           INS_Opleidingsfase_BPM %in% c("B", "M"),
           INS_Faculteit != "AUC",
           INS_Inschrijvingsjaar %notin% c(2017, 2020))
}

dfAanmeldingen_raw <-
  map_dfr(lAanmeldingen_bestandspaden, read_files)

dfSLK_Aanmeldingen <- readrds_csv(output = "2. Geprepareerde data/SLK_Aanmeldingen.rds") %>%
  filter(!SLK_Herinschrijving) %>%
  select(SLK_Opleidingsnaam_2002, SLK_Aantal_aanmeldingen_gewogen,
         SLK_Aantal_aanmeldingen_ongewogen, SLK_Aantal_aanmeldingen_geannuleerd,
         SLK_Academisch_jaar, SLK_Week_vanaf_1okt, SLK_EER)


dfOpleidingen <- read_file_proj("OPLAS_VU",
                                dir = "/_REPOSITORIES/opleidingen-analyseset/main/3. Analyseset",
                                add_branch = F) %>%
  select(INS_Opleidingsnaam_2002, INS_Inschrijvingsjaar, OPL_Numerus_fixus_selectie,
         OPL_Instructietaal, OPL_Numerus_fixus_selectie_capaciteit_max, UAS_Opleiding_Startmoment,
         INS_Penvoerder) %>%
  mutate(OPL_Numerus_fixus_selectie = as.logical(OPL_Numerus_fixus_selectie)) %>%
  distinct() %>%
  filter(row_number() == 1, .by = c(INS_Inschrijvingsjaar, INS_Opleidingsnaam_2002))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. BEWERKEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

gc()

vNieuwe_NF <- c("B Gezondheid en Leven", "B Computer Science")

dfSLK_Aanmeldingen <- dfSLK_Aanmeldingen %>%
  summarise(SLK_Aantal_aanmeldingen_gewogen_agg = sum(SLK_Aantal_aanmeldingen_gewogen),
            SLK_Aantal_aanmeldingen_ongewogen_agg = sum(SLK_Aantal_aanmeldingen_ongewogen),
            .by = c(SLK_Opleidingsnaam_2002, SLK_Academisch_jaar, SLK_Week_vanaf_1okt))

dfOpleidingen <- dfOpleidingen %>%
  mutate(
    ## Zet capaciteit om naar numeriek en maak van character waarden NA's
    ## suppreswaarings omdat de str_detect een warning geeft bij NA
    OPL_Numerus_fixus_selectie_capaciteit_max =  suppressWarnings(ifelse(
      str_detect(OPL_Numerus_fixus_selectie_capaciteit_max, "[0-9]"),
      as.numeric(OPL_Numerus_fixus_selectie_capaciteit_max),
      NA_integer_)),
    OPL_Numerus_fixus_selectie = !is.na(OPL_Numerus_fixus_selectie_capaciteit_max)) %>%
  mutate(INS_Penvoerder_in_recentste_jaar = INS_Penvoerder[INS_Inschrijvingsjaar == max(INS_Inschrijvingsjaar)],
         .by = INS_Opleidingsnaam_2002)

vNieuwe_NF <- c("B Gezondheid en Leven", "B Computer Science")
dfOpleidingen <-  dfOpleidingen %>%
  mutate(OPL_Numerus_fixus_selectie = ifelse(INS_Inschrijvingsjaar >= 2023,
                                             OPL_Numerus_fixus_selectie[INS_Inschrijvingsjaar == 2022],
                                             OPL_Numerus_fixus_selectie),
         .by = INS_Opleidingsnaam_2002) %>%
  mutate(OPL_Numerus_fixus_selectie = ifelse(INS_Opleidingsnaam_2002 %in% vNieuwe_NF & INS_Inschrijvingsjaar >= 2023,
                                             TRUE,
                                             OPL_Numerus_fixus_selectie))

## In case of missing years, copy from previous
for(jaar in (max(dfOpleidingen$INS_Inschrijvingsjaar) + 1) : academic_year(today())) {
  dfOpleidingen <- dfOpleidingen %>%
    filter(INS_Inschrijvingsjaar == max(INS_Inschrijvingsjaar)) %>%
    mutate(INS_Inschrijvingsjaar = jaar) %>%
    bind_rows(dfOpleidingen)
}



dfAS <- dfAS %>%
  filter(INS_Studiejaar == 1,
         INS_Indicatie_actief_op_peildatum_status != "uitgeschreven",
         INS_Hoofdneven == "Hoofdinschrijving") %>%
  mutate(Ingestroomd = TRUE,
         ## Just for joining purposes
         aanmeldingnummer_in_opl_in_jaar = 1)


Concat_opleidingen <- function(s) {
  ## Maakt van een lijst in een cel één (geordende) string.
  paste(sort(s), collapse = ", ")
}


dfAanmeldingen <- dfAanmeldingen_raw %>%
  # filter(AAN_Datum == min(AAN_Datum),
  #        .by = all_of(vAggregatieniveau)) %>%
  # filter(AAN_Dagen_tot_1_sept == min(AAN_Dagen_tot_1_sept),
  #        .by = all_of(vAggregatieniveau)) %>%
  filter(AAN_Dagen_tot_1_sept == -26) %>%

  ## Add dates in 2023-2024 format (necessary for leap year) to be able to compare dates
  mutate(Datum_aanmelding_aangepast_2024 = case_when(
      month(AAN_Datum) > 9 ~ `year<-`(AAN_Datum, 2023),
      TRUE ~ `year<-`(AAN_Datum, 2024)),
    Maand_inschrijving = month(INS_Datum_inschrijving)) %>%
  mutate(late_aanmelder = (INS_Opleidingsfase_BPM == "B" & Datum_aanmelding_aangepast_2024 > as.Date("2024-05-01")) |
           (INS_Opleidingsfase_BPM == "M" & Datum_aanmelding_aangepast_2024 > as.Date("2024-05-31"))) %>%
  mutate(aanmeldingnummer_in_opl_in_jaar = row_number(), .by = all_of(vAggregatieniveau)) %>%
  # select(INS_Studentnummer, INS_Opleidingsnaam_2002, INS_Faculteit, INS_Opleidingsfase_BPM,
  #        INS_Inschrijvingsjaar, AAN_Datum, DEM_Nationaliteit_EER_Naam, AAN_Indicatie_EI) %>%
  #distinct() %>%
  mutate(Masteraanmelding_na_B_VU = INS_Opleidingsfase_BPM == "M" &
           INS_Studentnummer %in% (dfAS %>% filter(INS_Opleidingsfase_BPM == "B") %>%
                                                              pull(INS_Studentnummer)),
         INS_Opleidingsvorm = ifelse(INS_Opleidingsvorm == "Voltijd", "Voltijd", "Deeltijd-duaal")) %>%
  mutate(Aanmeldingen_VU_gelijke_fase = n(), .by = c(INS_Studentnummer, INS_Opleidingsfase_BPM,
                                                     INS_Inschrijvingsjaar)) %>%
  mutate(any_aanmelding_voor_deadline = any(Datum_aanmelding_aangepast_2024 <= as.Date("2024-05-01") & INS_Opleidingsfase_BPM == "B",
                                            Datum_aanmelding_aangepast_2024 <= as.Date("2024-05-31") & INS_Opleidingsfase_BPM == "M"),
         .by = c(INS_Studentnummer, INS_Inschrijvingsjaar, INS_Opleidingsfase_BPM))


dfAanmeldingen_incl_conv <- dfAanmeldingen %>%
  left_join(dfAS, by = c("INS_Studentnummer", "INS_Opleidingsfase_BPM", "INS_Opleidingsnaam_2002",
                         "INS_Inschrijvingsjaar", "aanmeldingnummer_in_opl_in_jaar"), relationship = "one-to-one") %>%
  left_join(dfOpleidingen, by = c("INS_Opleidingsnaam_2002", "INS_Inschrijvingsjaar"), relationship = "many-to-one") %>%
  mutate(INS_Studentnummer = hash_var(INS_Studentnummer),
         Ingestroomd = replace_na(Ingestroomd, FALSE)) %>%
  mutate(Aangemeld_voor_VU_NF = any(OPL_Numerus_fixus_selectie, na.rm = TRUE),
         Ingestroomd_bij_VU = any(Ingestroomd),
         Ingestroomd_bij = Concat_opleidingen(INS_Opleidingsnaam_2002[Ingestroomd]),
         Ingestroomd_bij_hoofdinsch = Concat_opleidingen(INS_Opleidingsnaam_2002[Ingestroomd & INS_Hoofdneven == "Hoofdinschrijving"]),
         .by = c(INS_Studentnummer, INS_Inschrijvingsjaar))

dfAanmeldingen_incl_conv_XS <- dfAanmeldingen_incl_conv %>%
  select(INS_Inschrijvingsjaar, INS_Opleidingsnaam_2002, INS_Opleidingsfase_BPM, Ingestroomd, INS_Opleidingsvorm)# %>%
  ## B GNK not taken into account in conv. prognosis
  #filter(INS_Opleidingsnaam_2002 != "B Geneeskunde")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## BEWAAR & RUIM OP ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

write_file(dfAanmeldingen_incl_conv_XS,
           "Conversie_evaluatie_XS",
           destination = paste0(Sys.getenv("TABLEAU_DATA_DIR"), "/F. Prognoses/VU-Instroomprognose/"),
           save_csv = TRUE)


write_file(dfAanmeldingen_incl_conv,
           "Conversie_evaluatie",
           destination = paste0(Sys.getenv("TABLEAU_DATA_DIR"), "/F. Prognoses/VU-Instroomprognose/"),
           save_csv = TRUE)

write_file(dfSLK_Aanmeldingen,
           "Conversie_evaluatie_SLK_Aanmeldingen",
           destination = paste0(Sys.getenv("TABLEAU_DATA_DIR"), "/F. Prognoses/VU-Instroomprognose/"),
           save_csv = TRUE)

#clear_script_objects()

