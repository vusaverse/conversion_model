## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Titel van het bestand ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code voor Student Analytics Vrije Universiteit Amsterdam
## Copyright 2022 VU
## Web Page: http://www.vu.nl
## Contact: vu-analytics@vu.nl
## Verspreiding buiten de VU: Ja
##
## Doel: Doel
##
## Afhankelijkheden: Afhankelijkheid
##
## Datasets: Datasets
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


variabelen <- c("DEM_Geslacht",
        "DEM_Leeftijd_peildatum_1_oktober",
        "HNP_Honours_programma_begonnen",
        "INS_Aansluiting_cat",
        "INS_Aantal_inschrijvingen_in_HO",
        "INS_Aantal_inschrijvingen_jaar",
        "INS_Dagen_tussen_aanmelding_en_1_september",
        "INS_Datum_aanmelding",
        "INS_Dubbele_studie_VU",
        "INS_Faculteit",
        "INS_Grootte_opleiding",
        "INS_Herinschrijving_jaar_2_na_uitschrijving_voor_1_feb_in_jaar_1",
        "INS_Hoofdneven",
        "INS_Hoogste_vooropleiding_soort_cat",
        "INS_Inschrijvingsjaar",
        "INS_IO_Herkomst_EER_naam",
        "INS_Opleiding_uitgeloot_ja_nee",
        "INS_Opleidingsfase_BPM",
        "INS_Opleidingsnaam_2002",
        "INS_Opleidingsvorm_naam",
        "INS_Opleiding_uitgeloot_naam",
        "INS_Soort_eerstejaars",
        "INS_Studentnummer",
        "INS_Studiejaar",
        "INS_Uitschrijving_voor_1_feb",
        "INS_Uitwonend",
        "INS_Vooropleiding_voor_HO_soort_SAP",
        "INS_Vooropleiding_voor_HO_soort",
        "INS_Vooropleiding_voor_HO_profiel_standaard",
        "INS_Vooropleiding_voor_HO_VWO_examen_kans",
        "INS_Vooropleiding_voor_HO_plaats",
        "MVR_AV_Studiedoelen_stellen",
        "MVR_AV_Studieplanning",
        "MVR_AV_Zelfdiscipline",
        "MVR_OBK_Vertrouwen_studiesucces",
        "MVR_Score_ingeschreven_opleiding",
        "MVR_Studiesituatie_Ondersteuning_nodig",
        "MVR_Studiesituatie_Studiekeuze",
        "MVR_Studiesituatie_Reistijd_minuten",
        "MVR_Ingeschreven_voor_hoogste_interesse",
        "MVR_OBK_Keuzezekerheid",
        "OPL_VU_alfa_beta_gamma",
        "OPL_VU_gebied",
        "ORI_Orientatie_komt_voor",
        "PUC_Deelgenomen",
        "RES_Aantal_EC_tm_P3",
        "RES_Aantal_herkansingen_tm_P3",
        "RES_Aantal_no_shows_tm_P3",
        "RES_Gem_resultaat_tm_P3",
        "SUC_Uitval_na_jaar_1_cohorten",
        "VOP_Cijfer_biologie",
        "VOP_Cijfer_economie",
        "VOP_Cijfer_engels",
        "VOP_Cijfer_gemiddeld",
        "VOP_Cijfer_gemiddeld_alfa",
        "VOP_Cijfer_gemiddeld_beta",
        "VOP_Cijfer_gemiddeld_gamma",
        "VOP_Cijfer_natuurkunde",
        "VOP_Cijfer_scheikunde",
        "VOP_Cijfer_wiskunde",
        "VOP_School_gemeente_vestiging",
        "VOP_School_reistijd_OV_VU")

dfAS <- readrds_csv(output = "3. Analyseset/Analysis_set_1.fst", columns = variabelen) %>%
    filter(INS_Studiejaar == 1,
           INS_Inschrijvingsjaar <= 2017,
           INS_Inschrijvingsjaar >= 2014,
           INS_Opleidingsfase_BPM == 'B',
           !INS_Uitschrijving_voor_1_feb)


dfCor <- dfAS %>%
  select(-c(
    INS_Studentnummer,
    INS_Herinschrijving_jaar_2_na_uitschrijving_voor_1_feb_in_jaar_1,
    INS_Grootte_opleiding,
    INS_Aantal_inschrijvingen_jaar,
    INS_Studiejaar,
    INS_Uitschrijving_voor_1_feb,
    VOP_Cijfer_scheikunde,
    VOP_Cijfer_natuurkunde,
    VOP_Cijfer_gemiddeld_beta,
    VOP_Cijfer_engels,
    VOP_Cijfer_economie,
    VOP_Cijfer_biologie
  )) %>%
  select_if(function(col) is.logical(col) | is.numeric(col)) %>%
  cor(use="complete.obs") %>%
  as_tibble()

dfCor['variable'] = names(dfCor)


savecsv(dfAS, 'df_RF_EDA', dataloc = 'Tableau/205 VU-Inschrijvingen prognose 2021/Random Forest/Data/')
savecsv(dfCor, 'df_RF_EDA_correlatie', dataloc = 'Tableau/205 VU-Inschrijvingen prognose 2021/Random Forest/Data/')










