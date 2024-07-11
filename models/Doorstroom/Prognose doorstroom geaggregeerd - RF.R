## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Prognose doorstroom geaggregeerd - RF.R ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code voor Student Analytics Vrije Universiteit Amsterdam
## Copyright 2023 VU
## Web Page: http://www.vu.nl
## Contact: vu-analytics@vu.nl
## Verspreiding buiten de VU: Ja
##
## Doel: Doel
##
## Opmerkingen:
## 1) ___
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 0. Run modellen ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

model_folder = paste0(getwd(), "/models/Doorstroom/")
source(paste0(model_folder, "Prognose doorstroom bachelor jaar 1 - RF.R"))
source(paste0(model_folder, "Prognose doorstroom bachelor jaar 2+ - RF.R"))
source(paste0(model_folder, "Prognose doorstroom master - RF.R"))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. INLEZEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Lees resultaten in

dfPrognoses_bachelor1 <- read_file_proj("doorstroom_predictions_B1",
                                        base_dir = paste0(Sys.getenv("NETWORK_DIR"), "Output/"),
                                        dir = "4. Analyses/Doorstroomprognose/Data/Resultaten",
                                        add_branch = TRUE,
                                        extension = "csv")

dfPrognoses_bachelor2 <- read_file_proj("doorstroom_predictions_B2+",
                                        base_dir = paste0(Sys.getenv("NETWORK_DIR"), "Output/"),
                                        dir = "4. Analyses/Doorstroomprognose/Data/Resultaten",
                                        add_branch = TRUE,
                                        extension = "csv")

dfPrognoses_master <- read_file_proj("doorstroom_predictions_M",
                                     base_dir = paste0(Sys.getenv("NETWORK_DIR"), "Output/"),
                                     dir = "4. Analyses/Doorstroomprognose/Data/Resultaten",
                                     add_branch = TRUE,
                                     extension = "csv")

vColumns_to_keep_all <- c(
  "INS_Studentnummer",
  "INS_Faculteit",
  "INS_Opleidingsnaam_2002",
  "INS_Opleidingsfase_BPM",
  "INS_Studiejaar",
  "INS_Inschrijvingsjaar",
  "INS_IO_Herkomst_EER_naam"
)

vColumns_to_keep_predictions <- c(
  ".pred_Uitval",
  ".pred_Nog.studerend",
  ".pred_Diploma"
)

dfAS <- readrds_csv(output = "3. Analyseset/Analysis_set_1.fst",
                    columns = c(vColumns_to_keep_all, "SUC_Type_uitstroom_studiejaar", "INS_Hoofdneven")) %>%
  filter(INS_Inschrijvingsjaar >= 2016)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. BEWERKEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

dfPrognoses_bachelor1 <- dfPrognoses_bachelor1 %>%
  rename(".pred_Uitval" = ".pred_TRUE",
         ".pred_Nog.studerend" = ".pred_FALSE") %>%
  mutate(.pred_Diploma = 0.0)

dfPrognose_gecombineerd <- dfPrognoses_bachelor1 %>%
  mutate(.pred_class = as.character(.pred_class)) %>%
  bind_rows(dfPrognoses_bachelor2) %>%
  bind_rows(dfPrognoses_master) %>%
  select(c(vColumns_to_keep_all, vColumns_to_keep_predictions)) %>%
  ## TODO join AS
  mutate(INS_Studentnummer = hash_var(INS_Studentnummer))

## Toon VU-brede aantallen
dfPrognose_gecombineerd %>%
  group_by(INS_Studiejaar == 1 & INS_Opleidingsfase_BPM == "B", INS_Opleidingsfase_BPM == "B") %>%
  summarise(nog_studerend = sum(.pred_Nog.studerend),
            diploma = sum(.pred_Diploma),
            uitval = sum(.pred_Uitval))


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## BEWAAR & RUIM OP ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
dataloc <- "Tableau/205 VU-Inschrijvingen prognose 2021/Random Forest/Data/final results/"

date <- gsub("-", "", Sys.Date())
write_file_proj(dfPrognose_gecombineerd,
                "final_doorstroom_predictions_rf_agg",
                base_dir = Sys.getenv("NETWORK_DIR"),
                dir = dataloc,
                add_branch = FALSE,
                extensions = "csv")

write_file_proj(dfPrognose_gecombineerd,
                paste0("final_doorstroom_predictions_rf_a_", date),
                base_dir = Sys.getenv("NETWORK_DIR"),
                dir = dataloc,
                add_branch = FALSE,
                extensions = "csv")

write_file_proj(dfPrognose_gecombineerd,
                "doorstroom_predictions_gecombineerd",
                base_dir = paste0(Sys.getenv("NETWORK_DIR"), "Output/"),
                dir = output,
                add_branch = TRUE,
                extensions = "csv")

write_file_proj(dfPrognose_gecombineerd,
                paste0("doorstroom_predictions_gecombineerd", date),
                base_dir = paste0(Sys.getenv("NETWORK_DIR"), "Output/"),
                dir = paste0(output, "Archief/"),
                add_branch = TRUE,
                extensions = "csv")

clear_script_objects()

