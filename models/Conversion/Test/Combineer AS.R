

vColumns <- c(
  "INS_Inschrijvingsjaar",
  "INS_Opleidingsfase_BPM",
  "INS_Opleidingsnaam_2002",
  "INS_Studentnummer",
  "INS_Studiejaar",
  "INS_Indicatie_actief_op_peildatum_status",
  "INS_Hoofdneven"
)

dfAS <- get_analysisset(columns = vColumns) %>%
  filter(INS_Indicatie_actief_op_peildatum_status == "actief",
         INS_Inschrijvingsjaar == 2024,
         INS_Studiejaar == 1,
         INS_Hoofdneven == "Hoofdinschrijving")

dfConversieprognose_raw <- read_file_proj(
  "Conversieprognose_resultaat",
  base_dir = paste0(Sys.getenv("NETWORK_DIR"), "Output/"),
  add_branch = TRUE,
  dir = "4. Analyses/Instroom komend jaar/Conversieprognose/Resultaat/Test/",
  extension = "rds")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. BEWERKEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

dfAS <- dfAS %>%
  mutate(Ingestroomd = TRUE)

dfConversie_joined <- dfConversieprognose_raw %>%
  filter(INS_Inschrijvingsjaar == 2024) %>%
  select(-Ingestroomd) %>%
  left_join(dfAS, by = c("INS_Studentnummer", "INS_Opleidingsnaam_2002", "INS_Inschrijvingsjaar",
                         "INS_Opleidingsfase_BPM"),
            relationship = "many-to-one", multiple = "first")

dfConversie_joined <- dfConversie_joined %>%
  mutate(INS_Studentnummer = hash_var(INS_Studentnummer))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## BEWAAR & RUIM OP ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

write_file(dfConversie_joined,
           "Conversieprognose_testresultaat_2024",
           "Tableau/Git voor Tableau/Data/F. Prognoses/Conversieprognose/Test/",
           save_csv = TRUE)

