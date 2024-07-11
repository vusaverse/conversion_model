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
## Opmerkingen:
## 1) ___
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Shapley's ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
test_cutoff <- 2018

dfAS_full <- readrds_csv(dataloc='Tableau/205 VU-Inschrijvingen prognose 2021/Random Forest/Data/dfAS_prepped.rds')
rf_fit_test <- readrds_csv(dataloc='Tableau/205 VU-Inschrijvingen prognose 2021/Random Forest/Data/rf_model_test.rds')

dfAS_trainval <- dfAS_full %>%
  filter(INS_Inschrijvingsjaar < test_cutoff,
         INS_Opleidingsnaam_2002 == 'B Econometrie en Operationele Research')

vip_train <- dfAS_trainval %>%
  select(-SUC_Uitval_na_jaar_1_cohorten) %>%
  filter(INS_Opleidingsnaam_2002 == 'B Econometrie en Operationele Research')


explainer_rf <-
  explain_tidymodels(
    rf_fit_test,
    data = vip_train,
    y = as.logical(dfAS_trainval$SUC_Uitval_na_jaar_1_cohorten),
    label = "random forest",
    verbose = FALSE
  )


duplex <- tail(dfAS_trainval, 102)

shap_duplex <-
  predict_parts(
    explainer = explainer_rf,
    new_observation = duplex,
    type = "shap",
    B = 3
  )


library(forcats)
shap_duplex %>%
  group_by(variable) %>%
  mutate(mean_val = mean(contribution)) %>%
  ungroup() %>%
  filter(abs(mean_val) > 0.001) %>%
  mutate(variable = fct_reorder(variable, abs(mean_val))) %>%
  ggplot(aes(contribution, variable, fill = mean_val > 0)) +
  geom_col(data = ~distinct(., variable, mean_val),
           aes(mean_val, variable),
           alpha = 0.5) +
  geom_boxplot(width = 0.5) +
  theme(legend.position = "none") +
  scale_fill_viridis_d() +
  labs(y = NULL)


set.seed(1805)

var <- "RES_Aantal_EC_tm_P2"
pdp_ec <- model_profile(explainer_rf, N = 500, variables = var)

ggplot_pdp <- function(obj, x) {

  p <-
    as_tibble(obj$agr_profiles) %>%
    mutate(`_label_` = stringr::str_remove(`_label_`, "^[^_]*_")) %>%
    ggplot(aes(`_x_`, `_yhat_`)) +
    geom_line(data = as_tibble(obj$cp_profiles),
              aes(x = {{ x }}, group = `_ids_`),
              size = 0.5, alpha = 0.05, color = "gray50")+
    theme(axis.text.x = element_text(angle = 90))

  num_colors <- n_distinct(obj$agr_profiles$`_label_`)

  if (num_colors > 1) {
    p <- p + geom_line(aes(color = `_label_`), size = 1.2, alpha = 0.8)
  } else {
    p <- p + geom_line(color = "midnightblue", size = 1.2, alpha = 0.8)
  }

  p
}


ggplot_pdp(pdp_ec, !!sym(var))  +
  labs(x = var,
       y = "pred uitval",
       color = NULL)

