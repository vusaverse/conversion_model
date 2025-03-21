## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code voor Student Analytics Vrije Universiteit Amsterdam
## Copyright 2023 VU
## Web Page: http://www.vu.nl
## Contact: vu-analytics@vu.nl
##
##' *INFO*:
## 1) ___
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

set_all_environments <- function(var_name, var_value) {
  args <- list(var_value)
  names(args) <- var_name
  do.call(Sys.setenv, args)
}

## Read system variables from Excel file
##' *INFO* The RENVIRON_PATH environment variable has been set using the .Renviron file
##' To set your own path run and edit the following code:
##' usethis::edit_r_environ()
variables_to_set <- readxl::read_xlsx(Sys.getenv("RENVIRON_PATH"))

## Set SHAREPOINT_DIR
Sys.setenv(SHAREPOINt_DIR = paste0("C:/Users/", Sys.getenv("USERNAME"), "/Vrije Universiteit Amsterdam/"))

Sys.setenv(DOCUMENTATION_DIR =
             paste0(Sys.getenv("SHAREPOINt_DIR"), "Education Analytics - General/01_Documentatie/XX. Data Documentatie/"))

Sys.setenv(MAP_TABLE_DIR =
             paste0(Sys.getenv("SHAREPOINt_DIR"), "Education Analytics - General/01_Documentatie/XX. Data Documentatie/Mapping Tables/"))

## Set variables in R system variables
purrr::pmap(list(variables_to_set$variable, variables_to_set$value), set_all_environments)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## CLEAR #######
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

vusa::clear_script_objects()

