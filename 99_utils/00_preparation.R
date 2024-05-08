## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code for Education Analytics Vrije Universiteit Amsterdam
## Copyright 2024 VU
## Web Page: http://www.vu.nl
## Contact:
##
##' *INFO*:
## 1) ___
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Get current branch
Sys.setenv("BRANCH" = system("git branch --show-current", intern = TRUE))

## Latest vusa version
renv::install("vusaverse/vusa", rebuild = TRUE, prompt = FALSE)
renv::record("vusaverse/vusa")

## Restore packages from renv
renv::restore(prompt = FALSE)

## Load packages
source("99_utils/load_packages.R")

## Set system variables
source("99_utils/set_systemvariables.R")

## Load functions
source("99_utils/read_and_write_settings_functions.R")


##' *TODO*
##' slack setup

config <- yaml::read_yaml("99_utils/config.yml")

##' *INFO* clear_global_proj
object_names <- ls(envir = .GlobalEnv)

# Concatenate the object names into a space-separated string
default_keep_list <- paste(object_names, collapse = " ")

# Set the environment variable
Sys.setenv(DEFAULT_KEEP_LIST = default_keep_list)

vusa::clear_global_proj()

##'* INFO*
##' All possible settings:
##' https://docs.posit.co/ide/server-pro/reference/session_user_settings.html
##' https://docs.posit.co/ide/server-pro/rstudio_pro_sessions/session_startup_scripts.html
##' Enforce margin of 100; use rstudio.sessionInit hook as RStudio needs to be initiated.
setHook("rstudio.sessionInit", function(newSession) {
  if (newSession) {
    vusa::use_rstudio_prefs_silent(
      "margin_column" = as.integer(100)
    )
  }
}, action = "append")
