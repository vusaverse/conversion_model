## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code voor Student Analytics Vrije Universiteit Amsterdam
## Copyright 2023 VU
## Web Page: http://www.vu.nl
## Contact: vu-analytics@vu.nl
##
##' *INFO*:
## 1) Loads packages for the project using library()
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Define the basic packages
basic_packages <- c(
  "cli",            # Used to add color to console messages
  "digest",         # Used for hashing variables
  "janitor",        # Used to clean up variable names from special characters
  "lubridate",      # Used to work with dates and times
  "purrr",          # Used to work with functions and vectors
  "readr",          # Used to read data (csv, tsv, and fwf)
  "slackr",         # Used to send messages in Slack
  "stringr",        # Used for functions to work with strings
  "tibble",         # Used for editing and creating tibbles
  "tidyr",          # Used to clean data in the tidyverse environment
  "utils",          # Used for utility functions
  "styler",         # Used for improving the style of script
  "vusa",           # Mainly to always have the addins
  "vvmover",
  "vvsculptor",
  "zoo",           #
  "ranger",           #
  "tidymodels",           #,
  "dplyr"          # Used for the dplyr environment, keep last
)

# Load the packages into the library
suppressMessages(purrr::walk(basic_packages, ~library(.x, character.only = TRUE, warn.conflicts = FALSE)))

if (interactive()) {
  library(tidylog)
}

vusa::clear_script_objects()
