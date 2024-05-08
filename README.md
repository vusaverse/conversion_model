# conversion_model

# Overview
This R project is designed for education analytics at the Vrije Universiteit Amsterdam (VU). It focuses on predicting the conversion of applicants to enrolled students using a Random Forest model.

# Features

- Reads in and preprocesses data on student applications and enrollments
- Calculates historical conversion rates for different student groups
- Trains a Random Forest model to predict student enrollment based on application data
- Generates predictions for the current academic year

# Data Sources

TBD

# Usage

- Ensure you have the necessary system variables set in your .Renviron file.
- Run the R script to generate the enrollment predictions.

# Technical Details
The project uses the following key libraries and techniques:

- `vusa`: A vusaverse package
- `renv`: For managing package dependencies
- Random Forest modeling with the `ranger` package
- Feature engineering, including handling of missing data and creating lagged conversion rates
