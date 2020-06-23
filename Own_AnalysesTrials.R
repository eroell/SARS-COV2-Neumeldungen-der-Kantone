# Author: Eljas Roellin
# Date: 28.05.2020
# Script adapted from Peter Moser on GitHub, 28.05.2020 on
# https://github.com/statistikZH/covid19monitoring_health_covid19cases/blob/master/Health_covid19cases.Rhttps://github.com/statistikZH/covid19monitoring_health_covid19cases/blob/master/Health_covid19cases.R

# Purpose: Own visualization of registered SARS-COV2-Cases in Switzerland


# Import libraries
library(dplyr) # Version: '0.8.5'
library(tidyr) # '1.0.2'
library(lubridate) # '1.7.4'
library(reshape2)
library(xts)
library(lattice)

# Read in Data
url_cases  <- "https://raw.githubusercontent.com/openZH/covid_19/master/COVID19_Fallzahlen_CH_total.csv"
cases <- read.csv(url(url_cases), header=T, sep=",", stringsAsFactors=FALSE, encoding="UTF-8")

# Filter for Zug
cumul_casesZG <- filter(cases, abbreviation_canton_and_fl == "ZG")

# Set NA to non-NA before
cumul_casesZG <- na.locf(cumul_casesZG, na.rm = FALSE)

plot(cumul_casesZG$ncumul_conf)
