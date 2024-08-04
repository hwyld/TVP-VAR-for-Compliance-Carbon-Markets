## Econometrics Research Project , Semester 2, 2024
## The pursuit of a global carbon market; A time varying analysis of international compliance carbon markets
## Converting all domestic currency prices into EUR denominated prices
## Author: Henry Wyld
## Date of creation: 2024-08-04

#-------------------------------------
# clear memory
rm(list=ls())    
#----------------------------------

## Packages ##
#----------------------------------
# Source the package setup script
Git <- "C:/Users/henry/OneDrive - The University of Melbourne/GitHub/TVP-VAR-for-Compliance-Carbon-Markets"
setwd(Git)
source("Packages.R")

# Set the working directory
setwd(Git)

## Read the datasets from CSV files ##

#-----------------------------------------

# Read exchange rate data
exchange_rate_data <- readr::read_csv("Refinitiv_Exchange_Rates.csv")

# Format the date column
exchange_rate_data$Date <- as.Date(exchange_rate_data$Date, format = "%d/%m/%Y")


