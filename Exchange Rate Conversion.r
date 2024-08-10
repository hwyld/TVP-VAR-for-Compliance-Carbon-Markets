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
exchange_rate_data$Date <- as.Date(exchange_rate_data$Date, format = "%Y-%m-%d")

# Read the ICAP data
icap_data <- readr::read_csv("ICAP_secondary_market_data.csv")

# Format the date column
icap_data$Date <- as.Date(icap_data$Date, format = "%Y-%m-%d")

# Remove the first column (assuming it's an index column)
icap_data <- icap_data[, -1]

# Read the Clearblue data
clearblue_data <- readr::read_csv("Clearblue_market_data.csv")

# Format the date column
clearblue_data$Date <- as.Date(clearblue_data$Date, format = "%Y-%m-%d")

# Remove the first column (assuming it's an index column)
clearblue_data <- clearblue_data[, -1]

## Convert all domestic currency prices into EUR denominated prices ##
#-----------------------------------------

# Extract the currency codes from the column names of the exchange rate data
currency_codes <- colnames(exchange_rate_data)[-1]

# Relate the column names in the ICAP data to the currency codes
icap_currency_codes <- c("NZD", "EUR", "KRW", "GBP", "CNY", "CNY")

# Convert the ICAP data into EUR denominated prices
icap_data_eur <- icap_data

# Function to get the exchange rate for a given currency
get_exchange_rate <- function(currency) {
  rate <- as.numeric(tail(exchange_rate_data[[currency]], 1))
  return(rate)
}

# Convert the prices in ICAP data
for (i in seq_along(icap_currency_codes)) {
  currency <- icap_currency_codes[i]
  if (currency != "EUR") {  # Skip conversion if the currency is already EUR
    exchange_rate <- get_exchange_rate(currency)
    
    if (!is.na(exchange_rate) && exchange_rate > 0) {
      icap_data_eur[, i] <- icap_data_eur[, i] / exchange_rate
    } else {
      message(paste("Exchange rate for", currency, "not found or invalid."))
    }
  }
}
