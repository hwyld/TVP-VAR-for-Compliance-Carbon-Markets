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

#-----------------------------------------

### Convert all dataframes to Zoo objects ###

#--------------------------------------------------------------

# Convert the ICAP data to a zoo object
icap_data <- zoo::zoo(icap_data[, -1], order.by = icap_data$Date)

# Convert the Clearblue data to a zoo object
clearblue_data <- zoo::zoo(clearblue_data[, -1], order.by = clearblue_data$Date)

# Convert the exchange rate data to a zoo object
exchange_rate_data <- zoo::zoo(exchange_rate_data[, -1], order.by = exchange_rate_data$Date)


#--------------------------------------------------------------


### Convert all domestic currency prices into EUR denominated prices ###

#--------------------------------------------------------------

# Extract the currency codes from the column names of the exchange rate data
currency_codes <- colnames(exchange_rate_data)[-1]

# Relate the column names in the ICAP data to the currency codes
icap_currency_codes <- c("NZD", "EUR", "KRW", "GBP", "CNY", "CNY")

# Create a copy for EUR denominated prices
icap_data_eur <- icap_data

# Function to get the exchange rate for a given currency at the same date as the ICAP data
get_exchange_rate <- function(date, currency) {
  if (date %in% index(exchange_rate_data) && currency %in% colnames(exchange_rate_data)) {
    rate <- as.numeric(exchange_rate_data[date, currency])
    return(rate)
  } else {
    return(NA)  # Return NA if date or currency not found
  }
}

# Convert the prices in ICAP data
for (i in seq_along(icap_currency_codes)) {
  currency <- icap_currency_codes[i]
  if (currency != "EUR") {  # Skip conversion if the currency is already EUR
    # Check if the currency column exists in icap_data_eur
    if (i <= ncol(icap_data_eur)) {
      # Iterate over each date in the icap_data_eur zoo object
      for (date in index(icap_data_eur)) {
        exchange_rate <- get_exchange_rate(date, currency)
        
        if (!is.na(exchange_rate) && exchange_rate > 0) {
          icap_data_eur[date, i] <- icap_data_eur[date, i] / exchange_rate
        }
      }
    }
  }
}

stop()
#--------------------------------------------------------------