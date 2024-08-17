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
exchange_rate_data1 <- readr::read_csv("Refinitiv_Exchange_Rates.csv")

# Read exchange rate data 'EUR Cross Rates' sheet xlsx from Refinitiv
exchange_rate_data <- readxl::read_excel("EUR Cross Rates 20Y 16082024.xlsx", sheet = "Table Data")

# Remove the first row
exchange_rate_data <- exchange_rate_data[-1, ]

# Format the date column
#exchange_rate_data1$Date <- as.Date(exchange_rate_data$Date, format = "%d/%m/%Y")
exchange_rate_data$Date <- as.Date(exchange_rate_data$Date, format = "%d-%m-%Y")

# Rename column names to currency codes; USD, CNY, AUD, KRW, NZD, CAD, JPY, GBP
colnames(exchange_rate_data) <- c("Date", "USD", "CNY", "AUD", "KRW", "NZD", "CAD", "JPY", "GBP")

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

## ICAP Data Conversion ##

# Relate the column names in the ICAP data to the currency codes
icap_currency_codes <- c("NZD", "EUR", "KRW", "GBP", "CNY", "CNY")

# Create a copy for EUR denominated prices
icap_data_eur <- icap_data

# Function to get the exchange rate for a given currency at the same date as the ICAP data
get_exchange_rate <- function(dates, currency) {
  if (currency %in% colnames(exchange_rate_data)) {
    rates <- exchange_rate_data[dates, currency, drop = FALSE]
    return(as.numeric(rates))
  } else {
    return(rep(NA, length(dates)))  # Return NA if currency not found
  }
}

# Convert the prices in ICAP data
icap_data_eur <- icap_data  # Make a copy of the original data

dates <- index(icap_data_eur)  # Get the dates

for (i in seq_along(icap_currency_codes)) {
  currency <- icap_currency_codes[i]
  if (currency != "EUR") {  # Skip conversion if the currency is already EUR
    if (i <= ncol(icap_data_eur)) {  # Check if the column index is valid
      exchange_rates <- get_exchange_rate(dates, currency)
      
      # Only divide if exchange rate is not NA and greater than 0
      valid_indices <- !is.na(exchange_rates) & exchange_rates > 0
      if (any(valid_indices)) {
        icap_data_eur[valid_indices, i] <- icap_data_eur[valid_indices, i] / exchange_rates[valid_indices]
      }
    }
  }
}

# Optional: Inspect the result
print(tail(icap_data_eur))
print(tail(icap_data))

# Sample from 30/11/2023 to 31/12/2023 from both datasets
#--------------------------------------------------------
# Define the date range
start_date <- as.Date("2023-11-30")
end_date <- as.Date("2023-12-31")

# Extract dates from the zoo objects
dates_icap <- index(icap_data)
dates_icap_eur <- index(icap_data_eur)

# Filter icap_data for the specified date range using logical indexing
filtered_icap_data <- icap_data[dates_icap >= start_date & dates_icap <= end_date]

# Filter icap_data_eur for the specified date range using logical indexing
filtered_icap_data_eur <- icap_data_eur[dates_icap_eur >= start_date & dates_icap_eur <= end_date]

# Convert filtered zoo objects to data frames
filtered_icap_data_df <- data.frame(Date = index(filtered_icap_data), coredata(filtered_icap_data))
filtered_icap_data_eur_df <- data.frame(Date = index(filtered_icap_data_eur), coredata(filtered_icap_data_eur))

# Print the filtered data to compare
print("Filtered icap_data:")
print(filtered_icap_data_df)

print("Filtered icap_data_eur:")
print(filtered_icap_data_eur_df)

colnames(filtered_icap_data_eur_df)

# Optionally, you can also compare specific columns to see if the conversion has worked
# For example, if you want to compare a column named 'NZU'
icap_comparison <- data.frame(
  Date = filtered_icap_data_df$Date,
  Original_Value = filtered_icap_data_df$New.Zealand.Emissions.Trading.System,
  Converted_Value = filtered_icap_data_eur_df$New.Zealand.Emissions.Trading.System
)

print("Comparison of Original and Converted Values:")
print(icap_comparison)
#--------------------------------------------------------


## Clearblue Data Conversion ##

# Relate the column names in the ICAP data to the currency codes
clearblue_currency_codes <- c("AUD", "CNY", "EUR", "GBP", "NZD", "KRW", "USD", "USD")

# Create a copy for EUR denominated prices
clearblue_data_eur <- clearblue_data

dates <- index(clearblue_data_eur)  # Get the dates

# Get the last available date in the exchange rate data
max_exchange_rate_date <- max(index(exchange_rate_data))

for (i in seq_along(clearblue_currency_codes)) {
  currency <- clearblue_currency_codes[i]
  if (currency != "EUR") {  # Skip conversion if the currency is already EUR
    if (i <= ncol(clearblue_data_eur)) {  # Check if the column index is valid
      # Filter dates that are within the range of the exchange rate data
      valid_dates <- dates[dates <= max_exchange_rate_date]
      
      # Get exchange rates only for the valid dates
      exchange_rates <- get_exchange_rate(valid_dates, currency)
      
      # Only divide if exchange rate is not NA and greater than 0
      valid_indices <- !is.na(exchange_rates) & exchange_rates > 0
      
      if (any(valid_indices)) {
        # Apply the conversion only to valid dates
        clearblue_data_eur[valid_dates[valid_indices], i] <- clearblue_data_eur[valid_dates[valid_indices], i] / exchange_rates[valid_indices]
      }
    }
  }
}

# Optional: Inspect the result

print(tail(clearblue_data))
print(tail(exchange_rate_data))
print(tail(clearblue_data_eur))

# Sample from 30/11/2023 to 31/12/2023 from both datasets
#--------------------------------------------------------
# Define the date range
start_date <- as.Date("2023-11-30")
end_date <- as.Date("2023-12-31")

# Extract dates from the zoo objects
dates_clearblue <- index(clearblue_data)
dates_clearblue_eur <- index(clearblue_data_eur)

# Filter clearblue_data for the specified date range using logical indexing
filtered_clearblue_data <- clearblue_data[dates_clearblue >= start_date & dates_clearblue <= end_date]

# Filter clearblue_data_eur for the specified date range using logical indexing
filtered_clearblue_data_eur <- clearblue_data_eur[dates_clearblue_eur >= start_date & dates_clearblue_eur <= end_date]

# Convert filtered zoo objects to data frames
filtered_clearblue_data_df <- data.frame(Date = index(filtered_clearblue_data), coredata(filtered_clearblue_data))
filtered_clearblue_data_eur_df <- data.frame(Date = index(filtered_clearblue_data_eur), coredata(filtered_clearblue_data_eur))

# Print the filtered data to compare
print("Filtered clearblue_data:")
print(filtered_clearblue_data_df)

print("Filtered clearblue_data_eur:")
print(filtered_clearblue_data_eur_df)

# Optionally, you can also compare specific columns to see if the conversion has worked
# For example, if you want to compare a column named 'NZU'
comparison <- data.frame(
  Date = filtered_clearblue_data_df$Date,
  Original_Value = filtered_clearblue_data_df$NZU,
  Converted_Value = filtered_clearblue_data_eur_df$NZU
)

icap_cleablue_comparison <- data.frame(
  Date = filtered_clearblue_data_df$Date,
  ICAP = filtered_icap_data_eur_df$New.Zealand.Emissions.Trading.System,
  Clearblue = filtered_clearblue_data_eur_df$NZU
)

print("Comparison of Original and Converted Values:")
print(comparison)
#--------------------------------------------------------


## NEED TO EXTEND THE EXHCANGE RATE DATA SET, NEED TO UNDERSTAND WHY CNVERSIONS ARE STILL HAPPENING WHEN NO EXCHANGE RATE DATA IS AVAILABLE

#--------------------------------------------------------------

### Export Converted Datasets as csv ###

#--------------------------------------------------------------.

# Extract the Date index and convert zoo objects to dataframes
clearblue_data_eur_df <- data.frame(Date = index(clearblue_data_eur), coredata(clearblue_data_eur))
icap_data_eur_df <- data.frame(Date = index(icap_data_eur), coredata(icap_data_eur))

# Export the dataframes to CSV files
write.csv(clearblue_data_eur_df, "clearblue_data_eur.csv", row.names = FALSE)
write.csv(icap_data_eur_df, "icap_data_eur.csv", row.names = FALSE)

# Publish both data sets to Git
setwd(Git)
# Final Data Set
write.csv(clearblue_data_eur_df, "clearblue_data_eur.csv", row.names = FALSE)
write.csv(icap_data_eur_df, "icap_data_eur.csv", row.names = FALSE)

#--------------------------------------------------------------
