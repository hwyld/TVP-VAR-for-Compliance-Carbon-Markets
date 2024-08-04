### Econometrics Research Project , Semester 2, 2024
## The pursuit of a global carbon market; A time varying analysis of international compliance carbon markets
## Read the ICAP Price Explorer data and format in R
## Author: Henry Wyld
## Date of creation: 2024-07-19

#-------------------------------------
# clear memory
rm(list = ls())
#----------------------------------

## Packages ##
#----------------------------------
# Source the package setup script
Git <- "C:/Users/henry/OneDrive - The University of Melbourne/GitHub/TVP-VAR-for-Compliance-Carbon-Markets"
setwd(Git)
source("Packages.R")

#----------------------------------

# Set the working directory
setwd(ICAP_Data)

## ICAP Data ##
#----------------------------------
# Sourced from https://icapcarbonaction.com/en/ets-prices
# Only want to keep spot price including EU ETS, NZ ETS, K ETS, China ETS, Hubei ETS ;  https://icapcarbonaction.com/en/documentation-allowance-price-explorer

# Read the csv file
df <- readr::read_csv("Primary.csv")

## Save the ETS Order

# Extract only the headers from df
headers <- colnames(df)

# Remove non-text data in headers
headers <- headers[!grepl("[0-9]", headers)]

# Add Date to the headers
headers <- c("Date", headers)

## Only keep those price series with a secondary market, i.e. exclude the primary market
# Replace the Column names with the first row
colnames(df) <- df[1, ]

# Find the indices of the column names that contain "Secondary Market"
indices <- grep("Secondary Market", colnames(df))

# Create new dataframe with only the columns with "Secondary Market" in the name
secondary_market_data <- df[, c(1, indices)]

# Replace the column names with headers
colnames(secondary_market_data) <- headers

head(secondary_market_data,5)

# Remove the first row
secondary_market_data <- secondary_market_data[-c(1:1), ]

#----------------------------------

####### Data Cleaning ########

#---------------------------------------

# Convert the data to a time series
secondary_market_data$Date <- as.Date(secondary_market_data$Date, format = "%Y-%m-%d")

# Remove weekends and public holidays
secondary_market_data_trimmed <- secondary_market_data[!weekdays(secondary_market_data$Date) %in% c("Saturday", "Sunday"), ]

# Order the data by date
secondary_market_data_trimmed <- secondary_market_data_trimmed[order(secondary_market_data_trimmed$Date), ]

# Check for duplicates
duplicated_rows <- secondary_market_data_trimmed[duplicated(secondary_market_data_trimmed), ]

# Check max and min dates
max_date <- max(secondary_market_data_trimmed$Date)
min_date <- min(secondary_market_data_trimmed$Date)

#---------------------------------------

#### Plot the data - Allowance Price ####
#---------------------------------------

## Domestic Currency Allowance prices ##

# Create a plotly plot for all columns
plot_all_columns <- function(data) {
  p <- plot_ly(data, x = ~Date)
  
  # Iterate over each column except the Date column
  for (col in colnames(data)[-which(colnames(data) == "Date")]) {
    p <- add_trace(p, y = as.formula(paste0("~`", col, "`")), name = col, type = 'scatter', mode = 'lines')
  }
  
  p <- layout(p, title = 'Secondary Market Data',
              xaxis = list(title = 'Date'),
              yaxis = list(title = 'Value'))
  
  return(p)
}

# Call the function to plot the data
p <- plot_all_columns(secondary_market_data_trimmed)

# Save the plot
htmlwidgets::saveWidget(p, "Allowance_Price_Plot.html")

#---------------------------------------

#### Export the data ####
# Export cleaned and trimmed data
#---------------------------------------
write.csv(secondary_market_data_trimmed, "ICAP_secondary_market_data.csv")

# Publish both data sets to Git
setwd(Git)
# Final Data Set
write.csv(secondary_market_data_trimmed, "ICAP_secondary_market_data.csv")
# Final HTML file
htmlwidgets::saveWidget(final_plot, "EUR_Allowance_Price_Plot.html")
#---------------------------------------

# stop the script
stop()