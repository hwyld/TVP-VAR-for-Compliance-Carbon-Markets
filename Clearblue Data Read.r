## Econometrics Research Project , Semester 2, 2024
## The pursuit of a global carbon market; A time varying analysis of international compliance carbon markets
## Read the Clearblue data and format in R
## Author: Henry Wyld
## Date of creation: 2024-07-19

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
setwd(Clearblue_Data)

## Read the datasets from CSV files ##

#-----------------------------------------

# Set Paths to the Excel files
path <- Clearblue_Data

# Create loop to read all CSV files in the directory
files <- list.files(path, pattern = "*.csv", full.names = TRUE)

# Print the names of the files being processed
print("Files being processed:")
print(files)

read_and_format_csv <- function(file) {
  # Print the file name being processed
  print(paste("Processing file:", file))
  
  # Initially read only the first two rows to get the column names and number of columns
  temp_df <- read.csv(file, nrows = 2, header = FALSE)
  col_names <- as.character(unlist(temp_df[1, ])) # Save the column names from the first row
  num_cols <- ncol(temp_df)
  
  # Print the column names and number of columns
  print("Column names:")
  print(col_names)
  print(paste("Number of columns:", num_cols))
  
  # Create a vector of column types with 'Date' for the first column and 'numeric' for the others
  col_types <- c("Date", rep("numeric", num_cols - 1))
  
  # Read the entire dataset with the specified column types, skipping the first row
  df <- read.csv(file, skip = 1, header = FALSE, colClasses = col_types)
  
  # Replace the column names with the saved column names
  colnames(df) <- col_names
  
  # Print the first few rows of the dataframe
  print("First few rows of the dataframe:")
  print(head(df))
  
  # Return the formatted dataframe
  return(df)
}

# Run the function across the list of files creating dataframes for each file
dataframes <- lapply(files, read_and_format_csv)

# Create a list of dataframes with meaningful names
dataframes <- setNames(dataframes, gsub(".csv", "", list.files(path, pattern = "*.csv")))

# Check the structure of the dataframes
lapply(dataframes, head)

# Convert the list of dataframes to individual dataframes
dataframes <- lapply(dataframes, as.data.frame)

# Create the dataframes in the global environment
list2env(dataframes, envir = .GlobalEnv)

#-----------------------------------------

### Data Merging ###

#-----------------------------------------

# Merge the dataframes
merged_data <- Reduce(function(x, y) merge(x, y, by = "DateTime", all = TRUE), dataframes)

# Order the data by DateTime
merged_data <- merged_data[order(merged_data$DateTime), ]

# Define the list of columns to keep ; 
# ACCU Spot Price - RepuTex = ACCU Spot Price, 
# CEA Closing Price = China Emission Allowance Close, 
# Front December - ICE.x = EUA Front December - ICE, 
# Front December - ICE.y = UKA Front December - ICE, 
# NZUs - Spot = NZU Spot Price , 
# KAU Closing Price = Korean Allowance Close , 
# Washington Carbon Allowance Front December = Washington Front December - ICE , 
# CCA - Front December - ICE = CCA Front December - ICE
cols_to_keep <- c("DateTime", "ACCU Spot Price - RepuTex", "CEA Closing Price", "Front December - ICE.x", "Front December - ICE.y", "NZUs - Spot", "KAU Closing Price", "Washington Carbon Allowance Front December", "CCA - Front December - ICE")

# Subset the dataframe to only include these columns
merged_data <- merged_data[, cols_to_keep]

# Rename the columns
colnames(merged_data) <- c("Date", "ACCU", "CEA", "EUA", "UKA", "NZU", "KAU", "WCA", "CCA")

### Data Trimming  ###

#-------------------------------------

# Convert the data to a time series
merged_data$Date <- as.Date(merged_data$Date, format = "%Y-%m-%d")

# Remove weekends and public holidays
merged_data_trimmed <- merged_data[!weekdays(merged_data$Date) %in% c("Saturday", "Sunday"), ]

# Order the data by date
merged_data_trimmed <- merged_data_trimmed[order(merged_data_trimmed$Date), ]

# Check for duplicates
duplicated_rows <- merged_data_trimmed[duplicated(merged_data_trimmed), ]

# Check max and min dates
max_date <- max(merged_data_trimmed$Date)
min_date <- min(merged_data_trimmed$Date)

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
p <- plot_all_columns(merged_data_trimmed)

# Save the plot
htmlwidgets::saveWidget(p, "Clearblue_Allowance_Price_Plot.html")

#---------------------------------------

#### Export the data ####
# Export cleaned and trimmed data
#---------------------------------------
write.csv(merged_data_trimmed, "Clearblue_market_data.csv")

# Publish both data sets to Git
setwd(Git)
# Final Data Set
write.csv(merged_data_trimmed, "Clearblue_market_data.csv")
# Final HTML file
htmlwidgets::saveWidget(p, "Clearblue_Allowance_Price_Plot.html")
#---------------------------------------

# stop the script
stop()