### Econometrics Research Project , Semester 2, 2024
## The pursuit of a global carbon market; A time varying analysis of international compliance carbon markets
## Compare datasets from both sources and create the research data
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


####### Import and Format Data #######
#---------------------------------------
# Read the CSV file
#ICAP_df <- readr::read_csv("ICAP_EUR_denom_allowance_prices_trimmed.csv", locale = readr::locale(encoding = "UTF-8"))
Clearblue_df <- readr::read_csv("clearblue_data_eur.csv", locale = readr::locale(encoding = "UTF-8"))
ICAP_df <- readr::read_csv("icap_data_eur.csv", locale = readr::locale(encoding = "UTF-8"))

# Rename the DateTime column to Date
#Clearblue_df <- rename(Clearblue_df, Date = DateTime)

# Function to convert dataframe to xts, assuming the first column is the date
convert_to_xts <- function(df, date_col_name, date_format = "%Y-%m-%d") {
  # Check if the date column exists
  if (!date_col_name %in% names(df)) {
    stop("Date column specified does not exist in the dataframe")
  }
  
  # Convert the date column to Date class
  date_col <- as.Date(df[[date_col_name]], format = date_format)
  
  # Convert all other columns to numeric
  data_cols <- df[, !(names(df) %in% date_col_name)]
  data_cols <- data.frame(lapply(data_cols, function(x) as.numeric(as.character(x))))
  
  # Create the xts object
  xts_object <- xts(data_cols, order.by = date_col)
  
  return(xts_object)
}

# Use ICAP_df and Clearblue_df to create xts objects
ICAP_df_xts <- convert_to_xts(ICAP_df, "Date")
Clearblue_df_xts <- convert_to_xts(Clearblue_df, "Date")


head(ICAP_df_xts, 5)
head(Clearblue_df_xts, 5)

#---------------------------------------

#### Plot the data - Allowance Price ####
#---------------------------------------

## Domestic Currency Allowance prices ##

# Convert _df_xts to a dataframe with a Date column
ICAP_df_xts_df <- data.frame(Date = index(ICAP_df_xts), coredata(ICAP_df_xts))
Clearblue_df_xts_df <- data.frame(Date = index(Clearblue_df_xts), coredata(Clearblue_df_xts))

# Create a plotly plot for all columns
plot_all_columns <- function(data) {
  p <- plot_ly(data, x = ~Date)
  
  # Iterate over each column except the Date column
  for (col in colnames(data)[-which(colnames(data) == "Date")]) {
    p <- add_trace(p, y = as.formula(paste0("~`", col, "`")), name = col, type = 'scatter', mode = 'lines')
  }
  
  p <- layout(p, title = 'EUR denominated Price Data',
              xaxis = list(title = 'Date'),
              yaxis = list(title = 'Value'))
  
  return(p)
}

# Call the function to plot the data
p <- plot_all_columns(ICAP_df_xts_df)

# Save the plot
htmlwidgets::saveWidget(p, "ICAP_Price_Plot.html")

# Call the function to plot the data
p <- plot_all_columns(Clearblue_df_xts_df)

# Save the plot
htmlwidgets::saveWidget(p, "Clearblue_Price_Plot.html")

#---------------------------------------

#### Merge datasets and Plot the data - Allowance Price ####

#---------------------------------------

# Merge the dataframes on the Date column
Merged_df_xts_df <- merge(ICAP_df_xts_df, Clearblue_df_xts_df, by = "Date", all = TRUE)

# Optionally, handle missing values (e.g., fill NA with 0)
#Merged_df_xts_df[is.na(Merged_df_xts_df)] <- 0

# Print the first few rows of the merged dataframe
print(head(Merged_df_xts_df))

# Call the function to plot the data
p <- plot_all_columns(Merged_df_xts_df)

# Save the plot
htmlwidgets::saveWidget(p, "Merged_Price_Plot.html")

#---------------------------------------

####### Summary Statistics #######
#---------------------------------------
# Function to find start and end dates excluding NA
get_valid_dates <- function(series) {
  valid_dates <- index(series[!is.na(series)])  # Get dates for non-NA values
  start_date <- format(min(valid_dates), "%Y-%m-%d")
  end_date <- format(max(valid_dates), "%Y-%m-%d")
  return(c(Start = start_date, End = end_date))
}

# Apply the function to each column
valid_date_info_ICAP <- sapply(ICAP_df_xts, get_valid_dates)
valid_date_info_Clearblue <- sapply(Clearblue_df_xts, get_valid_dates)

# Check for NA-only columns and apply describe safely
safe_describe <- function(x) {
  non_na_values <- x[!is.na(x)]
  if (length(non_na_values) == 0) {
    return(list(Count = NA, Mean = NA, SD = NA))  # Return NA for stats if no data
  } else {
    desc <- describe(non_na_values)
    # Ensure the returned list has consistent names
    return(list(Count = desc$n, Mean = desc$mean, SD = desc$sd))
  }
}

# Apply the function to each column
summary_stats_ICAP <- sapply(ICAP_df_xts, safe_describe, simplify = FALSE)
summary_stats_Clearblue <- sapply(Clearblue_df_xts, safe_describe, simplify = FALSE)

# Convert summary statistics to a data frame
summary_stats_to_df <- function(stats) {
  stats_df <- do.call(rbind, lapply(stats, function(x) as.data.frame(t(x))))
  return(stats_df)
}

# Convert your summary statistics arrays to data frames
summary_stats_ICAP_df <- summary_stats_to_df(summary_stats_ICAP)
summary_stats_Clearblue_df <- summary_stats_to_df(summary_stats_Clearblue)

# Convert date info arrays to data frames
valid_date_info_ICAP_df <- dates_to_df(valid_date_info_ICAP)
valid_date_info_Clearblue_df <- dates_to_df(valid_date_info_Clearblue)

# Combine summary stats with the corresponding date info
ICAP_combined_df <- cbind(valid_date_info_ICAP_df, summary_stats_ICAP_df)
Clearblue_combined_df <- cbind(valid_date_info_Clearblue_df, summary_stats_Clearblue_df)

# Merge the combined data frames and order by oldest start date
merged_dates <- rbind(ICAP_combined_df, Clearblue_combined_df)
merged_dates <- merged_dates[order(as.Date(merged_dates$Start, "%Y-%m-%d")), ]

# Display the merged and ordered data frame
print(merged_dates)

# Export as table in html format
html_table <- kable(merged_dates, format = "html", escape = FALSE) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = FALSE, 
                position = "center") %>%
  column_spec(1, bold = TRUE) %>%  # Bold the row names
  row_spec(0, bold = TRUE) %>%     # Bold the column names
  kable_styling(latex_options = "hold_position", 
                position = "center", 
                stripe_color = "gray!15", 
                stripe_index = seq(1, nrow(merged_dates), 2)) %>%
  add_header_above(c(" " = 1, "Summary Statistics" = ncol(merged_dates) - 1)) %>%
  kable_styling(latex_options = c("striped", "hold_position"))

# Save the HTML table to a file
writeLines(html_table, "Summary_Statistics.html")

#---------------------------------------

####### Create the Research Data #######
# Key dates:
  # Jan 2010, start of NZU and EUA time series
  # Apr 2014, start of HBEA time series
  # Jan 2015, start of KAU and CCA time series
  # Jan 2018, start of ACCU time series
  # May 2021, start of UKA time series
  # Jul 2021, start of CEA time series
  # Dec 2022, start of WCA time series

# Rolling 3 year windows?

#---------------------------------------

# Remove CEA, EUA, NZU, KAU from merged dataset
# Find the column numbers corresponding to the specified column names
columns_to_remove <- which(colnames(Merged_df_xts_df) %in% c("United.Kingdom.Emissions.Trading.Scheme", "CEA", "EUA", "NZU", "KAU"))

# Remove the columns by their indices
Research_Data <- Merged_df_xts_df[, -columns_to_remove]

# Check the remaining column names
colnames(Research_Data)

# Print the first few rows of the updated dataframe
print(head(Research_Data))

length(colnames(Research_Data))

# Define new column names
new_column_names <- c("Date", "NZU", "EUA", "KAU", "CEA", "HBEA", "ACCU", "UKA", "WCA", "CCA")

length(new_column_names)

# Rename columns of Merged_df_xts_df
colnames(Research_Data) <- new_column_names

# Print the first few rows of the updated dataframe
print(head(Research_Data))

# Upload Research Data to global environment
assign("Research_Data", Research_Data, envir = .GlobalEnv)

# plot the data
# Call the function to plot the data
p <- plot_all_columns(Research_Data)

# Save the plot
htmlwidgets::saveWidget(p, "Research_Data_Price_Plot.html")

#---------------------------------------

####### Data Transformation ########

### Handling Missing Data/NAs ###
## Forward Fill NA values ##
#---------------------------------------
# Summarize NA presence in Research_Data

#---------------------------------------

####### Summary Stats on Research Data #######

#---------------------------------------

# Use Research_data_df to create xts objects
Research_Data_xts <- convert_to_xts(Research_Data, "Date")

# Apply the function to each column
valid_date_info_Research_data <- sapply(Research_Data_xts, get_valid_dates)

# Check for NA-only columns and apply describe safely
safe_describe <- function(x) {
  non_na_values <- x[!is.na(x)]
  if (length(non_na_values) == 0) {
    return(list(Count = NA, Mean = NA, SD = NA))  # Return NA for stats if no data
  } else {
    desc <- describe(non_na_values)
    # Ensure the returned list has consistent names
    return(list(Count = desc$n, Mean = desc$mean, SD = desc$sd))
  }
}

# Apply the function to each column
summary_stats_Research_data <- sapply(Research_Data_xts, safe_describe, simplify = FALSE)

# Convert your summary statistics arrays to data frames
summary_stats_Research_data_df <- summary_stats_to_df(summary_stats_Research_data)

# Convert date info arrays to data frames
valid_date_info_Research_data_df <- dates_to_df(valid_date_info_Research_data)

# Combine summary stats with the corresponding date info
Research_data_combined_df <- cbind(valid_date_info_Research_data_df, summary_stats_Research_data_df)

# Merge the combined data frames and order by oldest start date
merged_dates <- merged_dates[order(as.Date(Research_data_combined_df$Start, "%Y-%m-%d")), ]

# Display the merged and ordered data frame
print(merged_dates)

# Save the HTML table to a file
html_table <- kable(merged_dates, format = "html", escape = FALSE)

# Save the HTML table to a file
writeLines(html_table, "Summary_Statistics_Research_Data.html")


# Export to Git
#---------------------------------------
# Publish both data sets to Git
setwd(Git)
# Final Data Set
write.csv(Research_Data, "Research_data.csv")
#---------------------------------------
