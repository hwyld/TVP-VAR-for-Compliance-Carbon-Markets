### Econometrics Research Project , Semester 2, 2024
## Transform data into weekly returns and volatility, run summary statistics
## Author: Henry Wyld
## Date of creation: 2024-03-31

# #-------------------------------------
# # clear memory
# rm(list=ls())    
# #----------------------------------

# ## Packages ##
# #----------------------------------
# # Source the package setup script
# Git <- "C:/Users/henry/OneDrive - The University of Melbourne/GitHub/TVP-VAR-for-Compliance-Carbon-Markets"
# setwd(Git)
# source("Packages.R")

####### Import and Format Data #######
#---------------------------------------
# Read the CSV file
cleaned_datasets <- read_csv("Research_data.csv", locale = locale(encoding = "UTF-8"))

# Verify the structure of the data
print(tail(cleaned_datasets))

# Convert dataframe to xts, assuming the first column after Date removal is the date
cleaned_datasets <- cleaned_datasets[, -1]  # Removes the first column
print(tail(cleaned_datasets))

# Modify the function to explicitly use dplyr's select
convert_to_xts <- function(df, date_col_name, date_format = "%Y-%m-%d") {
    if (!date_col_name %in% names(df)) {
        stop("Date column specified does not exist in the dataframe: ", date_col_name)
    }
  
    df[[date_col_name]] <- as.Date(df[[date_col_name]], format = date_format)
  
    # Explicitly use dplyr's select function
    data_cols <- dplyr::select(df, -dplyr::all_of(date_col_name)) %>%
        mutate(across(everything(), as.numeric))
  
    xts_object <- xts(data_cols, order.by = df[[date_col_name]])
    return(xts_object)
}

# Convert data to xts using the function
cleaned_datasets_xts <- convert_to_xts(cleaned_datasets, "Date")
print(tail(cleaned_datasets_xts))

####### Calculate Weekly Returns #######
#---------------------------------------

# Define the function to calculate weekly returns from Friday-to-Friday.
# The function assumes the data is already in xts format.
calculate_weekly_returns <- function(data) {
  # Ensure that the 'data' is an xts object and has a proper Date index 
  # Align data to Friday to Friday returns only
  fridays <- which(format(index(data), "%A") == "Friday")
  aligned_data <- data[fridays]
  returns <- diff(log(aligned_data))
  return(na.omit(returns))
}
# Initialize the weekly returns list with proper names
weekly_returns_list <- setNames(vector("list", ncol(cleaned_datasets_xts)), colnames(cleaned_datasets_xts))

# Calculate weekly returns
for (i in seq_along(weekly_returns_list)) {
  weekly_returns_list[[i]] <- calculate_weekly_returns(cleaned_datasets_xts[, i])
}

# Combine the weekly returns into a single xts object
weekly_returns <- do.call(merge, weekly_returns_list)
print(head(weekly_returns))


#---------------------------------------

## Double Check Friday to Friday - Can remove ##
#---------------------------------------
# # Define a custom function to calculate weekly returns from Friday-to-Friday
# calculate_weekly_returns <- function(data) {
#   # Ensure that the 'data' is an xts object and has a proper Date index
#   if (!inherits(data, "xts")) stop("Data must be an xts object")

#  # Find indexes for all Fridays within the data range
#   fridays <- which(format(index(data), "%A") == "Friday")

#   # Make sure it starts and ends with Fridays (if not, adjust accordingly)
#   if (length(fridays) > 1) {
#     aligned_data <- data[fridays]

#     # Calculate weekly returns using logarithmic differences between consecutive Fridays
#     returns <- diff(log(aligned_data))
#   } else {
#     returns <- xts()  # Return an empty xts object if there are not enough Fridays
#   }

#   return(na.omit(returns))
# }

# # Initialize the weekly returns list with proper names
# weekly_returns_list <- setNames(vector("list", ncol(cleaned_datasets_xts)), colnames(cleaned_datasets_xts))

# # Calculate weekly returns
# for (i in seq_along(weekly_returns_list)) {
#   weekly_returns_list[[i]] <- calculate_weekly_returns(cleaned_datasets_xts[, i])
# }

# # Combine the weekly returns into a single xts object
# weekly_returns1 <- do.call(merge, weekly_returns_list)
# print(head(weekly_returns1))
# print(head(weekly_returns))

# print(tail(weekly_returns1))
# print(tail(weekly_returns))

# # Save weekly index for later use
# # weekly_index_returns <- cut(index(weekly_returns), breaks = "week", labels = FALSE)
#---------------------------------------

#### Annualised Weekly Volatilty #### 
# From Paper "The main measure is the standard deviation of weekly return over the five-day interval during each week"
#---------------------------------------
# Assuming cleaned_datasets_xts is already loaded and is an xts object
daily_returns_xts <- diff(log(cleaned_datasets_xts))  # calculate daily log returns

print(tail(daily_returns_xts))

# Create a weekly index to group by trading weeks Friday to Friday
weekly_index <- cut(index(daily_returns_xts), breaks = "week", labels = FALSE)

# Save daily index for later use
daily_index <- index(daily_returns_xts)

# Add the weekly index to the daily_returns data frame
daily_returns <- cbind(as.data.frame(daily_returns_xts), Week = weekly_index)

# Check the structure of daily_returns
str(daily_returns)

# Convert daily_returns to an xts object
#daily_returns <- as.xts(daily_returns, order.by = daily_index)

# Calculate the average of the daily returns for each week and each market using aggregate 
avg_weekly_returns <- aggregate(daily_returns, by = list(Week = weekly_index), FUN = mean)

# Calculate the number of valid observations for each week
num_trading_days <- aggregate(!is.na(daily_returns), by = list(Week = weekly_index), FUN = sum)

# Drop the last column from the avg_weekly_returns dataframe
avg_weekly_returns <- avg_weekly_returns[, -ncol(avg_weekly_returns)]
num_trading_days <- num_trading_days[, -ncol(num_trading_days)]

# Loop through each column in avg_weekly_returns and add it to daily_returns
for (col in colnames(avg_weekly_returns)[-1]) {  # Exclude the 'Week' column
  daily_returns[[paste0(col, "_avg")]] <- avg_weekly_returns[[col]][match(daily_returns$Week, avg_weekly_returns$Week)]
}

# Calculate squared differences for all columns in daily_returns
for (col in colnames(avg_weekly_returns)[-1]) {  # Exclude the 'Week' column
  daily_returns[[paste0(col, "_diff")]] <- (daily_returns[[col]] - daily_returns[[paste0(col, "_avg")]])^2
}

# Put the squared differences into a new data frame
squared_diff <- daily_returns[, c("Week", grep("_diff$", colnames(daily_returns), value = TRUE))]

print(tail(daily_returns))
print(tail(squared_diff))

# Sum the squared differences for each week and each market, keep the weekly index as the first column
sum_squared_diff <- aggregate(squared_diff, by = list(Week = weekly_index), FUN = sum)
length(sum_squared_diff)
print(tail(sum_squared_diff))

# Remove column 2 from the sum_squared_diff dataframe
sum_squared_diff <- sum_squared_diff[, -2]
length(sum_squared_diff)

# Divide the sum of squared differences by the number of trading days minus 1 to get the variance, do not apply on the Week column keeping this index within teh dataframe
variance <- sum_squared_diff[, -1] / (num_trading_days[, -1] - 1)

# Take the square root of the variance to get the standard deviation
volatility <- sqrt(variance)

# Annualise the volatility by multiplying 100 by sqrt(52)
volatility <- volatility * 100 * sqrt(52)

# Retrieve the weekly dates for use in the zoo object
week_dates <- as.Date(names(num_trading_days$Week), format = "%Y-%U")

# Calculate the first day of each week for indexing
first_day_of_week <- tapply(daily_index, weekly_index, min)

# Convert 'first_day_of_week' to Date class if not already
first_day_of_week <- as.Date(first_day_of_week)

tail(first_day_of_week)
tail(weekly_returns)

# Create an xts object for variance
variance <- xts(variance, order.by = first_day_of_week)
volatility <- xts(volatility, order.by = first_day_of_week)

#---------------------------------------

# Simplified weekly volatility calculation
#---------------------------------------
# Calculate the standard deviation of weekly returns over the five-day interval during each week

# Calculate standard deviation for each column individually using the TTR package
#volatility_list <- lapply(weekly_returns, function(column_data) {
#  runSD(x = column_data, n = 5, sample = TRUE, cumulative = FALSE)
#})

# Convert the list back to an xts object if you want to keep all results together
#volatility_2 <- do.call(merge, volatility_list)

# Convert to a data frame
#vol_2 <- data.frame(Date = index(volatility), Volatility = coredata(volatility))

#last(vol_2, 15)
#last(volatility, 15)
#---------------------------------------

### SUBSET DATA ###
#---------------------------------------

# Trim the data to start when NZU data set begins on 05/01/2010
Research_Data_weekly_returns <- weekly_returns["2018-01-08/2024-08-23"]
Research_Data_weekly_volatility <- volatility["2018-01-08/2024-08-23"]

# No filter
# Research_Data_weekly_returns <- weekly_returns
# Research_Data_weekly_volatility <- volatility

# Replace Volatility column names with the original names
colnames(Research_Data_weekly_volatility) <- colnames(Research_Data_weekly_returns)

# Drop CEA, UKA, WCA
Research_Data_weekly_returns <- Research_Data_weekly_returns[, setdiff(names(Research_Data_weekly_returns), c("CEA", "UKA", "WCA"))]
Research_Data_weekly_volatility <- Research_Data_weekly_volatility[, setdiff(names(Research_Data_weekly_volatility), c("CEA", "UKA", "WCA"))]

# Save the Date indexes for later use
Research_Data_weekly_returns_dates <- index(Research_Data_weekly_returns)
Research_Data_weekly_volatility_dates <- index(Research_Data_weekly_volatility)

# Rename column names for Research_Data_weekly_volatility to match Research_Data_weekly_returns
colnames(Research_Data_weekly_volatility) <- colnames(Research_Data_weekly_returns)

# Weekly Returns date is Wednesday to Wednesday
# Weekly volatility date is Monday to Monday
head(Research_Data_weekly_returns, 5)
head(Research_Data_weekly_volatility, 5)

last(Research_Data_weekly_returns, 5)
last(Research_Data_weekly_volatility, 5)

# Check for any missing values
sum(is.na(Research_Data_weekly_returns))
sum(is.na(Research_Data_weekly_volatility))

# Function to calculate NA counts, non-NA counts, total data points, and first non-NA date with debugging

calculate_na_stats <- function(df) {
  na_counts <- colSums(is.na(df))
  non_na_counts <- colSums(!is.na(df))
  total_counts <- rep(nrow(df), length(na_counts))
  
  first_non_na_date <- sapply(df, function(column) {
    non_na_indices <- which(!is.na(column))
    if (length(non_na_indices) > 0) {
      return(index(df)[non_na_indices[1]])
    } else {
      return(NA)
    }
  })
  
  # Convert numeric date format to original date format
  first_non_na_date <- as.Date(first_non_na_date, origin = "1970-01-01")
  
  # Ensure that all vectors have the same length
  if (length(na_counts) == length(non_na_counts) && length(non_na_counts) == length(total_counts) && length(total_counts) == length(first_non_na_date)) {
    stats_df <- data.frame(
      Column = colnames(df),
      NA_Counts = na_counts,
      Non_NA_Counts = non_na_counts,
      Total_Data_Points = total_counts,
      First_Non_NA_Date = first_non_na_date
    )
  } else {
    stop("Mismatch in the length of vectors being combined into the data frame.")
  }
  
  return(stats_df)
}

# Calculate stats for Research_Data_weekly_returns
returns_stats <- calculate_na_stats(Research_Data_weekly_returns)
print(returns_stats)

# Calculate stats for Research_Data_weekly_volatility
volatility_stats <- calculate_na_stats(Research_Data_weekly_volatility)
print(volatility_stats)

# Function to fill NAs with the column mean but only after the first non-NA value
fill_na_after_first_valid <- function(x) {
  # Find the first non-NA value's index
  first_valid_index <- which(!is.na(x))[1]
  
  if (!is.na(first_valid_index)) {
    # For elements after the first valid index, replace NAs with the mean of non-NA values
    x[(first_valid_index + 1):length(x)] <- sapply(x[(first_valid_index + 1):length(x)], function(y) {
      if (is.na(y)) {
        mean(x[first_valid_index:length(x)], na.rm = TRUE)
      } else {
        y
      }
    })
  }
  
  return(x)
}

# Apply the function to each column in Research_Data_weekly_returns
Research_Data_weekly_returns <- apply(Research_Data_weekly_returns, 2, fill_na_after_first_valid)

# Apply the function to each column in Research_Data_weekly_volatility
Research_Data_weekly_volatility <- apply(Research_Data_weekly_volatility, 2, fill_na_after_first_valid)

# Check for any missing values
sum(is.na(Research_Data_weekly_returns))
sum(is.na(Research_Data_weekly_volatility))

#---------------------------------------

#### Descriptive statistics ####
## From Paper: "The descriptive statistics of the weekly returns and weekly volatility are presented in Table 2 in Panel A and B."
#---------------------------------------
library(moments)  # for skewness and kurtosis
library(ggplot2)  # for plotting

install.packages("forecast")  # Install the forecast package if not already installed
library(forecast) # for ACF calculation and Ljung-Box test

library(stargazer) # for table export

#### Descriptive statistics with ACF p-value calculation ####

# Function to perform the autocorrelation test at lag 1 and return the autocorrelation value
perform_autocorr_test <- function(series) {
  series <- na.omit(series)
  acf_result <- acf(series, plot = FALSE, lag.max = 1)
  return(acf_result$acf[2])  # Return the ACF value at lag 1
}

# Function to perform the Ljung-Box test at lag 1 and return the p-value
perform_autocorr_pvalue <- function(series) {
  series <- na.omit(series)
  
  # Ensure the series has enough data points after removing NAs
  if (length(series) < 10) {
    return(NA)  # Not enough data to perform autocorrelation test
  }
  
  # Perform Ljung-Box test at lag 1
  test_result <- Box.test(series, lag = 1, type = "Ljung-Box")
  
  # Return the p-value of the test
  return(test_result$p.value)
}

# Function to calculate descriptive statistics and autocorrelation p-value
calculate_descriptive_stats <- function(series) {
  mean_val <- mean(series, na.rm = TRUE)
  min_val <- min(series, na.rm = TRUE)
  max_val <- max(series, na.rm = TRUE)
  sd_val <- sd(series, na.rm = TRUE)
  skew_val <- skewness(series, na.rm = TRUE)
  kurt_val <- kurtosis(series, na.rm = TRUE)
  
  # Autocorrelation at lag 1 and its p-value
  autocorr_val <- perform_autocorr_test(series)
  acf_pvalue <- perform_autocorr_pvalue(series)
  
  # Return rounded results, including the autocorrelation p-value
  return(c(mean = round(mean_val, 3), 
           min = round(min_val, 3), 
           max = round(max_val, 3), 
           sd = round(sd_val, 3), 
           skew = round(skew_val, 3), 
           kurt = round(kurt_val, 3), 
           autocorr = round(autocorr_val, 3), 
           p_value = round(acf_pvalue, 3)))
}

# Apply the function to each column of the datasets using apply()
summary_stats_returns <- apply(Research_Data_weekly_returns, 2, calculate_descriptive_stats)
summary_stats_volatility <- apply(Research_Data_weekly_volatility, 2, calculate_descriptive_stats)

# Transpose to match required format
summary_stats_returns <- t(summary_stats_returns)
summary_stats_volatility <- t(summary_stats_volatility)

# Convert to data frames for better formatting and set column names
summary_stats_returns_df <- as.data.frame(summary_stats_returns)
summary_stats_volatility_df <- as.data.frame(summary_stats_volatility)

# Set the column names, including the autocorrelation at lag 1 and its p-value
colnames(summary_stats_returns_df) <- c("Mean", "Min", "Max", "St.dev.", "Skew", "Kurtosis", "Autocorr(1)", "p-value(Autocorr)")
colnames(summary_stats_volatility_df) <- c("Mean", "Min", "Max", "St.dev.", "Skew", "Kurtosis", "Autocorr(1)", "p-value(Autocorr)")

# Print the results to check if formatting is correct
print("Descriptive Statistics for Weekly Returns:")
print(summary_stats_returns_df)

print("Descriptive Statistics for Weekly Volatility:")
print(summary_stats_volatility_df)

# Convert the data frame into the appropriate format for stargazer
## Export the tables to HTML with a note on ACF specification
stargazer(summary_stats_returns_df, 
          type = "html", 
          digits = 3, 
          summary = FALSE,
          align = TRUE,
          title = "Summary Statistics for Returns (with ACF p-values)",
          notes = "ACF is calculated at lag 1 using the Ljung-Box test. The p-value indicates the significance of autocorrelation at lag 1.",
          out = "Summary_Statistics_for_Returns_with_ACF_p_values.html")

stargazer(summary_stats_volatility_df, 
          type = "html", 
          digits = 3, 
          summary = FALSE,
          align = TRUE,
          title = "Summary Statistics for Volatility (with ACF p-values)",
          notes = "ACF is calculated at lag 1 using the Ljung-Box test. The p-value indicates the significance of autocorrelation at lag 1.",
          out = "Summary_Statistics_for_Volatility_with_ACF_p_values.html")

# Export as CSV
write.csv(summary_stats_returns_df, "Summary_Statistics_for_Returns_with_ACF_p_values.csv", row.names = TRUE)
write.csv(summary_stats_volatility_df, "Summary_Statistics_for_Volatility_with_ACF_p_values.csv", row.names = TRUE)

#### ACF Plotting ####
# Function to calculate and plot ACF for each series
calculate_acf <- function(series, series_name, lag_max = 20) {
  # Remove NAs
  series <- na.omit(series)
  
  # Calculate the ACF
  acf_result <- acf(series, plot = FALSE, lag.max = lag_max)
  
  # Convert ACF result to a data frame for plotting
  acf_df <- data.frame(
    Lag = acf_result$lag[-1],  # Lag values (excluding the 0th lag)
    ACF = acf_result$acf[-1]   # ACF values
  )
  
  # Create ACF plot
  p <- ggplot(acf_df, aes(x = Lag, y = ACF)) +
    geom_bar(stat = "identity", fill = "steelblue", width = 0.5) +
    labs(title = paste0("Autocorrelation Function (ACF) for ", series_name),
         x = "Lag", y = "ACF") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 14)
    )
  
  # Save the plot
  ggsave(file.path(getwd(), paste0("ACF_", series_name, ".png")), plot = p, width = 8, height = 6)
  
  return(p)  # Return the plot object for further use if needed
}

## ACF Plotting for each series ##
# Loop through the series in returns and volatility and generate ACF plots
for (colname in colnames(Research_Data_weekly_returns)) {
  calculate_acf(Research_Data_weekly_returns[, colname], series_name = colname)
}

for (colname in colnames(Research_Data_weekly_volatility)) {
  calculate_acf(Research_Data_weekly_volatility[, colname], series_name = colname)
}

#---------------------------------------

### Plot the data ###
## Weekly Returns ##
#---------------------------------------
# Load necessary libraries
# Plot the weekly returns as 4 separate charts for each series but merge them into one file
# Convert xts objects to data frames, capturing Date indices
Research_Data_continuously_compounded_weekly_returns <- data.frame(Date = Research_Data_weekly_returns_dates, coredata(Research_Data_weekly_returns))

# Match Column Names to the paper
colnames(Research_Data_continuously_compounded_weekly_returns) <- c('Date',colnames(Research_Data_weekly_returns))

# Melt the data for plotting (if using ggplot2 and the data is wide)
Research_Data_continuously_compounded_weekly_returns_long <- melt(Research_Data_continuously_compounded_weekly_returns, id.vars = "Date", variable.name = "Series", value.name = "Weekly_Return")

# Create 4 separate plots for each series and save them in the save file as a 2 by 2 grid
# Plot the weekly returns for each series
a <- ggplot(Research_Data_continuously_compounded_weekly_returns_long, aes(x = Date)) +
  geom_line(aes(y = Weekly_Return, color = Series)) +
  labs(title = "Weekly Returns",
       x = "Date",
       y = "Volatility") +
  #scale_color_manual(values = c("EUA" = "blue", "NZU" = "red", "CCA" = "green", "HBEA" = "purple")) +
  facet_wrap(~ Series, scales = "free_y") +  # Create a separate plot for each series
  theme_minimal()


# Save plots together in one file
#ggsave("Weekly_Returns_Plot.png", bg = "white")  

# Convert the ggplot object to a plotly object
plotly::ggplotly(a)

# Seperate plots for each series
# Create 4 separate plots for each series and save them in the same file as a 2 by 2 grid
plot <- ggplot(Research_Data_continuously_compounded_weekly_returns_long, aes(x = Date, y = Weekly_Return)) +
  geom_line(aes(color = Series), size = 0.7) +
  facet_wrap(~ Series, scales = "free", ncol = 2) +  # Create a 2 by 2 grid
  labs(x = "Date", y = "Return (%)") +
  ggtitle("Weekly Returns of Global Carbon Market Network") +  # Add title
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +  # Set x-axis to show yearly ticks
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),  # Remove grid lines
    axis.title = element_text(size = 16),  # Increase size of axis titles
    axis.text = element_text(size = 14),  # Increase size of axis labels
    strip.text = element_text(size = 16, face = "bold", hjust = 0),  # Left-align series labels
    axis.line.x.bottom = element_line(color = "black", size = 0.5),  # Add x-axis line
    axis.line.y.left = element_line(color = "black", size = 0.5),  # Add y-axis line
    axis.ticks = element_line(color = "black", size = 0.5),  # Add axis ticks
    axis.ticks.length = unit(0.2, "cm"),  # Length of the ticks
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5)  # Style the title
  )

# Export the plot to a html file
htmlwidgets::saveWidget(plotly::ggplotly(plot), "Weekly_Returns_Plot.html")

# Export the plot to a png file
ggsave("Weekly_Returns_Plot.png", plot, width = 12, height = 8, units = "in", dpi = 300)

# Histogram of weekly returns

library(reshape2)
library(gridExtra)

# Function to create histogram with normal distribution overlay, matching the style of the line plots
plot_histogram <- function(data, series_name) {
  ggplot(data[data$Series == series_name,], aes(x = Weekly_Return)) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
    stat_function(fun = dnorm, args = list(mean = mean(data[data$Series == series_name, "Weekly_Return"]),
                                           sd = sd(data[data$Series == series_name, "Weekly_Return"])),
                  color = "red", size = 1) +
    labs(title = paste("Histogram of Weekly Returns:", series_name),
         x = "Return (%)", y = "Density") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0),  # Title size and style
      axis.title = element_text(size = 16),  # Axis title size
      axis.text = element_text(size = 14),  # Axis label size
      axis.line.x.bottom = element_line(color = "black", size = 0.5),  # Add x-axis line
      axis.line.y.left = element_line(color = "black", size = 0.5),  # Add y-axis line
      axis.ticks = element_line(color = "black", size = 0.5),  # Add axis ticks
      axis.ticks.length = unit(0.2, "cm")  # Length of the ticks
    )
}

# Create histograms for each series
plot_list <- lapply(unique(Research_Data_continuously_compounded_weekly_returns_long$Series), function(series) {
  plot_histogram(Research_Data_continuously_compounded_weekly_returns_long, series)
})

# Arrange the plots in a 2x2 grid and save the combined plot
combined_histogram_plot <- cowplot::plot_grid(plotlist = plot_list, ncol = 2)

# Save the combined plot as a PNG file
ggsave("Weekly_Returns_Histogram_Plots.png", combined_histogram_plot, width = 12, height = 8, units = "in", dpi = 300, bg = "white")


#---------------------------------------

## Weekly Volatility ##
#---------------------------------------

# Plot the weekly volatility as 4 separate charts for each series but merge them into one file
# Convert xts objects to data frames, capturing Date indices
Research_Data_annualised_weekly_volatility <- data.frame(Date = Research_Data_weekly_volatility_dates, coredata(Research_Data_weekly_volatility))

# Match Column Names to the paper
colnames(Research_Data_annualised_weekly_volatility) <- c('Date',colnames(Research_Data_weekly_volatility))

# Melt the data for plotting (if using ggplot2 and the data is wide)
Research_Data_annualised_weekly_volatility_long <- melt(Research_Data_annualised_weekly_volatility, id.vars = "Date", variable.name = "Series", value.name = "Weekly_Volatility")

# Create 4 separate plots for each series and save them in the save file as a 2 by 2 grid
# Plot the weekly volatility for each series
p <- ggplot(Research_Data_annualised_weekly_volatility_long, aes(x = Date)) +
  geom_line(aes(y = Weekly_Volatility, color = Series)) +
  labs(title = "Weekly Volatility",
       x = "Date",
       y = "Weekly Volatility") +
  #scale_color_manual(values = c("EUR_EUR" = "blue", "NZ_EUR" = "red", "CCA...Front.December...ICE" = "green", "Hubei_EUR" = "purple")) +
  facet_wrap(~ Series, scales = "free_y") +  # Create a separate plot for each series
  theme_minimal()

# Save plots together in one file
#ggsave("Weekly_Volatility_Plot.png", bg = "white")

# Save plots as plotly interactive plots
# Convert the ggplot object to a plotly object
plotly::ggplotly(p)

# Separate plots for each series
# Create 4 separate plots for each series and save them in the same file as a 2 by 2 grid
plot <- ggplot(Research_Data_annualised_weekly_volatility_long, aes(x = Date, y = Weekly_Volatility)) +
  geom_line(aes(color = Series), size = 0.7) +
  facet_wrap(~ Series, scales = "free", ncol = 2) +  # Create a 2 by 2 grid
  labs(x = "Date", y = "Volatility") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +  # Set x-axis to show yearly ticks
  ggtitle("Weekly Volatility of Global Carbon Market Network") +  # Add title
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),  # Remove grid lines
    axis.title = element_text(size = 16),  # Increase size of axis titles
    axis.text = element_text(size = 14),  # Increase size of axis labels
    strip.text = element_text(size = 16, face = "bold", hjust = 0),  # Left-align series labels
    axis.line.x.bottom = element_line(color = "black", size = 0.5),  # Add x-axis line
    axis.line.y.left = element_line(color = "black", size = 0.5),  # Add y-axis line
    axis.ticks = element_line(color = "black", size = 0.5),  # Add axis ticks
    axis.ticks.length = unit(0.2, "cm"),  # Length of the ticks
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5)  # Style the title
  )


# Export the plot to a html file
htmlwidgets::saveWidget(plotly::ggplotly(plot), "Weekly_Volatility_Plot.html")

# Export the plot to a png file
ggsave("Weekly_Volatility_Plot.png", plot, width = 12, height = 8, units = "in", dpi = 300)

## Pearson correlation across all series ##

# Plot heatmap of correlation matrix
# Create a correlation matrix for the weekly returns
correlation_matrix_returns <- cor(Research_Data_weekly_returns, use = "pairwise.complete.obs")
correlation_matrix_volatility <- cor(Research_Data_weekly_volatility, use = "pairwise.complete.obs")

# Convert the correlation matrices to a long format for ggplot2
correlation_matrix_returns_melted <- melt(correlation_matrix_returns)
correlation_matrix_volatility_melted <- melt(correlation_matrix_volatility)

# Define a custom color palette
custom_palette <- colorRampPalette(c("red", "white", "blue"))

# Plot the heatmap for the correlation matrix of returns
ggplot(correlation_matrix_returns_melted, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Pearson Correlation") +  # Simplify the legend title to avoid overlap
  theme_minimal() + 
  theme(axis.text.x = element_text(vjust = 1, hjust = 1, size = 16),  # Align and set size for x-axis text
        axis.text.y = element_text(size = 16),  # Set size for y-axis text
        axis.title.x = element_blank(),  # Remove x-axis title
        axis.title.y = element_blank(),  # Remove y-axis title
        legend.position = "bottom",  # Position the legend at the bottom
        legend.text = element_text(size = 14),  # Set legend text size
        legend.title = element_text(size = 16, margin = margin(b = 10)),  # Increase font size and add margin to the bottom
        legend.key.width = unit(2.5, "cm"),  # Increase the width of the legend keys
        legend.spacing.x = unit(0.5, "cm")) +  # Increase the spacing between legend keys
  coord_fixed(ratio = 1) +
  labs(title = "Correlation Matrix for Weekly Returns") +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5))

# Save the returns heatmap
ggsave("Correlation_Matrix_Weekly_Returns.png", width = 10, height = , dpi = 300, bg = "white")

# Plot the heatmap for the correlation matrix of volatility
ggplot(correlation_matrix_volatility_melted, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Pearson Correlation") +  # Simplify the legend title to avoid overlap
  theme_minimal() + 
  theme(axis.text.x = element_text(vjust = 1, hjust = 1, size = 16),  # Align and set size for x-axis text
        axis.text.y = element_text(size = 16),  # Set size for y-axis text
        axis.title.x = element_blank(),  # Remove x-axis title
        axis.title.y = element_blank(),  # Remove y-axis title
        legend.position = "bottom",  # Position the legend at the bottom
        legend.text = element_text(size = 14),  # Set legend text size
        legend.title = element_text(size = 16, margin = margin(b = 10)),  # Increase font size and add margin to the bottom
        legend.key.width = unit(2.5, "cm"),  # Increase the width of the legend keys
        legend.spacing.x = unit(0.5, "cm")) +  # Increase the spacing between legend keys
  coord_fixed(ratio = 1) +
  labs(title = "Correlation Matrix for Weekly Volatility") +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5))

# Save the volatility heatmap
ggsave("Correlation_Matrix_Weekly_Volatility.png", width = 10, height = 8, dpi = 300, bg = "white")
#---------------------------------------



## Data Export ##
#---------------------------------------

# Trim data to start when NZU data set begins on 05/01/2010
#Research_Data_weekly_returns_dates <- Research_Data_weekly_returns_dates["2010-01-16/"]
#Research_Data_weekly_volatility_dates <- Research_Data_weekly_volatility_dates["2010-01-16/"]

# trim data to start when NZU data set begins on 05/01/2010
# Research_Data_weekly_returns <- Research_Data_weekly_returns["2010-01-16/"]
# Research_Data_weekly_volatility <- Research_Data_weekly_volatility["2010-01-16/"]

# Convert xts object to a data frame
Research_Data_weekly_returns_df <- as.data.frame(Research_Data_weekly_returns)
Research_Data_weekly_volatility_df <- as.data.frame(Research_Data_weekly_volatility)

# Add the index (date) as a column in the data frame
Research_Data_weekly_returns_df$Date <- Research_Data_weekly_returns_dates
Research_Data_weekly_volatility_df$Date <- Research_Data_weekly_volatility_dates

# Move the Date column to the first position
Research_Data_weekly_returns_df <- Research_Data_weekly_returns_df[, c(ncol(Research_Data_weekly_returns_df), 1:(ncol(Research_Data_weekly_returns_df)-1))]
Research_Data_weekly_volatility_df <- Research_Data_weekly_volatility_df[, c(ncol(Research_Data_weekly_volatility_df), 1:(ncol(Research_Data_weekly_volatility_df)-1))]

# Export the data to CSV files
write.csv(Research_Data_weekly_returns_df, "Research_Data_weekly_returns.csv", row.names = FALSE)
write.csv(Research_Data_weekly_volatility_df, "Research_Data_weekly_volatility.csv", row.names = FALSE)
#---------------------------------------

## Comparison to Semester 1 paper

#---------------------------------------

# Ensure the Date column is of Date type
Research_Data_weekly_returns_df$Date <- as.Date(Research_Data_weekly_returns_df$Date)
Research_Data_weekly_volatility_df$Date <- as.Date(Research_Data_weekly_volatility_df$Date)

# Subset the data to between Friday May 2, 2014, to Friday December 3, 2021
Research_Data_weekly_returns_subset <- subset(Research_Data_weekly_returns_df, Date >= as.Date("2014-05-02") & Date <= as.Date("2021-12-03"))
Research_Data_weekly_volatility_subset <- subset(Research_Data_weekly_volatility_df, Date >= as.Date("2014-05-02") & Date <= as.Date("2021-12-03"))

# Turn into xts objects
Research_Data_weekly_returns_subset <- xts(Research_Data_weekly_returns_subset[, -1], order.by = Research_Data_weekly_returns_subset$Date)
Research_Data_weekly_volatility_subset <- xts(Research_Data_weekly_volatility_subset[, -1], order.by = Research_Data_weekly_volatility_subset$Date)

# Run Summary Statistics
summary_stats_returns_subset <- apply(Research_Data_weekly_returns_subset, 2, calculate_descriptive_stats)
summary_stats_volatility_subset <- apply(Research_Data_weekly_volatility_subset, 2, calculate_descriptive_stats)

# Transpose to match required format
summary_stats_returns <- t(summary_stats_returns_subset)
summary_stats_volatility <- t(summary_stats_volatility_subset)

# Convert to data frames for better formatting and set column names
summary_stats_returns_df <- as.data.frame(summary_stats_returns)
summary_stats_volatility_df <- as.data.frame(summary_stats_volatility)

# Set the column names
colnames(summary_stats_returns_df) <- c("Mean", "Min", "Max", "St.dev.", "Skew.", "Kurt.", "ADF")
colnames(summary_stats_volatility_df) <- c("Mean", "Min", "Max", "St.dev.", "Skew.", "Kurt.", "ADF")

# Drop KAU, ACCU, UKA, CEA, WCA
# Define the row names to drop
rows_to_drop <- c("KAU", "ACCU", "UKA", "CEA", "WCA")

# Drop the specified rows
summary_stats_returns_df <- summary_stats_returns_df[!rownames(summary_stats_returns_df) %in% rows_to_drop, ]
summary_stats_volatility_df <- summary_stats_volatility_df[!rownames(summary_stats_volatility_df) %in% rows_to_drop, ]

# Order as EUA, NZU, CCA, HBEA
desired_order <- c("EUA", "NZU", "CCA", "HBEA")

# Reorder the rows based on the desired order
summary_stats_returns_df <- summary_stats_returns_df[desired_order, ]
summary_stats_volatility_df <- summary_stats_volatility_df[desired_order, ]

# Transpose the data frames
summary_stats_returns <- t(t(summary_stats_returns_df))
summary_stats_volatility <- t(t(summary_stats_volatility_df))

## Export the tables to HTML
stargazer(summary_stats_returns, 
          type = "html", 
          digits=3, align=TRUE,
          intercept.bottom=FALSE,
          title = "Summary Statistics for Returns",
          out= "Summary Statistics for Returns Replication Exercise Data.html"
        )

stargazer(summary_stats_volatility, 
          type = "html", 
          digits=3, align=TRUE,
          intercept.bottom=FALSE,
          title = "Summary Statistics for Volatility",
          out= "Summary Statistics for Volatility Replication Exercise Data.html")

# Summary stats are slightly different to the paper, but this is likely due to the different data sources used. Not materially different.