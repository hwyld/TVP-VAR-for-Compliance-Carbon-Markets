### Econometrics Research Project , Semester 2, 2024
## The pursuit of a global carbon market; A time varying analysis of international compliance carbon markets
## TVP-VAR model estimation procedures in R using the ConnectednessApproach package
## Author: Henry Wyld
## Date of creation: 2024-03-20

## References
## https://sites.google.com/view/davidgabauer/econometric-code?authuser=0
## https://sites.google.com/site/fk83research/code?authuser=0

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
#----------------------------------

## Import data ##
#----------------------------------
return_df <- read.csv("Research_Data_weekly_returns.csv")
vol_df <- read.csv("Research_Data_weekly_volatility.csv")

# Convert the data to zoo objects
return_zoo <- zoo(return_df[, -1], order.by = as.Date(return_df$Date))
vol_zoo <- zoo(vol_df[, -1], order.by = as.Date(vol_df$Date))
#----------------------------------

## Data Cleaning ##
#----------------------------------

# If there are any NAs or infinite values,  removing or imputing them
#return_zoo <- na.omit(return_zoo)  # Removes entire rows where any NA values are present

# Adapt any remaining NAs or infinite values to 0 in the data
return_zoo <- na.fill(return_zoo, fill = 0)

# Replace infinite values with 0
return_zoo[is.infinite(return_zoo)] <- 0

## Ensure there are no NAs or infinite values ##
summary(return_zoo)
any(is.na(return_zoo))
any(is.infinite(return_zoo))

#----------------------------------

## VAR model estimation with rolling windows ##

#----------------------------------
# Load the necessary library
library(vars)

# Initialize the TCI DataFrame
TCI_df <- data.frame(Date = as.Date(character()), TCI = numeric(), stringsAsFactors = FALSE)

# Subset the data for the first window (first 50 periods)
window_data_1 <- return_zoo[1:50, ]

# Determine the optimal lag length using VARselect
lag_selection <- VARselect(window_data_1, lag.max = 10, type = "const")

VARselect(window_data_1, lag.max = 12, type = "const")

optimal_lag <- lag_selection$selection["SC(n)"]

# Estimate the VAR model using the optimal lag
var_model_1 <- tryCatch(VAR(window_data_1, p = optimal_lag, type = "const"),
                        error = function(e) return(NULL))
# Check if model was successfully estimated
if (!is.null(var_model_1) && inherits(var_model_1, "varest")) {
    # Calculate the FEVD with a 10-step-ahead forecast
    fevd_result_1 <- fevd(var_model_1, n.ahead = 10)
    
    # Extract the FEVD results
    fevd_values <- as.data.frame(fevd_result_1)
    
    # Calculate the Total Connectedness Index (TCI)
    # In this case, TCI could be defined as the sum of off-diagonal elements of the FEVD matrix
    # This is a basic proxy for connectedness
    tci_1 <- sum(fevd_values) - sum(diag(fevd_values))
    
    # Append the result to the TCI DataFrame
    TCI_df <- rbind(TCI_df, data.frame(Date = index(window_data_1)[50], TCI = tci_1))
    
    # Print the FEVD result
    print(fevd_values)
} else {
    cat("VAR model fitting failed for the first window.\n")
}

# Save the TCI DataFrame to a CSV file
write.csv(TCI_df, file = "TCI_results_var.csv", row.names = FALSE)
#----------------------------------



## TVP-VAR model estimation with rolling windows ##

#----------------------------------

# Load necessary libraries
# Install tvReg package if not already installed
if (!requireNamespace("tvReg", quietly = TRUE)) {
  install.packages("tvReg")
}

# Load necessary libraries
library(tvReg)
library(vars)
library(ConnectednessApproach)

# Initialize the TCI DataFrame
TCI_df <- data.frame(Date = as.Date(character()), TCI = numeric(), stringsAsFactors = FALSE)

# Define the initial window size
initial_window_size <- 50

# Iterate over the data, increasing the number of variables as new markets enter
for (i in seq(initial_window_size, nrow(return_zoo))) {
  
  # Use all available data up to the current period
  current_data <- return_zoo[1:i, ]
  
  # Estimate the TV-VAR model
  tv_var_model <- tvVAR(current_data, p = 1)
  
  # Perform 10-step-ahead forecasts
  fevd_result <- fevd(tv_var_model, n.ahead = 10)
  
  # Calculate the Total Connectedness Index (TCI)
  TCI_value <- ConnectednessApproach::totalConnectedness(fevd_result)
  
  # Append the results to the TCI DataFrame
  TCI_df <- rbind(TCI_df, data.frame(Date = index(current_data)[i], TCI = TCI_value))
}

# Save the TCI DataFrame
write.csv(TCI_df, file = "TCI_results_tvvar.csv", row.names = FALSE)


library(vars)
data(Canada)
#COnvert to zoo object
Canada <- zoo(Canada, order.by = time(Canada))
VAR(Canada)
var1 <- VAR(window_data_1)

# Show the estimated VAR model
var1

VARselect(window_data_1, lag.max = 12, type = "const")
var1 <- VAR(window_data_1, p = 1, type = "const",season = NULL, 
    exog = NULL)

summary(var1)

VAR(Canada, p = 2, type = "const")
VAR(Canada, p = 2, type = "trend")
VAR(Canada, p = 2, type = "both")

fit = VAR(window_data_1, configuration=list(nlag=1))

# Calculate the Forecast Error Variance Decomposition (FEVD)
fevd_result <- fevd(fit, n.ahead = 10)

kable(fit$Q)

# Extract the coefficient matrices and the covariance matrix
Phi <- fit$varresult
Sigma <- fit$covres

# Calculate the connectedness measures

dca = TimeConnectedness(fit$B, fit$Q, nfore=10, generalized=TRUE)
kable(dca$TABLE)

dca = ConnectednessApproach(return_zoo,
                            nlag=1,
                            nfore=10,
                            window.size=100)

kable(dca$TABLE)

data(dy2012)
dca = ConnectednessApproach(dy2012,
                            nlag=4,
                            nfore=10,
                            window.size=200)