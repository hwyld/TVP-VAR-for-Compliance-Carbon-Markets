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

# Forecast horizon
h = 10

# Window size
window_size = 50

# Initialize the TCI DataFrame
TCI_df <- data.frame(Date = as.Date(character()), TCI = numeric(), stringsAsFactors = FALSE)

# Subset the data for the first window (first 50 periods)
window_data_1 <- return_zoo[1:window_size, ]

# Remove any variables with zero values
window_data_1 <- window_data_1[, colSums(window_data_1) != 0]

# Determine the optimal lag length using VARselect
lag_selection <- VARselect(window_data_1, lag.max = h, type = "const")

# Extract the optimal lag based on the Schwarz Criterion (SC)
optimal_lag <- lag_selection$selection["SC(n)"]

VAR(window_data_1)

dca = ConnectednessApproach(window_data_1,
                            nlag=optimal_lag,
                            nfore=10, 
                            model="VAR",
                            corrected=FALSE,
                            window.size=NULL)

kable(dca$TABLE)

# Store last date from window_data_1 and TCI in TCI_df
TCI_df <- rbind(TCI_df, data.frame(Date = index(window_data_1)[window_size], TCI = dca$TCI))



## Looped version
#----------------------------------

# Load necessary libraries
library(vars)

# Forecast horizon
h <- 10

# Window size
window_size <- 150

# Tiny number to replace zeros
tiny_number <- 1e-10

# Initialize the TCI DataFrame
TCI_df <- data.frame(Date = as.Date(character()), 
                     TCI = numeric(), 
                     Model_Success = character(), 
                     Num_Variables = integer(), 
                     stringsAsFactors = FALSE)

# Ensure the index of the zoo object is unique
return_zoo <- return_zoo[!duplicated(index(return_zoo)), ]

# Start the rolling window process
for (start_index in 1:(nrow(return_zoo) - window_size + 1)) {
    
    # Define the end index for the current window
    end_index <- start_index + window_size - 1
    
    # Subset the data for the current window
    window_data <- return_zoo[start_index:end_index, ]
    
    # Data cleaning steps
    
    # 1. Remove any columns with all zero values
    window_data <- window_data[, colSums(window_data) != 0]
    
    # 2. Remove variables with near-zero variance
    nzv <- apply(window_data, 2, function(x) var(x) < 1e-8)
    window_data <- window_data[, !nzv]
    
    # 3. For columns with > 95% zeros but some non-zero values, replace zeros with tiny number
    zero_pct <- colSums(window_data == 0) / nrow(window_data)
    columns_to_adjust <- which(zero_pct > 0.95 & zero_pct < 1)
    
    # Ensure the subset is done correctly by iterating over each column to be adjusted
    for (col in columns_to_adjust) {
        window_data[, col][window_data[, col] == 0] <- tiny_number
    }
    
    # Record the number of columns (variables) in the cleaned window
    num_variables <- ncol(window_data)
    
    # Determine the optimal lag length using VARselect
    lag_selection <- VARselect(window_data, lag.max = h, type = "const")
    optimal_lag <- lag_selection$selection["SC(n)"]
    
    # Perform the connectedness analysis
    dca <- tryCatch({
        ConnectednessApproach(window_data,
                              nlag = optimal_lag,
                              nfore = h,
                              model = "VAR",
                              corrected = FALSE,
                              window.size = NULL)
    }, error = function(e) {
        cat("Error in ConnectednessApproach:", e$message, "\n")
        return(NULL)
    })
    
    if (!is.null(dca)) {
        # Store the results in TCI_df
        TCI_df <- rbind(TCI_df, data.frame(Date = index(window_data)[window_size], 
                                           TCI = dca$TCI, 
                                           Model_Success = "Y", 
                                           Num_Variables = num_variables))
    } else {
        # Record failure in TCI_df
        TCI_df <- rbind(TCI_df, data.frame(Date = index(window_data)[window_size], 
                                           TCI = NA, 
                                           Model_Success = "N", 
                                           Num_Variables = num_variables))
        cat("Connectedness analysis failed for window starting at index", start_index, ".\n")
    }
}

# Create a new dataframe for the TCI results that contains only Date and TCI
TCI <- TCI_df[!is.na(TCI_df$TCI), c("Date", "TCI")]

# Plot the connectedness measures
ggplot(TCI, aes(x = Date, y = TCI)) +
  geom_line(color = "blue") +
  labs(title = "Total Connectedness Index Over Time",
       x = "Date",
       y = "Total Connectedness Index (TCI)") +
  theme_minimal()

# Save the TCI DataFrame to a CSV file
write.csv(TCI, file = "TCI_results_var.csv", row.names = FALSE)

#----------------------------------

# Plot the connectedness measures
Plot(dca, ylim=c(0,40))

# Manual Approach
fit = VAR(window_data_1, configuration=list(nlag=optimal_lag))
# Error variance-covariance
kable(fit$Q)

dca = TimeConnectedness(fit$B, fit$Q, nfore=10)
kable(dca$TABLE)

# Extract the TCI values
TCI_values <- dca$TABLE$TCI


# Extract the connectedness measures
str(dca)
TCI_return <- as.data.frame(dca$TCI)
TCI_return$Date <- as.Date(row.names(TCI_return))
to_return <- as.data.frame(dca$TO)
from_return  <- as.data.frame(dca$FROM)
NET_return <- as.data.frame(dca$NET)


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
                            window.size=200)

kable(dca$TABLE)

data(dy2012)
dca = ConnectednessApproach(dy2012,
                            nlag=4,
                            nfore=10,
                            window.size=200)





## Using vars package , dy2012 is a dataset in vars package ##
#----------------------------------
NAMES = colnames(dy2012)
nlag = 4
k = ncol(dy2012)
fit = vars::VAR(dy2012, p=nlag)
B = t(matrix(unlist(coefficients(fit)), ncol=4))[,1:(k*nlag)]
Q = summary(fit)$covres
colnames(Q) = rownames(Q) = NAMES
kable(Q)

dca = TimeConnectedness(B, Q, nfore=10)
kable(dca$TABLE)
#----------------------------------
