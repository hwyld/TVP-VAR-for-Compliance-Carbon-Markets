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

## Initial Data Cleaning ##

#------------------------------------------------------------------------------------------------
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
#------------------------------------------------------------------------------------------------

## VAR model estimation with rolling windows ## TEST 

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


## LOOPED VERSION OF STATIC VAR MODEL ESTIMATION WITH ROLLING WINDOWS ##
#----------------------------------
# Load necessary libraries
library(vars)

# Forecast horizon
h <- 10

# Window size
window_size <- 200

# Tiny number to replace zeros
tiny_number <- 1e-10

# Initialize the TCI DataFrame
TCI_df <- data.frame(Date = as.Date(character()), 
                     TCI = numeric(), 
                     Model_Success = character(), 
                     Num_Variables = integer(), 
                     stringsAsFactors = FALSE)

# Initialize a list to store window_data for failed models
failed_windows <- list()

# Initialize a DataFrame to store failure reasons
failure_log <- data.frame(Date = as.Date(character()), 
                          Reason = character(), 
                          stringsAsFactors = FALSE)

# Initialize a DataFrame to store model specifications
model_specs <- data.frame(Date = as.Date(character()), 
                          Optimal_Lag = integer(), 
                          stringsAsFactors = FALSE)

# Ensure the index of the zoo object is unique
return_zoo <- return_zoo[!duplicated(index(return_zoo)), ]

# Start the rolling window process
for (start_index in 1:(nrow(return_zoo) - window_size + 1)) {
    
    # Define the end index for the current window
    end_index <- start_index + window_size - 1
    
    # Subset the data for the current window
    window_data <- return_zoo[start_index:end_index, ]
    
    # Extract the last date of this window for logging purposes
    last_date <- index(window_data)[window_size]
    
    # Data cleaning steps
    
    # 1. Remove any columns with all zero values
    window_data <- window_data[, colSums(window_data) != 0]
    
    # 2. Remove variables with near-zero variance
    nzv <- apply(window_data, 2, function(x) var(x) < 1e-8)
    window_data <- window_data[, !nzv]
    
    # 3. For columns with > 95% zeros but some non-zero values, replace zeros with tiny number
    zero_pct <- colSums(window_data == 0) / nrow(window_data)
    columns_to_adjust <- which(zero_pct > 0.95 & zero_pct < 1)
    
    # Replace zeros with the tiny number in the identified columns
    for (col in columns_to_adjust) {
        window_data[, col][window_data[, col] == 0] <- tiny_number
    }
    
    # Record the number of columns (variables) in the cleaned window
    num_variables <- ncol(window_data)
    
    # Determine the optimal lag length using VARselect
    lag_selection <- tryCatch({
        VARselect(window_data, lag.max = h, type = "const")
    }, error = function(e) {
        # Capture the error and log the failed window and reason
        failed_windows[[length(failed_windows) + 1]] <- list(data = window_data, reason = "VARselect failed")
        failure_log <- rbind(failure_log, 
                             data.frame(Date = last_date, 
                                        Reason = "VARselect failed"))
        return(NULL)
    })
    
    if (!is.null(lag_selection)) {
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
            # Capture the error and log the failed window and reason
            failed_windows[[length(failed_windows) + 1]] <- list(data = window_data, reason = e$message)
            failure_log <- rbind(failure_log, 
                                 data.frame(Date = last_date, 
                                            Reason = e$message))
            return(NULL)
        })
        
        if (!is.null(dca)) {
            # Store the results in TCI_df
            TCI_df <- rbind(TCI_df, data.frame(Date = last_date, 
                                               TCI = dca$TCI, 
                                               Model_Success = "Y", 
                                               Num_Variables = num_variables))
            # Record model specifications
            model_specs <- rbind(model_specs, 
                                 data.frame(Date = last_date, 
                                            Optimal_Lag = optimal_lag))
        } else {
            # Record failure in TCI_df
            TCI_df <- rbind(TCI_df, data.frame(Date = last_date, 
                                               TCI = NA, 
                                               Model_Success = "N", 
                                               Num_Variables = num_variables))
        }
    } else {
        # Record failure in TCI_df if VARselect fails
        TCI_df <- rbind(TCI_df, data.frame(Date = last_date, 
                                           TCI = NA, 
                                           Model_Success = "N", 
                                           Num_Variables = num_variables))
    }
}

# Count the number of successful models
num_successful_models <- sum(TCI_df$Model_Success == "Y")

# As a proportion of the total number of models
prop_successful_models <- num_successful_models / nrow(TCI_df)



write.csv(failure_log, file = "failure_log.csv", row.names = FALSE)
write.csv(model_specs, file = "model_specs.csv", row.names = FALSE)

# Adapt TCI_df to TCI, dropping last 2 columns
TCI <- TCI_df[, -c(3, 4)]

# Plot the connectedness measures
ggplot(TCI, aes(x = Date, y = TCI)) +
  geom_line(color = "blue") +
  labs(title = "Total Connectedness Index Over Time",
       x = "Date",
       y = "Total Connectedness Index (TCI)") +
  theme_minimal()

#----------------------------------



## Looped version with a break for errors
#----------------------------------

# Load necessary libraries
library(vars)

# Forecast horizon
h <- 10

# Window size
window_size <- 200

# Tiny number to replace zeros
tiny_number <- 1e-10

# Initialize the TCI DataFrame
TCI_df <- data.frame(Date = as.Date(character()), 
                     TCI = numeric(), 
                     Model_Success = character(), 
                     Num_Variables = integer(), 
                     stringsAsFactors = FALSE)

# Initialize a list to store window_data for failed models
failed_windows <- list()

# Initialize a DataFrame to store failure reasons
failure_log <- data.frame(Date = as.Date(character()), 
                          Reason = character(), 
                          stringsAsFactors = FALSE)

# Initialize a DataFrame to store model specifications
model_specs <- data.frame(Date = as.Date(character()), 
                          Optimal_Lag = integer(), 
                          stringsAsFactors = FALSE)

# Ensure the index of the zoo object is unique
return_zoo <- return_zoo[!duplicated(index(return_zoo)), ]

# Define the stop date
stop_date <- as.Date("2023-12-31")

# Start the rolling window process
for (start_index in 1:(nrow(return_zoo) - window_size + 1)) {
    
    # Define the end index for the current window
    end_index <- start_index + window_size - 1
    
    # Subset the data for the current window
    window_data <- return_zoo[start_index:end_index, ]
    
    # Extract the last date of this window for logging purposes
    last_date <- index(window_data)[window_size]
    
    # Stop the loop if the last date of the current window exceeds the stop date
    if (last_date >= stop_date) {
        cat("Stopping at date:", last_date, "\n")
        break
    }
    
    # Data cleaning steps
    
    # 1. Remove any columns with all zero values
    window_data <- window_data[, colSums(window_data) != 0]
    
    # 2. Remove variables with near-zero variance
    nzv <- apply(window_data, 2, function(x) var(x) < 1e-8)
    window_data <- window_data[, !nzv]
    
    # 3. For columns with > 95% zeros but some non-zero values, replace zeros with tiny number
    zero_pct <- colSums(window_data == 0) / nrow(window_data)
    columns_to_adjust <- which(zero_pct > 0.95 & zero_pct < 1)
    
    # Replace zeros with the tiny number in the identified columns
    for (col in columns_to_adjust) {
        window_data[, col][window_data[, col] == 0] <- tiny_number
    }
    
    # Record the number of columns (variables) in the cleaned window
    num_variables <- ncol(window_data)
    
    # Determine the optimal lag length using VARselect
    lag_selection <- tryCatch({
        VARselect(window_data, lag.max = h, type = "const")
    }, error = function(e) {
        # Capture the error and log the failed window and reason
        failed_windows[[length(failed_windows) + 1]] <- list(data = window_data, reason = "VARselect failed")
        failure_log <- rbind(failure_log, 
                             data.frame(Date = last_date, 
                                        Reason = "VARselect failed"))
        cat("Error in VARselect. Stopping at date:", last_date, "\n")
        break
    })
    
    if (!is.null(lag_selection)) {
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
            # Capture the error and log the failed window and reason
            failed_windows[[length(failed_windows) + 1]] <- list(data = window_data, reason = e$message)
            failure_log <- rbind(failure_log, 
                                 data.frame(Date = last_date, 
                                            Reason = e$message))
            cat("Error in ConnectednessApproach. Stopping at date:", last_date, "\n")
            break
        })
        
        if (!is.null(dca)) {
            # Store the results in TCI_df
            TCI_df <- rbind(TCI_df, data.frame(Date = last_date, 
                                               TCI = dca$TCI, 
                                               Model_Success = "Y", 
                                               Num_Variables = num_variables))
            # Record model specifications
            model_specs <- rbind(model_specs, 
                                 data.frame(Date = last_date, 
                                            Optimal_Lag = optimal_lag))
        } else {
            # Record failure in TCI_df
            TCI_df <- rbind(TCI_df, data.frame(Date = last_date, 
                                               TCI = NA, 
                                               Model_Success = "N", 
                                               Num_Variables = num_variables))
        }
    } else {
        # Record failure in TCI_df if VARselect fails
        TCI_df <- rbind(TCI_df, data.frame(Date = last_date, 
                                           TCI = NA, 
                                           Model_Success = "N", 
                                           Num_Variables = num_variables))
    }
}

# Inspect the window_data where the first error occurred
if (length(failed_windows) > 0) {
    first_failed_window <- failed_windows[[1]]$data
    print("Inspecting the first failed window_data:")
    print(first_failed_window)
}

window_data_20140411 <- window_data

VARselect(window_data, lag.max = h, type = "const")
optimal_lag <- lag_selection$selection["SC(n)"]

        dca <- ConnectednessApproach(window_data,
                                  nlag = optimal_lag,
                                  nfore = h,
                                  model = "VAR",
                                  corrected = FALSE,
                                  window.size = NULL)

# Count the number of successful models
num_successful_models <- sum(TCI_df$Model_Success == "Y")

# As a proportion of the total number of models
prop_successful_models <- num_successful_models / nrow(TCI_df)



write.csv(failure_log, file = "failure_log.csv", row.names = FALSE)
write.csv(model_specs, file = "model_specs.csv", row.names = FALSE)

# Adapt TCI_df to TCI, dropping last 2 columns
TCI <- TCI_df[, -c(3, 4)]

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




## TVP-VAR setting of looped model - to account for the lack of back history with models
#----------------------------------

# Load necessary libraries
library(vars)

# Forecast horizon
h <- 10

# This study considers forgetting factor, kappa1=0.99 and decay factor kappa2=0.96 
# follows Antonakakis et al. to keep the decay factors constant at fixed values.
# See https://gabauerdavid.github.io/ConnectednessApproach/2020AntonakakisChatziantoniouGabauer
forgetting_factor <- 0.99
decay_factor <- 0.96

# Window size
window_size <- 200

# Tiny number to replace zeros
tiny_number <- 1e-10

# Initialize the TCI DataFrame
TCI_df <- data.frame(Date = as.Date(character()), 
                     TCI = numeric(), 
                     Model_Success = character(), 
                     Num_Variables = integer(), 
                     stringsAsFactors = FALSE)

# Initialize a list to store window_data for failed models
failed_windows <- list()

# Initialize a DataFrame to store failure reasons
failure_log <- data.frame(Date = as.Date(character()), 
                          Reason = character(), 
                          stringsAsFactors = FALSE)

# Initialize a DataFrame to store model specifications
model_specs <- data.frame(Date = as.Date(character()), 
                          Optimal_Lag = integer(), 
                          stringsAsFactors = FALSE)

# Ensure the index of the zoo object is unique
return_zoo <- return_zoo[!duplicated(index(return_zoo)), ]

# Define the stop date
stop_date <- as.Date("2024-12-31")

# Start the rolling window process
for (start_index in 1:(nrow(return_zoo) - window_size + 1)) {
    
    # Define the end index for the current window
    end_index <- start_index + window_size - 1
    
    # Subset the data for the current window
    window_data <- return_zoo[start_index:end_index, ]
    
    # Extract the last date of this window for logging purposes
    last_date <- index(window_data)[window_size]
    
    # Stop the loop if the last date of the current window exceeds the stop date
    if (last_date >= stop_date) {
        cat("Stopping at date:", last_date, "\n")
        break
    }
    
    # Data cleaning steps
    
    # 1. Remove any columns with all zero values
    window_data <- window_data[, colSums(window_data) != 0]
    
    # 2. Remove variables with near-zero variance
    nzv <- apply(window_data, 2, function(x) var(x) < 1e-8)
    window_data <- window_data[, !nzv]
    
    # 3. For columns with > 95% zeros but some non-zero values, replace zeros with tiny number
    zero_pct <- colSums(window_data == 0) / nrow(window_data)
    columns_to_adjust <- which(zero_pct > 0.95 & zero_pct < 1)
    
    # Replace zeros with the tiny number in the identified columns
    for (col in columns_to_adjust) {
        window_data[, col][window_data[, col] == 0] <- tiny_number
    }
    
    # Record the number of columns (variables) in the cleaned window
    num_variables <- ncol(window_data)
    
    # Determine the optimal lag length using VARselect
    lag_selection <- tryCatch({
        VARselect(window_data, lag.max = h, type = "const")
    }, error = function(e) {
        # Capture the error and log the failed window and reason
        failed_windows[[length(failed_windows) + 1]] <- list(data = window_data, reason = "VARselect failed")
        failure_log <- rbind(failure_log, 
                             data.frame(Date = last_date, 
                                        Reason = "VARselect failed"))
        cat("Error in VARselect. Stopping at date:", last_date, "\n")
        break
    })
    
    if (!is.null(lag_selection)) {
        optimal_lag <- lag_selection$selection["SC(n)"]
        
        # Perform the connectedness analysis
        dca <- tryCatch({
            ConnectednessApproach(window_data,
                                  nlag = optimal_lag,
                                  nfore = h,
                                  model = "TVP-VAR",
                                  connectedness="Time",
                                  VAR_config=list(TVPVAR=list(kappa1=forgetting_factor, kappa2=decay_factor, prior="BayesPrior"))) # TVP-VAR model with forgetting factor and decay factor as specified
        }, error = function(e) {
            # Capture the error and log the failed window and reason
            failed_windows[[length(failed_windows) + 1]] <- list(data = window_data, reason = e$message)
            failure_log <- rbind(failure_log, 
                                 data.frame(Date = last_date, 
                                            Reason = e$message))
            cat("Error in ConnectednessApproach. Stopping at date:", last_date, "\n")
            break
        })
        
        if (!is.null(dca)) {
            # Store the results in TCI_df
            TCI_df <- rbind(TCI_df, data.frame(Date = last_date, 
                                               TCI = dca$TCI, 
                                               Model_Success = "Y", 
                                               Num_Variables = num_variables))
            # Record model specifications
            model_specs <- rbind(model_specs, 
                                 data.frame(Date = last_date, 
                                            Optimal_Lag = optimal_lag))
        } else {
            # Record failure in TCI_df
            TCI_df <- rbind(TCI_df, data.frame(Date = last_date, 
                                               TCI = NA, 
                                               Model_Success = "N", 
                                               Num_Variables = num_variables))
        }
    } else {
        # Record failure in TCI_df if VARselect fails
        TCI_df <- rbind(TCI_df, data.frame(Date = last_date, 
                                           TCI = NA, 
                                           Model_Success = "N", 
                                           Num_Variables = num_variables))
    }
}

# Inspect the window_data where the first error occurred
if (length(failed_windows) > 0) {
    first_failed_window <- failed_windows[[1]]$data
    print("Inspecting the first failed window_data:")
    print(first_failed_window)
}

window_data_20140411 <- window_data

VARselect(window_data, lag.max = h, type = "const")
optimal_lag <- lag_selection$selection["SC(n)"]

dca <-  ConnectednessApproach(window_data,
                                  nlag = optimal_lag,
                                  nfore = h,
                                  model = "TVP-VAR",
                                  connectedness="Time",
                                  VAR_config=list(TVPVAR=list(kappa1=forgetting_factor, kappa2=decay_factor, prior="BayesPrior"))) # TVP-VAR model with forgetting factor and decay factor as specified

kable(dca$TABLE)

# Count the number of successful models
num_successful_models <- sum(TCI_df$Model_Success == "Y")

# As a proportion of the total number of models
prop_successful_models <- num_successful_models / nrow(TCI_df)

write.csv(failure_log, file = "failure_log.csv", row.names = FALSE)
write.csv(model_specs, file = "model_specs.csv", row.names = FALSE)

# Adapt TCI_df to TCI, dropping last 2 columns
TCI <- TCI_df[, -c(3, 4)]

# Plot the connectedness measures
ggplot(TCI, aes(x = Date, y = TCI)) +
  geom_line(color = "blue") +
  labs(title = "Total Connectedness Index Over Time",
       x = "Date",
       y = "Total Connectedness Index (TCI)") +
  theme_minimal()

# Save the TCI DataFrame to a CSV file
write.csv(TCI, file = "TCI_results_var.csv", row.names = FALSE)

### Optimised version of the TVP-VAR model estimation with rolling windows ###
#----------------------------------

# Load necessary libraries
library(vars)
library(doParallel)
library(foreach)

# Set up parallel backend to use multiple processors
cl <- makeCluster(detectCores() - 1)  # Use all but one core
registerDoParallel(cl)

# Forecast horizon
h <- 10

# Forgetting and decay factors
forgetting_factor <- 0.99
decay_factor <- 0.96

# Window size
window_size <- 200

# Tiny number to replace zeros
tiny_number <- 1e-10

# Initialize storage for results
TCI_list <- vector("list", length = nrow(return_zoo) - window_size + 1)
model_specs_list <- vector("list", length = nrow(return_zoo) - window_size + 1)
failure_log_list <- vector("list", length = nrow(return_zoo) - window_size + 1)

# Ensure the index of the zoo object is unique
return_zoo <- return_zoo[!duplicated(index(return_zoo)), ]

# Define the stop date
stop_date <- as.Date("2023-12-31")

# Start the parallel rolling window process
results <- foreach(start_index = 1:(nrow(return_zoo) - window_size + 1), 
                   .packages = c("vars", "ConnectednessApproach")) %dopar% {
    
    # Define the end index for the current window
    end_index <- start_index + window_size - 1
    
    # Subset the data for the current window
    window_data <- return_zoo[start_index:end_index, ]
    
    # Extract the last date of this window for logging purposes
    last_date <- index(window_data)[window_size]
    
    # Stop the loop if the last date of the current window exceeds the stop date
    if (last_date >= stop_date) {
        return(list(TCI = NULL, model_spec = NULL, failure = "Stop date reached"))
    }
    
    # Data cleaning steps
    window_data <- window_data[, colSums(window_data) != 0]  # Remove all-zero columns
    nzv <- apply(window_data, 2, function(x) var(x, na.rm = TRUE) < 1e-8)
    window_data <- window_data[, !nzv]  # Remove near-zero variance columns
    zero_pct <- colSums(window_data == 0) / nrow(window_data)
    columns_to_adjust <- which(zero_pct > 0.95 & zero_pct < 1)
    window_data[, columns_to_adjust] <- apply(window_data[, columns_to_adjust], 2, function(x) ifelse(x == 0, tiny_number, x))

    # Record the number of columns (variables) in the cleaned window
    num_variables <- ncol(window_data)
    
    # Determine the optimal lag length using VARselect
    lag_selection <- tryCatch({
        VARselect(window_data, lag.max = h, type = "const")
    }, error = function(e) {
        return(list(TCI = NULL, model_spec = NULL, failure = paste("VARselect failed:", e$message)))
    })
    
    if (is.null(lag_selection)) {
        return(list(TCI = NULL, model_spec = NULL, failure = "VARselect failed"))
    }
    
    optimal_lag <- lag_selection$selection["SC(n)"]
    
    # Perform the connectedness analysis using TVP-VAR
    dca <- tryCatch({
        ConnectednessApproach(window_data,
                              nlag = optimal_lag,
                              nfore = h,
                              model = "TVP-VAR",
                              connectedness="Time",
                              VAR_config = list(TVPVAR = list(kappa1 = forgetting_factor, kappa2 = decay_factor, prior = "BayesPrior")))
    }, error = function(e) {
        return(list(TCI = NULL, model_spec = NULL, failure = paste("ConnectednessApproach failed:", e$message)))
    })
    
    if (is.null(dca)) {
        return(list(TCI = NULL, model_spec = NULL, failure = "ConnectednessApproach failed"))
    }
    
    # Store results
    list(
        TCI = data.frame(Date = last_date, TCI = dca$TCI, Model_Success = "Y", Num_Variables = num_variables),
        model_spec = data.frame(Date = last_date, Optimal_Lag = optimal_lag),
        failure = NULL
    )
}

# Stop the cluster
stopCluster(cl)

# Process results
for (i in 1:length(results)) {
    if (!is.null(results[[i]]$failure)) {
        failure_log_list[[i]] <- data.frame(Date = index(return_zoo)[window_size], Reason = results[[i]]$failure)
    } else {
        TCI_list[[i]] <- results[[i]]$TCI
        model_specs_list[[i]] <- results[[i]]$model_spec
    }
}

# Combine results
TCI_df <- do.call(rbind, TCI_list)
model_specs <- do.call(rbind, model_specs_list)
failure_log <- do.call(rbind, failure_log_list)

# Plot the connectedness measures
library(ggplot2)
ggplot(TCI_df, aes(x = Date, y = TCI)) +
  geom_line(color = "blue") +
  labs(title = "Total Connectedness Index Over Time",
       x = "Date",
       y = "Total Connectedness Index (TCI)") +
  theme_minimal()

# Save results
write.csv(TCI_df, file = "TCI_results_var.csv", row.names = FALSE)
write.csv(model_specs, file = "model_specs.csv", row.names = FALSE)
write.csv(failure_log, file = "failure_log.csv", row.names = FALSE)
