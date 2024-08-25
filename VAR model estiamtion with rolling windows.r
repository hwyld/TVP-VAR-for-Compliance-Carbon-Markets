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

# Define the start date
start_date <- as.Date("2010-01-10")

# Trim data to start from the specified start date to form a 2 variable system
return_zoo <- window(return_zoo, start = start_date)
vol_zoo <- window(vol_zoo, start = start_date)

# Drop WCA from the return zoo object
return_zoo <- return_zoo[, -which(colnames(return_zoo) == "WCA")]
vol_zoo <- vol_zoo[, -which(colnames(vol_zoo) == "WCA")]

# Adapt any remaining NAs or infinite values to 0 in the data
#return_zoo <- na.fill(return_zoo, fill = 0)

# Replace infinite values with 0
#return_zoo[is.infinite(return_zoo)] <- 0

## Ensure there are no NAs or infinite values ##
summary(return_zoo)
any(is.na(return_zoo))
any(is.infinite(return_zoo))
length(return_zoo)
length_without_na <- sum(!is.na(return_zoo$WCA))

# Total seris for TVP-VAR model
# Drop CEA and UKA
return_zoo_Full <- return_zoo[, -which(colnames(return_zoo) %in% c("CEA", "UKA"))]

# If there are any NAs or infinite values, removing or imputing them
return_zoo_Full <- na.omit(return_zoo_Full)  # Removes entire rows where any NA values are present

length(index(return_zoo_Full))

#------------------------------------------------------------------------------------------------

## VAR model estimation with rolling windows ## TEST 

#### Using the ConnectednessApproach package ####

# #----------------------------------
# # Load the necessary library
# library(vars)

# # Forecast horizon
# h = 10

# # Window size
# window_size = 100

# # Initialize the TCI DataFrame
# TCI_df <- data.frame(Date = as.Date(character()), TCI = numeric(), stringsAsFactors = FALSE)

# ## Create data for the first window
# # Subset the data for the first window (first 50 periods)
# window_data_1 <- return_zoo[1:window_size, ]

# ## Clean window_data_1
# # Remove any variables with zero values
# window_data_1 <- window_data_1[, colSums(window_data_1) != 0]

# # Only keep variables that have complete data for the entire window
# window_data_1 <- window_data_1[, colSums(is.na(window_data_1)) == 0]
    
# # If no variables are left after filtering, skip to the next window
# if (ncol(window_data_1) == 0) next


# ## Method 1 - Using the vars package for model estimation and manually calculating TCI ##
# # Determine the optimal lag length using VARselect
# lag_selection <- VARselect(window_data_1, lag.max = h)

# # Extract the optimal lag based on the Schwarz Criterion (SC)
# optimal_lag <- lag_selection$selection["SC(n)"]

# # test
# var_model_window_1 <- vars::VAR(window_data_1, p = optimal_lag)

# # Compute the FEVD
# fevd_result_window_1 <- fevd(var_model_window_1, n.ahead = h)

# # Initialize variables for calculations
# num_variables <- ncol(window_data_1)
# spillover_matrix <- matrix(0, num_variables, num_variables)

# # Calculate the spillover matrix
# for (i in 1:num_variables) {
#   for (j in 1:num_variables) {
#     spillover_matrix[i, j] <- sum(fevd_result_window_1[[i]][, j])
#   }
# }

# # Normalize the spillover matrix (ensure each row sums to 1)
# spillover_matrix <- sweep(spillover_matrix, 1, rowSums(spillover_matrix), "/")

# # Calculate the Total Connectedness Index (TCI)
# off_diag_sum_window_1 <- sum(spillover_matrix) - sum(diag(spillover_matrix))
# total_variance <- sum(spillover_matrix)
# tci_window_1 <- (off_diag_sum_window_1 / total_variance) * 100

# # Output the Spillover Index
# tci_window_1

# ## Method 2 - Using the vars package for model estimation and the ConnectednessApporach for calculating TCI ##
# # From https://gabauerdavid.github.io/ConnectednessApproach/Rpackage #

# NAMES = colnames(window_data_1)
# nlag = optimal_lag
# k = ncol(window_data_1)
# fit = vars::VAR(window_data_1, p=nlag)
# B = t(matrix(unlist(coefficients(fit)), ncol=4))[,1:(k*nlag)]
# Q = summary(fit)$covres
# colnames(Q) = rownames(Q) = NAMES
# kable(Q)

# dca = TimeConnectedness(B, Q, nfore=h)
# kable(dca$TABLE)

# # Save residual tests
# residuals = residuals(fit)


# ## Method 3 - Using the vars package for model estimation and the ConnectednessApporach for calculating TCI ##
# # From https://gabauerdavid.github.io/ConnectednessApproach/Rpackage #
# # Connectedness Approach
# dca1 = ConnectednessApproach(window_data_1,
#                             nlag=optimal_lag,
#                             nfore=h, 
#                             model="VAR",
#                             corrected=TRUE,
#                             window.size=NULL)

# kable(dca1$TABLE)

# # Store last date from window_data_1 and TCI in TCI_df
# TCI_df <- rbind(TCI_df, data.frame(Date = index(window_data_1)[window_size], TCI = dca$TCI))


# DY Approach to VAR estimation using fixed rolling windows

### 3 options here 
## 1) using the vars package for model estimation and manually calculating TCI , allow for residuals testing ##
## 2) using the vars package for model estimation and the ConnectednessApporach for calculating TCI, allow for residuals testing ##
## 3) using the vars package for model estimation and the ConnectednessApporach for calculating TCI , not sure if residuals tests are available ##

#----------------------------------

# Load necessary libraries
library(vars)
library(ConnectednessApproach)

# Forecast horizon
h <- 10

# Window size (max 75 allows the WCA to be captured)
window_size <- 150

# Initialize DataFrames for each method
TCI_df_Method1 <- data.frame(Date = as.Date(character()), TCI = numeric(), Model_Success = integer(), Num_Variables = integer(), Serial_Autocorrelation_p_value = numeric(), Stability = logical(), Normality_p_value = numeric(), stringsAsFactors = FALSE)
TCI_df_Method2 <- data.frame(Date = as.Date(character()), TCI = numeric(), Model_Success = integer(), Num_Variables = integer(), Serial_Autocorrelation_p_value = numeric(), Stability = logical(), Normality_p_value = numeric(), stringsAsFactors = FALSE)
TCI_df_Method3 <- data.frame(Date = as.Date(character()), TCI = numeric(), Model_Success = integer(), Num_Variables = integer(), Serial_Autocorrelation_p_value = numeric(), Stability = logical(), Normality_p_value = numeric(), stringsAsFactors = FALSE)

# Initialize DataFrames to store spillover matrices
Spillover_Matrices_Method1 <- list()
Spillover_Matrices_Method2 <- list()
Spillover_Matrices_Method3 <- list()

# Ensure the index of the zoo object is unique
return_zoo <- return_zoo[!duplicated(index(return_zoo)), ]

## Serial correlation test is Breusch-Godfrey LM-statistic , The null hypothesis is: H0 : B1 = . . . = Bh = 0  ##
## Normality test is multivariate Jarque-Bera tests

# Start the rolling window process for each method
for (start_index in 1:(nrow(return_zoo) - window_size + 1)) {
    
    # Define the end index for the current window
    end_index <- start_index + window_size - 1
    
    # Subset the data for the current window
    window_data <- return_zoo[start_index:end_index, ]
    
    # Extract the last date of this window for logging purposes
    last_date <- index(window_data)[window_size]
    
    # Only keep variables that have complete data for the entire window
    window_data <- window_data[, colSums(is.na(window_data)) == 0]
    
    # If no variables are left after filtering, skip to the next window
    if (ncol(window_data) == 0) next
    
    # Record the number of columns (variables) in the cleaned window
    num_variables <- ncol(window_data)
    
    # Determine the optimal lag length using VARselect
    lag_selection <- tryCatch({
        VARselect(window_data, lag.max = h, type = "const")
    }, error = function(e) {
        next
    })
    
    if (is.null(lag_selection)) next
    
    #optimal_lag <- lag_selection$selection["SC(n)"]
    optimal_lag = 1 # for testing

    #### Method 1 ####
    var_model_Method1 <- tryCatch({
        vars::VAR(window_data, p = optimal_lag, type = "const")
    }, error = function(e) {
        return(NULL)
    })
    
    if (!is.null(var_model_Method1)) {
        # Compute FEVD
        fevd_result <- fevd(var_model_Method1, n.ahead = h)
        spillover_matrix <- matrix(0, num_variables, num_variables)
        
        dimnames(spillover_matrix) <- list(
            Variables = colnames(window_data),
            Variables = colnames(window_data)
        )

        for (i in 1:num_variables) {
            for (j in 1:num_variables) {
                spillover_matrix[i, j] <- sum(fevd_result[[i]][, j])
            }
        }
        
        # Normalize the spillover matrix
        spillover_matrix <- sweep(spillover_matrix, 1, rowSums(spillover_matrix), "/")
        
        # Calculate TCI
        off_diag_sum <- sum(spillover_matrix) - sum(diag(spillover_matrix))
        total_variance <- sum(spillover_matrix)
        tci <- (off_diag_sum / total_variance) * 100
        
        # Check residual diagnostics
        serial_test <- serial.test(var_model_Method1, lags.bg = 1, type = "BG")
        serial_p_value <- serial_test$serial$p.value
        
        stability <- all(roots(var_model_Method1) < 1)
        
        normality_test <- normality.test(var_model_Method1)
        normality_p_value <- normality_test$jb.mul$JB$p.value
        
        # Store the results
        TCI_df_Method1 <- rbind(TCI_df_Method1, data.frame(Date = last_date, TCI = tci, Model_Success = 1, Num_Variables = num_variables, Serial_Autocorrelation_p_value = serial_p_value, Stability = stability, Normality_p_value = normality_p_value))
        Spillover_Matrices_Method1[[length(Spillover_Matrices_Method1) + 1]] <- list(Date = last_date, Spillover_Matrix = spillover_matrix)
    } else {
        TCI_df_Method1 <- rbind(TCI_df_Method1, data.frame(Date = last_date, TCI = NA, Model_Success = 0, Num_Variables = num_variables, Serial_Autocorrelation_p_value = NA, Stability = NA, Normality_p_value = NA))
    }
    
    #### Method 2 ####
    var_model_Method2 <- tryCatch({
        vars::VAR(window_data, p = optimal_lag, type = "const")
    }, error = function(e) {
        return(NULL)
    })
    
    if (!is.null(var_model_Method2)) {
        # Using the ConnectednessApproach
        fit <- var_model_Method2
        B <- t(matrix(unlist(coefficients(fit)), ncol = num_variables))[,1:(num_variables * optimal_lag)]
        Q <- summary(fit)$covres
        dca <- TimeConnectedness(B, Q, nfore = h)
        tci <- dca$TCI
        
        # Check residual diagnostics
        serial_test <- serial.test(var_model_Method2, lags.bg = 1, type = "BG")
        serial_p_value <- serial_test$serial$p.value
        
        stability <- all(roots(var_model_Method2) < 1)
        
        normality_test <- normality.test(var_model_Method2)
        normality_p_value <- normality_test$jb.mul$JB$p.value
        
        # Store the results
        TCI_df_Method2 <- rbind(TCI_df_Method2, data.frame(Date = last_date, TCI = tci, Model_Success = 1, Num_Variables = num_variables, Serial_Autocorrelation_p_value = serial_p_value, Stability = stability, Normality_p_value = normality_p_value))
        Spillover_Matrices_Method2[[length(Spillover_Matrices_Method2) + 1]] <- list(Date = last_date, Spillover_Matrix = dca$TABLE)
    } else {
        TCI_df_Method2 <- rbind(TCI_df_Method2, data.frame(Date = last_date, TCI = NA, Model_Success = 0, Num_Variables = num_variables, Serial_Autocorrelation_p_value = NA, Stability = NA, Normality_p_value = NA))
    }
    
    #### Method 3 ####
    dca_Method3 <- tryCatch({
        ConnectednessApproach(window_data, nlag = optimal_lag, nfore = h, model = "VAR", corrected = TRUE, window.size = NULL)
    }, error = function(e) {
        return(NULL)
    })
    
    if (!is.null(dca_Method3)) {
        tci <- dca$TCI # Average TCI across horizons
        
        # Store the results (Method 3 doesn't involve a VAR model directly, so diagnostics are not applicable in the same way)
        TCI_df_Method3 <- rbind(TCI_df_Method3, data.frame(Date = last_date, TCI = tci, Model_Success = 1, Num_Variables = num_variables, Serial_Autocorrelation_p_value = NA, Stability = NA, Normality_p_value = NA))
        Spillover_Matrices_Method3[[length(Spillover_Matrices_Method3) + 1]] <- list(Date = last_date, Spillover_Matrix = dca_Method3$TABLE)
    } else {
        TCI_df_Method3 <- rbind(TCI_df_Method3, data.frame(Date = last_date, TCI = NA, Model_Success = 0, Num_Variables = num_variables, Serial_Autocorrelation_p_value = NA, Stability = NA, Normality_p_value = NA))
    }
}

# Count the number of models 
# where the BG test p-value is less than 0.05 (fail - reject the null that there is no serial correlation of any order up to p.) and 
# the normality p-value is less than 0.05 (fail - reject the null that residuals resemble that of teh normal dsitribution, i.e. not normal ormality)

# List of data frames for each method
methods <- list(TCI_df_Method1, TCI_df_Method2, TCI_df_Method3)
method_names <- c("Method1", "Method2", "Method3")

# Initialize an empty list to store the results
failure_stats_list <- list()

# Loop over each method
for (i in 1:length(methods)) {
  method_df <- methods[[i]]
  method_name <- method_names[i]
  
  # Calculate the failure stats
  failure_stats <- method_df %>%
    mutate(Serial_Failed = ifelse(Serial_Autocorrelation_p_value < 0.05, 1, 0),
           Normality_Failed = ifelse(Normality_p_value < 0.05, 1, 0),
           Either_Failed = ifelse(Serial_Failed == 1 | Normality_Failed == 1, 1, 0))
  
  # Calculate the proportion of failed tests
  Proportion_Serial_Failed <- mean(failure_stats$Serial_Failed)
  Proportion_Normality_Failed <- mean(failure_stats$Normality_Failed)
  
  # Add the result to the list
  failure_stats_list[[method_name]] <- list(
    failure_stats = failure_stats,
    Proportion_Serial_Failed = Proportion_Serial_Failed,
    Proportion_Normality_Failed = Proportion_Normality_Failed
  )
}

# Save as a summary data frame
failure_stats_df <- data.frame(
  Method = method_names,
  Proportion_Serial_Failed = sapply(failure_stats_list, function(x) x$Proportion_Serial_Failed),
  Proportion_Normality_Failed = sapply(failure_stats_list, function(x) x$Proportion_Normality_Failed)
)

# Create file name based on h and window_size
file_name <- paste0("failure_stats_h", h, "_window", window_size, ".csv")

# Export the results to a CSV file using the dynamically created file name
write.csv(failure_stats_df, file = file_name, row.names = FALSE)

# Save results
write.csv(TCI_df_Method1, file = "TCI_results_Method1.csv", row.names = FALSE)
write.csv(TCI_df_Method2, file = "TCI_results_Method2.csv", row.names = FALSE)
write.csv(TCI_df_Method3, file = "TCI_results_Method3.csv", row.names = FALSE)

# Save Spillover Matrices
# save(Spillover_Matrices_Method1, file = "Spillover_Matrices_Method1.RData")
# save(Spillover_Matrices_Method2, file = "Spillover_Matrices_Method2.RData")
# save(Spillover_Matrices_Method3, file = "Spillover_Matrices_Method3.RData")

# # Average spillover matrices
# average_spillover_matrix_Method1 <- Reduce("+", lapply(Spillover_Matrices_Method1, function(x) x$Spillover_Matrix)) / length(Spillover_Matrices_Method1)
# average_spillover_matrix_Method2 <- Reduce("+", lapply(Spillover_Matrices_Method2, function(x) x$Spillover_Matrix)) / length(Spillover_Matrices_Method2)
# average_spillover_matrix_Method3 <- Reduce("+", lapply(Spillover_Matrices_Method3, function(x) x$Spillover_Matrix)) / length(Spillover_Matrices_Method3)

# # Output the average spillover matrices
# average_spillover_matrix_Method1
# average_spillover_matrix_Method2
# average_spillover_matrix_Method3

# Combine the TCI DataFrames for plotting

# Adapt TCI_df_Method1 to only include TCI, number of variables
TCI_Method1 <- TCI_df_Method1[, c("Date", "TCI","Num_Variables")]
TCI_Method2 <- TCI_df_Method2[, c("Date", "TCI","Num_Variables")]
TCI_Method3 <- TCI_df_Method3[, c("Date", "TCI","Num_Variables")]

# Merge all 3 TCI DataFrames, naming the columns appropriately
TCI_df <- merge(TCI_Method1, TCI_Method2, by = "Date", suffixes = c(" - 1", " - 2"))
# Merge TCI_df with TCI_Method3, adding suffix " - 3" to TCI_Method3 columns in one line
TCI_df <- merge(TCI_df, rename_with(TCI_Method3, ~ paste0(., " - 3"), -Date), by = "Date")

# Remove any columns where NA 
TCI_df <- TCI_df[, colSums(is.na(TCI_df)) < 3]

# Create file name based on h and window_size
file_name <- paste0("TCI_plot_h", h, "_window", window_size, ".csv")

# Export the results to a CSV file using the dynamically created file name
write.csv(TCI_df, file = file_name, row.names = FALSE)

# Assuming TCI_df has columns: Date, `TVI - 1`, `TCI - 2`, `TCI - 3`, `Num_Variables - 1`, `Num_Variables - 2`, `Num_Variables - 3`

# Plot the 3 connectedness measures on the same plot, scale is 0-100
ggplot(TCI_df, aes(x = Date)) +
  geom_line(aes(y = `TCI - 1`, color = "TVI - 1")) +
  geom_line(aes(y = `TCI - 2`, color = "TCI - 2")) +
  geom_line(aes(y = `TCI - 3`, color = "TCI - 3")) +
  geom_bar(aes(y = `Num_Variables - 1` * 100 / max(`Num_Variables - 1`), fill = "Num_Variables - 1"), stat = "identity", alpha = 0.3) +
  geom_bar(aes(y = `Num_Variables - 2` * 100 / max(`Num_Variables - 2`), fill = "Num_Variables - 2"), stat = "identity", alpha = 0.3) +
  geom_bar(aes(y = `Num_Variables - 3` * 100 / max(`Num_Variables - 3`), fill = "Num_Variables - 3"), stat = "identity", alpha = 0.3) +
  scale_y_continuous(
    name = "Total Connectedness Index (TCI)",
    limits = c(0, 100),
    sec.axis = sec_axis(~ . * max(c(TCI_df$`Num_Variables - 1`, TCI_df$`Num_Variables - 2`, TCI_df$`Num_Variables - 3`)) / 100, name = "Number of Variables")
  ) +
  labs(title = "Total Connectedness Index Over Time",
       x = "Date") +
  scale_color_manual(values = c("TVI - 1" = "blue", "TCI - 2" = "red", "TCI - 3" = "green")) +
  scale_fill_manual(values = c("Num_Variables - 1" = "grey", "Num_Variables - 2" = "lightgrey", "Num_Variables - 3" = "darkgrey")) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )

# Create file name based on h and window_size
file_name <- paste0("TCI_plot_h", h, "_window", window_size, ".png")

# Export the plot with a white background using the dynamically created file name
ggsave(file_name, bg = "white")

# Plot serial autocorrelation p-values

# Adapt TCI_df_Method1 to only include TCI, number of variables
HGTest_Method1 <- TCI_df_Method1[, c("Date", "TCI","Serial_Autocorrelation_p_value")]
HGTest_Method2 <- TCI_df_Method2[, c("Date", "TCI","Serial_Autocorrelation_p_value")]
HGTest_Method3 <- TCI_df_Method3[, c("Date", "TCI","Serial_Autocorrelation_p_value")]

# Merge all 3 TCI DataFrames, naming the columns appropriately
HGTest_df <- merge(HGTest_Method1, HGTest_Method2, by = "Date", suffixes = c(" - 1", " - 2"))
# Merge TCI_df with TCI_Method3, adding suffix " - 3" to TCI_Method3 columns in one line
HGTest_df <- merge(HGTest_df, rename_with(HGTest_Method3, ~ paste0(., " - 3"), -Date), by = "Date")

# Remove any columns where the serial autocorrelation p-value is NA for all methods
HGTest_df <- HGTest_df[, colSums(is.na(HGTest_df)) < 3]

# Plot serial autocorrelation p-values and TCI on the same plot
ggplot(HGTest_df, aes(x = Date)) +
  geom_line(aes(y = `Serial_Autocorrelation_p_value - 1`, color = "Serial Autocorrelation - 1")) +
  geom_line(aes(y = `Serial_Autocorrelation_p_value - 2`, color = "Serial Autocorrelation - 2")) +
  
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "black") +
  labs(title = "Serial Autocorrelation p-values Over Time",
       x = "Date",
       y = "Serial Autocorrelation p-value") +
  scale_color_manual(values = c("Serial Autocorrelation - 1" = "blue", "Serial Autocorrelation - 2" = "red", "Serial Autocorrelation - 3" = "green")) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )

# Export the plot
ggsave("Serial_Autocorrelation_p_values.png")

# Plot normality test p-values

# Adapt TCI_df_Method1 to only include TCI, number of variables
JBTest_Method1 <- TCI_df_Method1[, c("Date", "TCI","Normality_p_value")]
JBTest_Method2 <- TCI_df_Method2[, c("Date", "TCI","Normality_p_value")]
JBTest_Method3 <- TCI_df_Method3[, c("Date", "TCI","Normality_p_value")]

# Merge all 3 TCI DataFrames, naming the columns appropriately
JBTest_df <- merge(JBTest_Method1, JBTest_Method2, by = "Date", suffixes = c(" - 1", " - 2"))
# Merge TCI_df with TCI_Method3, adding suffix " - 3" to TCI_Method3 columns in one line
JBTest_df <- merge(JBTest_df, rename_with(JBTest_Method3, ~ paste0(., " - 3"), -Date), by = "Date")

# Remove any columns where the serial autocorrelation p-value is NA for all methods
JBTest_df <- JBTest_df[, colSums(is.na(JBTest_df)) < 3]

# Plot normality p-values and TCI on the same plot
ggplot(JBTest_df, aes(x = Date)) +
  geom_line(aes(y = `Normality_p_value - 1`, color = "Normality - 1")) +
  geom_line(aes(y = `Normality_p_value - 2`, color = "Normality - 2")) +
  
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "black") +
  labs(title = "Normality p-values Over Time",
       x = "Date",
       y = "Normality p-value") +
  scale_color_manual(values = c("Normality - 1" = "blue", "Normality - 2" = "red", "Normality - 3" = "green")) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )

# Export the plot
ggsave("JB Test_plot.png")

#----------------------------------



## TVP-VAR model - Returns ##
#----------------------------------

# Specify the lag order
lag_order <- 1  # his analysis uses first-order VARs (p = 1) (selected by Schwarz information criterion), 
H <- 10 # with 10-step-ahead forecasts (H = 10).

# This study considers forgetting factor, kappa1=0.99 and decay factor kappa2=0.96 
# follows Antonakakis et al. to keep the decay factors constant at fixed values.
# See https://gabauerdavid.github.io/ConnectednessApproach/2020AntonakakisChatziantoniouGabauer
forgetting_factor <- 0.99
decay_factor <- 0.96

# David Gabauer approach
# https://gabauerdavid.github.io/ConnectednessApproach/2020AntonakakisChatziantoniouGabauer
# https://cran.r-project.org/web/packages/ConnectednessApproach/ConnectednessApproach.pdf

dca = ConnectednessApproach(return_zoo_Full, 
                            nlag=lag_order, 
                            nfore=H,
                            window.size=150,
                            model="TVP-VAR",
                            connectedness="Time",
                            VAR_config=list(TVPVAR=list(kappa1=forgetting_factor, kappa2=decay_factor, prior="BayesPrior"))) # TVP-VAR model with forgetting factor and decay factor as specified

## The TVP-VAR connectedness approach is implemented according to:
##  Antonakakis, N., Chatziantoniou, I., & Gabauer, D. (2020). Refined measures of dynamic connectedness based on time-varying parameter vector autoregressions. Journal of Risk and Financial Management, 13(4), 84.
# See https://gabauerdavid.github.io/ConnectednessApproach/2020AntonakakisChatziantoniouGabauer

## Computing connectedness measures
DCA = list()
WINDOW.SIZE = c(50, 75, 100, 125,150)
for (i in 1:length(WINDOW.SIZE)) {
 return[[i]] = suppressMessages(ConnectednessApproach(return_zoo_Full, 
                             nlag=lag_order, 
                             nfore=H,
                             window.size=WINDOW.SIZE[i]))
}

## Total Connectedness Index - TCI ##

#----------------------------------
# The total connectedness index (TCI) illustrates the average impact a shock in one series has on all others.

# Start PDF device before creating the plot
pdf("TCI_returns.pdf", width = 8, height = 6)  # Size in inches (default)

# Set larger margins (bottom, left, top, right) to avoid clipping of titles/labels
# Increase the left margin further for the y-axis title
par(mar=c(5, 5.5, 4, 2) + 0.1)  # Increased left margin

# Plot TCI data with adjusted limits and margins
PlotTCI(dca, 
        ca = NULL,
        ylim = c(-100, 100))

# PlotTCI(DCA, 
#         ca = NULL,
#         ylim = c(-100, 100))

# Add titles and axis labels with adjusted positions
#title("Total Connectedness Index (TCI) - Returns", line = 2.5, cex.main = 1.5)

# Overlay the event study window

# Close the device and save the plot
dev.off()


#----------------------------------
# Uses return_zoo_Full

# Load necessary libraries
library(vars)
library(ConnectednessApproach)

# Forecast horizon
h <- 10

# Window size (max 75 allows the WCA to be captured)
window_size <- 150

# Initialize DataFrames for each method
TCI_df_Method1 <- data.frame(Date = as.Date(character()), TCI = numeric(), Model_Success = integer(), Num_Variables = integer(), Serial_Autocorrelation_p_value = numeric(), Stability = logical(), Normality_p_value = numeric(), stringsAsFactors = FALSE)
TCI_df_Method2 <- data.frame(Date = as.Date(character()), TCI = numeric(), Model_Success = integer(), Num_Variables = integer(), Serial_Autocorrelation_p_value = numeric(), Stability = logical(), Normality_p_value = numeric(), stringsAsFactors = FALSE)
TCI_df_Method3 <- data.frame(Date = as.Date(character()), TCI = numeric(), Model_Success = integer(), Num_Variables = integer(), Serial_Autocorrelation_p_value = numeric(), Stability = logical(), Normality_p_value = numeric(), stringsAsFactors = FALSE)

# Initialize DataFrames to store spillover matrices
Spillover_Matrices_Method1 <- list()
Spillover_Matrices_Method2 <- list()
Spillover_Matrices_Method3 <- list()

# Ensure the index of the zoo object is unique
return_zoo_Full <- return_zoo_Full[!duplicated(index(return_zoo_Full)), ]

## Serial correlation test is Breusch-Godfrey LM-statistic , The null hypothesis is: H0 : B1 = . . . = Bh = 0  ##
## Normality test is multivariate Jarque-Bera tests

# Start the rolling window process for each method
for (start_index in 1:(nrow(return_zoo_Full) - window_size + 1)) {
    
    # Define the end index for the current window
    end_index <- start_index + window_size - 1
    
    # Subset the data for the current window
    window_data <- return_zoo_Full[start_index:end_index, ]
    
    # Extract the last date of this window for logging purposes
    last_date <- index(window_data)[window_size]
    
    # Only keep variables that have complete data for the entire window
    window_data <- window_data[, colSums(is.na(window_data)) == 0]
    
    # If no variables are left after filtering, skip to the next window
    if (ncol(window_data) == 0) next
    
    # Record the number of columns (variables) in the cleaned window
    num_variables <- ncol(window_data)
    
    # Determine the optimal lag length using VARselect
    lag_selection <- tryCatch({
        VARselect(window_data, lag.max = h, type = "const")
    }, error = function(e) {
        next
    })
    
    if (is.null(lag_selection)) next
    
    #optimal_lag <- lag_selection$selection["SC(n)"]
    optimal_lag = 1 # for testing

    #### Method 1 ####
    var_model_Method1 <- tryCatch({
        vars::VAR(window_data, p = optimal_lag, type = "const")
    }, error = function(e) {
        return(NULL)
    })
    
    if (!is.null(var_model_Method1)) {
        # Compute FEVD
        fevd_result <- fevd(var_model_Method1, n.ahead = h)
        spillover_matrix <- matrix(0, num_variables, num_variables)
        
        dimnames(spillover_matrix) <- list(
            Variables = colnames(window_data),
            Variables = colnames(window_data)
        )

        for (i in 1:num_variables) {
            for (j in 1:num_variables) {
                spillover_matrix[i, j] <- sum(fevd_result[[i]][, j])
            }
        }
        
        # Normalize the spillover matrix
        spillover_matrix <- sweep(spillover_matrix, 1, rowSums(spillover_matrix), "/")
        
        # Calculate TCI
        off_diag_sum <- sum(spillover_matrix) - sum(diag(spillover_matrix))
        total_variance <- sum(spillover_matrix)
        tci <- (off_diag_sum / total_variance) * 100
        
        # Check residual diagnostics
        serial_test <- serial.test(var_model_Method1, lags.bg = 1, type = "BG")
        serial_p_value <- serial_test$serial$p.value
        
        stability <- all(roots(var_model_Method1) < 1)
        
        normality_test <- normality.test(var_model_Method1)
        normality_p_value <- normality_test$jb.mul$JB$p.value
        
        # Store the results
        TCI_df_Method1 <- rbind(TCI_df_Method1, data.frame(Date = last_date, TCI = tci, Model_Success = 1, Num_Variables = num_variables, Serial_Autocorrelation_p_value = serial_p_value, Stability = stability, Normality_p_value = normality_p_value))
        Spillover_Matrices_Method1[[length(Spillover_Matrices_Method1) + 1]] <- list(Date = last_date, Spillover_Matrix = spillover_matrix)
    } else {
        TCI_df_Method1 <- rbind(TCI_df_Method1, data.frame(Date = last_date, TCI = NA, Model_Success = 0, Num_Variables = num_variables, Serial_Autocorrelation_p_value = NA, Stability = NA, Normality_p_value = NA))
    }
    
    #### Method 2 ####
    var_model_Method2 <- tryCatch({
        vars::VAR(window_data, p = optimal_lag, type = "const")
    }, error = function(e) {
        return(NULL)
    })
    
    if (!is.null(var_model_Method2)) {
        # Using the ConnectednessApproach
        fit <- var_model_Method2
        B <- t(matrix(unlist(coefficients(fit)), ncol = num_variables))[,1:(num_variables * optimal_lag)]
        Q <- summary(fit)$covres
        dca <- TimeConnectedness(B, Q, nfore = h)
        tci <- dca$TCI
        
        # Check residual diagnostics
        serial_test <- serial.test(var_model_Method2, lags.bg = 1, type = "BG")
        serial_p_value <- serial_test$serial$p.value
        
        stability <- all(roots(var_model_Method2) < 1)
        
        normality_test <- normality.test(var_model_Method2)
        normality_p_value <- normality_test$jb.mul$JB$p.value
        
        # Store the results
        TCI_df_Method2 <- rbind(TCI_df_Method2, data.frame(Date = last_date, TCI = tci, Model_Success = 1, Num_Variables = num_variables, Serial_Autocorrelation_p_value = serial_p_value, Stability = stability, Normality_p_value = normality_p_value))
        Spillover_Matrices_Method2[[length(Spillover_Matrices_Method2) + 1]] <- list(Date = last_date, Spillover_Matrix = dca$TABLE)
    } else {
        TCI_df_Method2 <- rbind(TCI_df_Method2, data.frame(Date = last_date, TCI = NA, Model_Success = 0, Num_Variables = num_variables, Serial_Autocorrelation_p_value = NA, Stability = NA, Normality_p_value = NA))
    }
    
    #### Method 3 ####
    dca_Method3 <- tryCatch({
        ConnectednessApproach(window_data, nlag = optimal_lag, nfore = h, model = "VAR", corrected = TRUE, window.size = NULL)
    }, error = function(e) {
        return(NULL)
    })
    
    if (!is.null(dca_Method3)) {
        tci <- dca$TCI # Average TCI across horizons
        
        # Store the results (Method 3 doesn't involve a VAR model directly, so diagnostics are not applicable in the same way)
        TCI_df_Method3 <- rbind(TCI_df_Method3, data.frame(Date = last_date, TCI = tci, Model_Success = 1, Num_Variables = num_variables, Serial_Autocorrelation_p_value = NA, Stability = NA, Normality_p_value = NA))
        Spillover_Matrices_Method3[[length(Spillover_Matrices_Method3) + 1]] <- list(Date = last_date, Spillover_Matrix = dca_Method3$TABLE)
    } else {
        TCI_df_Method3 <- rbind(TCI_df_Method3, data.frame(Date = last_date, TCI = NA, Model_Success = 0, Num_Variables = num_variables, Serial_Autocorrelation_p_value = NA, Stability = NA, Normality_p_value = NA))
    }
}

# Count the number of models 
# where the BG test p-value is less than 0.05 (fail - reject the null that there is no serial correlation of any order up to p.) and 
# the normality p-value is less than 0.05 (fail - reject the null that residuals resemble that of teh normal dsitribution, i.e. not normal ormality)

# List of data frames for each method
methods <- list(TCI_df_Method1, TCI_df_Method2, TCI_df_Method3)
method_names <- c("Method1", "Method2", "Method3")

# Initialize an empty list to store the results
failure_stats_list <- list()

# Loop over each method
for (i in 1:length(methods)) {
  method_df <- methods[[i]]
  method_name <- method_names[i]
  
  # Calculate the failure stats
  failure_stats <- method_df %>%
    mutate(Serial_Failed = ifelse(Serial_Autocorrelation_p_value < 0.05, 1, 0),
           Normality_Failed = ifelse(Normality_p_value < 0.05, 1, 0),
           Either_Failed = ifelse(Serial_Failed == 1 | Normality_Failed == 1, 1, 0))
  
  # Calculate the proportion of failed tests
  Proportion_Serial_Failed <- mean(failure_stats$Serial_Failed)
  Proportion_Normality_Failed <- mean(failure_stats$Normality_Failed)
  
  # Add the result to the list
  failure_stats_list[[method_name]] <- list(
    failure_stats = failure_stats,
    Proportion_Serial_Failed = Proportion_Serial_Failed,
    Proportion_Normality_Failed = Proportion_Normality_Failed
  )
}

# Save as a summary data frame
failure_stats_df <- data.frame(
  Method = method_names,
  Proportion_Serial_Failed = sapply(failure_stats_list, function(x) x$Proportion_Serial_Failed),
  Proportion_Normality_Failed = sapply(failure_stats_list, function(x) x$Proportion_Normality_Failed)
)

# Create file name based on h and window_size
file_name <- paste0("failure_stats_h", h, "_window", window_size, ".csv")

# Export the results to a CSV file using the dynamically created file name
write.csv(failure_stats_df, file = file_name, row.names = FALSE)

# Save results
write.csv(TCI_df_Method1, file = "TCI_results_Method1.csv", row.names = FALSE)
write.csv(TCI_df_Method2, file = "TCI_results_Method2.csv", row.names = FALSE)
write.csv(TCI_df_Method3, file = "TCI_results_Method3.csv", row.names = FALSE)

# Save Spillover Matrices
# save(Spillover_Matrices_Method1, file = "Spillover_Matrices_Method1.RData")
# save(Spillover_Matrices_Method2, file = "Spillover_Matrices_Method2.RData")
# save(Spillover_Matrices_Method3, file = "Spillover_Matrices_Method3.RData")

# # Average spillover matrices
# average_spillover_matrix_Method1 <- Reduce("+", lapply(Spillover_Matrices_Method1, function(x) x$Spillover_Matrix)) / length(Spillover_Matrices_Method1)
# average_spillover_matrix_Method2 <- Reduce("+", lapply(Spillover_Matrices_Method2, function(x) x$Spillover_Matrix)) / length(Spillover_Matrices_Method2)
# average_spillover_matrix_Method3 <- Reduce("+", lapply(Spillover_Matrices_Method3, function(x) x$Spillover_Matrix)) / length(Spillover_Matrices_Method3)

# # Output the average spillover matrices
# average_spillover_matrix_Method1
# average_spillover_matrix_Method2
# average_spillover_matrix_Method3

# Combine the TCI DataFrames for plotting

# Adapt TCI_df_Method1 to only include TCI, number of variables
TCI_Method1 <- TCI_df_Method1[, c("Date", "TCI","Num_Variables")]
TCI_Method2 <- TCI_df_Method2[, c("Date", "TCI","Num_Variables")]
TCI_Method3 <- TCI_df_Method3[, c("Date", "TCI","Num_Variables")]

# Merge all 3 TCI DataFrames, naming the columns appropriately
TCI_df <- merge(TCI_Method1, TCI_Method2, by = "Date", suffixes = c(" - 1", " - 2"))
# Merge TCI_df with TCI_Method3, adding suffix " - 3" to TCI_Method3 columns in one line
TCI_df <- merge(TCI_df, rename_with(TCI_Method3, ~ paste0(., " - 3"), -Date), by = "Date")

# Remove any columns where NA 
TCI_df <- TCI_df[, colSums(is.na(TCI_df)) < 3]

# Create file name based on h and window_size
file_name <- paste0("TCI_plot_h", h, "_window", window_size, ".csv")

# Export the results to a CSV file using the dynamically created file name
write.csv(TCI_df, file = file_name, row.names = FALSE)

# Assuming TCI_df has columns: Date, `TVI - 1`, `TCI - 2`, `TCI - 3`, `Num_Variables - 1`, `Num_Variables - 2`, `Num_Variables - 3`

# Plot the 3 connectedness measures on the same plot, scale is 0-100
ggplot(TCI_df, aes(x = Date)) +
  geom_line(aes(y = `TCI - 1`, color = "TVI - 1")) +
  geom_line(aes(y = `TCI - 2`, color = "TCI - 2")) +
  geom_line(aes(y = `TCI - 3`, color = "TCI - 3")) +
  geom_bar(aes(y = `Num_Variables - 1` * 100 / max(`Num_Variables - 1`), fill = "Num_Variables - 1"), stat = "identity", alpha = 0.3) +
  geom_bar(aes(y = `Num_Variables - 2` * 100 / max(`Num_Variables - 2`), fill = "Num_Variables - 2"), stat = "identity", alpha = 0.3) +
  geom_bar(aes(y = `Num_Variables - 3` * 100 / max(`Num_Variables - 3`), fill = "Num_Variables - 3"), stat = "identity", alpha = 0.3) +
  scale_y_continuous(
    name = "Total Connectedness Index (TCI)",
    limits = c(0, 100),
    sec.axis = sec_axis(~ . * max(c(TCI_df$`Num_Variables - 1`, TCI_df$`Num_Variables - 2`, TCI_df$`Num_Variables - 3`)) / 100, name = "Number of Variables")
  ) +
  labs(title = "Total Connectedness Index Over Time",
       x = "Date") +
  scale_color_manual(values = c("TVI - 1" = "blue", "TCI - 2" = "red", "TCI - 3" = "green")) +
  scale_fill_manual(values = c("Num_Variables - 1" = "grey", "Num_Variables - 2" = "lightgrey", "Num_Variables - 3" = "darkgrey")) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )

# Create file name based on h and window_size
file_name <- paste0("TCI_plot_h", h, "_window", window_size, ".png")

# Export the plot with a white background using the dynamically created file name
ggsave(file_name, bg = "white")

# Plot serial autocorrelation p-values

# Adapt TCI_df_Method1 to only include TCI, number of variables
HGTest_Method1 <- TCI_df_Method1[, c("Date", "TCI","Serial_Autocorrelation_p_value")]
HGTest_Method2 <- TCI_df_Method2[, c("Date", "TCI","Serial_Autocorrelation_p_value")]
HGTest_Method3 <- TCI_df_Method3[, c("Date", "TCI","Serial_Autocorrelation_p_value")]

# Merge all 3 TCI DataFrames, naming the columns appropriately
HGTest_df <- merge(HGTest_Method1, HGTest_Method2, by = "Date", suffixes = c(" - 1", " - 2"))
# Merge TCI_df with TCI_Method3, adding suffix " - 3" to TCI_Method3 columns in one line
HGTest_df <- merge(HGTest_df, rename_with(HGTest_Method3, ~ paste0(., " - 3"), -Date), by = "Date")

# Remove any columns where the serial autocorrelation p-value is NA for all methods
HGTest_df <- HGTest_df[, colSums(is.na(HGTest_df)) < 3]

# Plot serial autocorrelation p-values and TCI on the same plot
ggplot(HGTest_df, aes(x = Date)) +
  geom_line(aes(y = `Serial_Autocorrelation_p_value - 1`, color = "Serial Autocorrelation - 1")) +
  geom_line(aes(y = `Serial_Autocorrelation_p_value - 2`, color = "Serial Autocorrelation - 2")) +
  
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "black") +
  labs(title = "Serial Autocorrelation p-values Over Time",
       x = "Date",
       y = "Serial Autocorrelation p-value") +
  scale_color_manual(values = c("Serial Autocorrelation - 1" = "blue", "Serial Autocorrelation - 2" = "red", "Serial Autocorrelation - 3" = "green")) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )

# Export the plot
ggsave("Serial_Autocorrelation_p_values.png")

# Plot normality test p-values

# Adapt TCI_df_Method1 to only include TCI, number of variables
JBTest_Method1 <- TCI_df_Method1[, c("Date", "TCI","Normality_p_value")]
JBTest_Method2 <- TCI_df_Method2[, c("Date", "TCI","Normality_p_value")]
JBTest_Method3 <- TCI_df_Method3[, c("Date", "TCI","Normality_p_value")]

# Merge all 3 TCI DataFrames, naming the columns appropriately
JBTest_df <- merge(JBTest_Method1, JBTest_Method2, by = "Date", suffixes = c(" - 1", " - 2"))
# Merge TCI_df with TCI_Method3, adding suffix " - 3" to TCI_Method3 columns in one line
JBTest_df <- merge(JBTest_df, rename_with(JBTest_Method3, ~ paste0(., " - 3"), -Date), by = "Date")

# Remove any columns where the serial autocorrelation p-value is NA for all methods
JBTest_df <- JBTest_df[, colSums(is.na(JBTest_df)) < 3]

# Plot normality p-values and TCI on the same plot
ggplot(JBTest_df, aes(x = Date)) +
  geom_line(aes(y = `Normality_p_value - 1`, color = "Normality - 1")) +
  geom_line(aes(y = `Normality_p_value - 2`, color = "Normality - 2")) +
  
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "black") +
  labs(title = "Normality p-values Over Time",
       x = "Date",
       y = "Normality p-value") +
  scale_color_manual(values = c("Normality - 1" = "blue", "Normality - 2" = "red", "Normality - 3" = "green")) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )

# Export the plot
ggsave("JB Test_plot.png")