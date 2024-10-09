### Econometrics Research Project , Semester 2, 2024
## The pursuit of a global carbon market; A time varying analysis of international compliance carbon markets
## Asymmetric TVP-VAR model estimation procedures in R using the ConnectednessApproach package
## Author: Henry Wyld
## Date of creation: 2024-10-06

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

# Update path for repository
Asym <- "C:/Users/henry/OneDrive - The University of Melbourne/GitHub/TVP-VAR-for-Compliance-Carbon-Markets/Asymmetric Connectedness bvars"

AsymHTesting <- paste0(Asym, "/Horizon Test")
AsymWTesting <- paste0(Asym, "/Window Test")
AsymLagTesting <- paste0(Asym, "/Lag Test")
AsymGammaTesting <- paste0(Asym, "/Gamma Test")

# Ensure the directory exists
if (!dir.exists(Asym)) {
  dir.create(Asym)
}

#----------------------------------

## Import data ##

#----------------------------------

return_df <- read.csv("Research_Data_weekly_returns.csv")
vol_df <- read.csv("Research_Data_weekly_volatility.csv")

# End date
end_date <- as.Date("2024-08-25")

# Correct subsetting using dplyr
return_df <- return_df %>%
  filter(Date >= as.Date("2014-04-30") & Date <= as.Date(end_date))

vol_df <- vol_df %>%
  filter(Date >= as.Date("2014-04-30") & Date <= as.Date(end_date))

# Convert the data to zoo objects
return_zoo <- zoo(return_df[, -1], order.by = as.Date(return_df$Date))
vol_zoo <- zoo(vol_df[, -1], order.by = as.Date(vol_df$Date))

#----------------------------------

## Data Cleaning ##

#----------------------------------

# Limit to only EUA, NZU, HBEA, and CCA + two extensions in KAU and ACCU
return_zoo <- return_zoo[, c("EUA", "NZU", "HBEA", "CCA", "KAU", "ACCU","RGGI")]
vol_zoo <- vol_zoo[, c("EUA", "NZU", "HBEA", "CCA", "KAU", "ACCU","RGGI")]

# If there are any NAs or infinite values, removing or imputing them
return_zoo <- na.omit(return_zoo)  # Removes entire rows where any NA values are present
vol_zoo <- na.omit(vol_zoo)  # Removes entire rows where any NA values are present

# Check lengths of the two zoo objects
length(index(return_zoo))
length(index(vol_zoo))

# Create a full variable subset (without WCA given size)
# Convert the data to zoo objects
return_zoo_Full <- zoo(return_df[, -1], order.by = as.Date(return_df$Date))
vol_zoo_Full <- zoo(vol_df[, -1], order.by = as.Date(vol_df$Date))

# Remove WCA from the full zoo objects
return_zoo_Full <- return_zoo_Full[, -which(colnames(return_zoo_Full) == "WCA")]
vol_zoo_Full <- vol_zoo_Full[, -which(colnames(vol_zoo_Full) == "WCA")]

# If there are any NAs or infinite values, removing or imputing them
return_zoo_Full <- na.omit(return_zoo_Full)  # Removes entire rows where any NA values are present
vol_zoo_Full <- na.omit(vol_zoo_Full)  # Removes entire rows where any NA values are present

# Check lengths of the two zoo objects
length(index(return_zoo_Full))
length(index(vol_zoo_Full))

# # If there are any NAs, impute with mean
# return_zoo <- na.fill(return_zoo, fill = mean(return_zoo, na.rm = TRUE))
# vol_zoo <- na.fill(vol_zoo, fill = mean(vol_zoo, na.rm = TRUE))

# # If there are any infinite values, replace with mean
# return_zoo[is.infinite(return_zoo)] <- mean(return_zoo[!is.infinite(return_zoo)], na.rm = TRUE)
# vol_zoo[is.infinite(vol_zoo)] <- mean(vol_zoo[!is.infinite(vol_zoo)], na.rm = TRUE)

#----------------------------------

## Define Event Study Window ##

#----------------------------------

# Import data
events_study_df <- read.csv("events_study_data.csv")

# Convert the StartDate and EndDate columns to Date objects where format is dd/mm/yyyy
events_study_df$StartDate <- as.Date(events_study_df$StartDate, format = "%d/%m/%Y")
events_study_df$EndDate <- as.Date(events_study_df$EndDate, format = "%d/%m/%Y")
events_study_df$Midpoint <- as.Date(events_study_df$Midpoint, format = "%d/%m/%Y")

#----------------------------------

#### Asymmetric Series Preparation ####

#------------------------------------------------------------------------

prepare_asym_series <- function(zoo_object) {
  Y = Yp = Yn = zoo_object
  k = ncol(Y)
  for (i in 1:k) {
    Yp[which(Y[, i] < 0), i] = 0
    Yn[which(Y[, i] > 0), i] = 0
  }
  return(list(Y = Y, Yp = Yp, Yn = Yn))
}

# Prepare asymmetric series for both returns and volatility
asym_return <- prepare_asym_series(return_zoo)
asym_vol <- prepare_asym_series(vol_zoo)

#----------------------------------

## Asymmetric TVP-VAR models ##

#----------------------------------

# Model Parameters

  lag_order <- 1
  H <- 10
  window_size <- 50


# TVP-VAR Parameters
# Follows Adekoya, Akinseye, Antonakakis, Chatziantoniou, Gabauer, and Oliyide (2021)
# https://gabauerdavid.github.io/ConnectednessApproach/2022Adekoya
  forgetting_factor_asym <- 0.99
  decay_factor_asym <- 0.99


# follows Antonakakis et al. to keep the decay factors constant at fixed values.
# See https://gabauerdavid.github.io/ConnectednessApproach/2020AntonakakisChatziantoniouGabauer
  forgetting_factor <- 0.99
  decay_factor <- 0.96

# Diagonal value of variance-covariance matrix
gamma.setting <- 5

# Load necessary packages
library(bvarsv)
library(ggplot2)
library(reshape2)
library(plotly)

# Update path for repository
Asym <- "C:/Users/henry/OneDrive - The University of Melbourne/GitHub/TVP-VAR-for-Compliance-Carbon-Markets/Asymmetric Connectedness bvars"

#----------------------------------

## Data Preparation ##

# Load and prepare data as previously done
return_df <- read.csv("Research_Data_weekly_returns.csv")
vol_df <- read.csv("Research_Data_weekly_volatility.csv")

end_date <- as.Date("2024-08-25")

# Data filtering for the relevant dates
return_df <- return_df %>%
  filter(Date >= as.Date("2014-04-30") & Date <= as.Date(end_date))

vol_df <- vol_df %>%
  filter(Date >= as.Date("2014-04-30") & Date <= as.Date(end_date))

# Convert to zoo objects
return_zoo <- zoo(return_df[, -1], order.by = as.Date(return_df$Date))
vol_zoo <- zoo(vol_df[, -1], order.by = as.Date(vol_df$Date))

# Clean NA values
return_zoo <- na.omit(return_zoo)
vol_zoo <- na.omit(vol_zoo)

# Prepare asymmetric series function
prepare_asym_series <- function(zoo_object) {
  Y <- Yp <- Yn <- zoo_object
  k <- ncol(Y)
  for (i in 1:k) {
    Yp[which(Y[, i] < 0), i] <- 0
    Yn[which(Y[, i] > 0), i] <- 0
  }
  return(list(Y = Y, Yp = Yp, Yn = Yn))
}

# Prepare asymmetric return series
asym_return <- prepare_asym_series(return_zoo)
asym_vol <- prepare_asym_series(vol_zoo)

#----------------------------------

data <- return_df[,-1]

# Run BVAR-SV using bvarsv package and extract standard errors
run_and_save_bvarsv <- function(return_zoo, suffix) {

  # Confidence interval parameters
  cred_interval <- c(0.025, 0.975)  # 95% credible interval

  cat("Running BVAR-SV for return_zoo series\n")

  # Run BVAR with stochastic volatility
  model_fit <- bvar.sv.tvp(return_zoo, p = lag_order)

  # Extract posterior means and standard deviations (SE)
  posterior_means <- apply(model_fit$beta, 2, mean)  # Posterior means
  posterior_sds <- apply(model_fit$beta, 2, sd)  # Standard errors (posterior SDs)

  # Compute credible intervals (95%)
  lower_ci <- apply(model_fit$beta, 2, quantile, probs = cred_interval[1])
  upper_ci <- apply(model_fit$beta, 2, quantile, probs = cred_interval[2])

  # Debugging check
  cat("Sample TCI values for return_zoo:\n")
  print(head(data.frame(TCI = posterior_means, SE = posterior_sds, Lower_CI = lower_ci, Upper_CI = upper_ci)))

  ## Plotting the results ##
  # Create time series (index) for plotting
  time_index <- 1:length(posterior_means)

  # Create a dataframe for plotting
  df_plot <- data.frame(
    Time = time_index,
    TCI = posterior_means,
    CI_Lower = lower_ci,
    CI_Upper = upper_ci
  )

  # Plot TCI with confidence intervals
  p <- ggplot(df_plot, aes(x = Time)) +
    geom_line(aes(y = TCI, color = "TCI"), size = 1) +
    geom_ribbon(aes(ymin = CI_Lower, ymax = CI_Upper), alpha = 0.2, fill = "grey") +
    labs(title = paste0("TCI with Confidence Intervals - return_zoo"),
         x = "Time", y = "TCI") +
    theme_minimal() +
    theme(legend.position = "bottom")

  # Save plot
  ggsave(file.path(Asym, paste0("TCI_CI_return_zoo_", suffix, ".png")), plot = p, width = 8, height = 6)
  
  # Optional: Convert to Plotly for interactivity
  plotly::ggplotly(p)

  return(list(TCI = posterior_means, SE = posterior_sds, CI = list(Lower = lower_ci, Upper = upper_ci)))
}

# Run the BVAR-SV for return_zoo and generate plots
DCA_return <- run_and_save_bvarsv(return_zoo, "r")
