## Econometrics Research Project , Semester 2, 2024
## Run File for Replication of the Asymmetric TVP-VAR Model
## Henry Wyld

###### READ THIS TO RUN ######
# Run each script individually in the order below

# Continue running the scripts in order.

# If want to reproduce from raw data, then:
#-------------------------------------
# Need the following source files 
#-------------------------------------
# Store the Folder Named 'Data' in your working directory
# Store the following files in your working director
# a) EUR Cross Rates 20Y 16082024
# b) events_study_data.csv
# c) Data folder containing the ICAP and Clearblue data
#-------------------------------------
# Run all R files in run list below
#-------------------------------------

# If not wanting to reproduce from raw data, then:
#-------------------------------------
# Need the following file:
# Cleaned Research Data: ("Research_data.csv") - source from Git wd
# Event Study data: ("events_study_data.csv") - source from Git wd
# Only Run those files marked from 2) in run list below
#-------------------------------------

# 1)
# Set the working directory to where your R scripts are located

Git <- "C:/Users/henry/OneDrive - The University of Melbourne/GitHub/Final Estimation for Research Project"

# Ensure the directory exists
if (!dir.exists(Git)) {
  dir.create(Git)
}

setwd(Git)

# This may have to be defined in each run script separatel to ensure the correct working directory is set

# 1)
# Load the necessary packages

### BEFORE YOU RUN ###
#-------------------------------------
## Go into Packages.r and define your working directory for Git in the function set_working_directories
#-------------------------------------
source("Packages.r")

#-------------------------------------
# 1) IF REPRODUCING FROM RAW DATA
#-------------------------------------
    # ICAP Data Read - Loads and preprocesses data from ICAP
    source("ICAP Data Read.r")

    # Warnings may appear after running script, this is expected. 
    # Continue running the scripts in order.

    # Clearblue Data Read - Loads and preprocesses data from Clearblue
    setwd(Git)
    source("Clearblue Data Read.r")

    # Warnings may appear after running script, this is expected. 
    # Continue running the scripts in order.

    setwd(Git)
    source("Exchange Rate Conversion.r")

    # Warnings may appear after running script, this is expected. 
    # Continue running the scripts in order.

    # Create Research Data - Aggregates and prepares research data for analysis
    setwd(Git)
    source("Create Research Data.r")

    # may have to run this independently to ensure the correct working directory is set
#-------------------------------------

#-------------------------------------
# 2) IF NOT REPRODUCING FROM RAW DATA
#-------------------------------------
# Weekly Returns - Calculates weekly returns from the processed data
setwd(Git)
source("Weekly Returns.r")

# 3)
# TVP-VAR model - Fits a Time-Varying Parameter Vector Autoregression model on the dataset
#-------------------------------------
# RUN THIS FILE SEPARATELY
#-------------------------------------
setwd(Git)
source("Asymmetric TVP-VAR model.r")


# Finished
message("All scripts have been run successfully.")