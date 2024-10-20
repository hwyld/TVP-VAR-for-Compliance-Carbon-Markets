### Econometrics Research Project , Semester 2, 2024
## Centralised functions for installing and loading packages and setting working directories
## Henry Wyld
## Date of creation: 2024-07-19

# Function to install, load, and optionally update packages
load_packages <- function(update = FALSE) {
  # List of packages with descriptions
  packages <- list(
    devtools = "install and load packages from GitHub",
    ggplot2 = "flexible package for plotting in R", 
    rio = "easily read data into R", 
    car = "joint linear restrictions in R", 
    lmtest = "hypothesis tests in R", 
    sandwich = "robust variance estimator in R", 
    quantmod = "download financial data", 
    dynlm = "time-series models in R", 
    estimatr = "robust standard errors in R", 
    zoo = "handling time-series dates in R", 
    broom = "used for ggplot2 tidy", 
    tidyr = "used for ggplot2 spread", 
    scales = "used for formatting scales in ggplot2",
    dplyr = "data manipulation and transformation", 
    stringr = "string operations", 
    lubridate = "date-time manipulations",
    htmltools = "tools for HTML generation and output",
    htmlwidgets = "widgets for HTML output",
    plotly = "interactive plots in R",
    radiant = "interface for business analytics in R",
    httpgd = "graphics device for R",
    readxl = "read Excel files in R",
    data.table = "data manipulation in R",
    xts = "time-series data in R",
    tibble = "data frame with additional features",
    readr = "read data into R",
    rmarkdown = "dynamic documents in R",
    igraph = "network analysis in R",
    vars = "vector autoregression models in R",
    bsts = "Bayesian structural time series models in R",
    gtable = "tables in R",
    stargazer = "create LaTeX tables in R",
    languageserver = "language server for R",
    knitr = "dynamic reports in R",
    kableExtra = "create tables in R",
    here = "manage paths in R",
    reshape2 = "reshape data in R",
    ggthemes = "themes for ggplot2",
    ggpubr = "publication-ready plots in R",
    ggsci = "scientific journal themes for ggplot2",
    gganimate = "animations in ggplot2",
    ggforce = "add-on for ggplot2",
    priceR = "financial data in R",
    tseries = "time-series analysis in R",
    TTR = "technical trading rules in R",
    bvarsv = "Bayesian vector autoregression models in R",
    ConnectednessApproach = "connectedness measures in R",
    purrr = "functional programming in R",
    urca = "unit root and cointegration tests in R",
    psych = "psychological research in R",
    flextable = "create tables in R",
    moments = "statistical moments in R",
    doParallel = "parallel computing in R",
    orca = "plotly in R"
  )

  # Update all packages if specified
  if (update) {
    update.packages(ask = FALSE, checkBuilt = TRUE)
  }

  # Function to check, install, and load packages
  check_and_install <- function(pkg, desc) {
    if (!require(pkg, character.only = TRUE)) {
      message(paste("Installing", pkg, "for", desc))
      install.packages(pkg)
      library(pkg, character.only = TRUE)
    } else {
      message(paste("Loading", pkg, "-", desc))
      library(pkg, character.only = TRUE)
    }
  }

  # Apply the function to each package
  mapply(check_and_install, names(packages), packages)
}

# # Function to set and return working directories as a list of variables
# set_working_directories <- function() {
#   directories <- list(
#     Git = Git,
#     # ICAP_Data = paste0(Git, "/Data/ICAP"),
#     # Clearblue_Data = paste0(Git, "/Data/Clearblue Markets")
#   )

#   for (name in names(directories)) {
#     assign(name, directories[[name]], envir = .GlobalEnv)
#   }

#   return(directories)
# }

# Call the functions to load packages and set directories when this script runs
load_packages(update = FALSE)
# set_working_directories()