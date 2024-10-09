### Econometrics Research Project , Semester 2, 2024
## The pursuit of a global carbon market; A time varying analysis of international compliance carbon markets
## Asymmetric TVP-VAR model estimation procedures in R using the ConnectednessApproach package
## Author: Henry Wyld
## Date of creation: 2024-09-07

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

# Replication paper directory
# Asym <- "C:/Users/henry/OneDrive - The University of Melbourne/GitHub/TVP-VAR-for-Compliance-Carbon-Markets/Asymmetric Connectedness Bayes"
Asym <- "C:/Users/henry/OneDrive - The University of Melbourne/GitHub/TVP-VAR-for-Compliance-Carbon-Markets/Asymmetric Connectedness Minnesota"
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

# BayesPrior
# more appropriate for capturing large swings in connectedness, particularly around extreme events
# weaker restrictions  allow the model to capture these irregularities and extreme behavior more naturally. 
# It offers flexibility in capturing shifts in connectedness caused by large market moves or political events.   

# PriorChoice =list(TVPVAR=list(kappa1=forgetting_factor, kappa2=decay_factor, prior="BayesPrior"))

# MinnesotaPrior

# Prior shrinks the parameters toward an assumption that each market is primarily driven by its own dynamics rather than by shocks from other markets
# The Minnesota prior works well in cases where theoretical or empirical justification supports weak relationships between variables. 

PriorChoice = list(TVPVAR = list(kappa1 = forgetting_factor_asym, kappa2 = decay_factor_asym, prior="MinnesotaPrior", gamma=gamma.setting)) # Gamma is the shrinkage parameter for the Minnesota prior based on Antonakakis et al. (2020)

# Function to run TVP-VAR, save FEVD, and generate plots
run_and_save_tvp_var <- function(asym_series, suffix) {

  DCA = list()
  spec = c("All", "Positive", "Negative")
  
  FEVD_list <- list()  # Initialize list to store FEVDs
  
  # Run the ConnectednessApproach for each series and store the results
  for (i in 1:length(asym_series)) {
    DCA[[i]] = suppressMessages(ConnectednessApproach(
      asym_series[[i]], 
      model = "TVP-VAR",
      connectedness = "Time",
      nlag = lag_order,
      nfore = H,
      window.size = window_size,
      VAR_config = PriorChoice  # PriorChoice defined externally
    ))
    
    FEVD_list[[spec[i]]] <- DCA[[i]]$TABLE  # Store the FEVD for each specification
  }

  # Save the information criteria to an html file
  #stargazer::stargazer(DCA[[1]]$IC, type = "html", summary = FALSE, title = "Information Criteria", out = file.path(Asym, paste0("IC_", suffix, ".html")))

  ## Save FEVD tables ##
  for (i in 1:length(FEVD_list)) {
    FEVD_df <- FEVD_list[[spec[i]]]
    # Remove rows like "Inc.Own" and "NPT"
    FEVD_df <- FEVD_df[!(rownames(FEVD_df) %in% c("Inc.Own", "NPT")), ]
    
    # Save to HTML file using stargazer
    stargazer::stargazer(FEVD_df, type = "html", summary = FALSE, 
                         title = paste("FEVD -", spec[i], suffix), 
                         out = file.path(Asym, paste0("FEVD_", spec[i], "_", suffix, ".html")))
  }

  ## Combine HTML files ##
  html_lines <- list()
  for (i in 1:length(FEVD_list)) {
    html_lines[[i]] <- readLines(file.path(Asym, paste0("FEVD_", spec[i], "_", suffix, ".html")))
  }

  # Insert captions for each specification
  captions <- c(
    '<caption style="text-align:left;">Panel A: All connectedness (%)</caption>',
    '<caption style="text-align:left;">Panel B: Positive connectedness (%)</caption>',
    '<caption style="text-align:left;">Panel C: Negative connectedness (%)</caption>'
  )

  for (i in 1:length(html_lines)) {
    table_start_index <- which(grepl("<table", html_lines[[i]], fixed = TRUE))[1]
    html_lines[[i]] <- append(html_lines[[i]], captions[i], after = table_start_index)
  }

  # Combine HTML tables
  combined_html_lines <- c(html_lines[[1]], "<br><br>", html_lines[[2]], "<br><br>", html_lines[[3]])

  # Add final caption at the bottom
  caption_connectedness <- '<tr><td colspan="7" style="text-align:left;">Aligning with Adekoya et al. (2021), this analysis uses first-order VARs (p = 1) as selected by Schwarz information criterion, with 10-step-ahead forecasts and a Minnesota Prior.</td></tr>'
  # caption_connectedness <- '<tr><td colspan="7" style="text-align:left;">Aligning with Antonakakis et al. (2020), this analysis uses first-order VARs (p = 1) as selected by Schwarz information criterion, with 10-step-ahead forecasts and a Bayes Prior.</td></tr>'

  # Insert final caption
  table_end_index <- which(grepl("</table>", combined_html_lines, fixed = TRUE))[length(which(grepl("</table>", combined_html_lines, fixed = TRUE)))]
  combined_html_lines <- append(combined_html_lines, caption_connectedness, after = table_end_index)

  # Write combined HTML to file
  writeLines(combined_html_lines, file.path(Asym, paste0("FEVD_Combined_", suffix, ".html")))

  # Write combined HTML to a table in the R Markdown file
  cat("```{r, echo=FALSE}\n", file = file.path(Asym, paste0("FEVD_Combined_", suffix, ".Rmd")), append = TRUE)

  ## Plots ##
  # Total Connectedness Index (TCI)
  png(file.path(Asym, paste0("TCI_Asymmetric_", suffix, ".png")), width = 800, height = 600)
  PlotTCI(DCA[[1]], ca = list(DCA[[2]], DCA[[3]]), ylim = c(0, 100))
  dev.off()

  # Dynamic directional spillovers TO markets
  png(file.path(Asym, paste0("TO_Asymmetric_", suffix, ".png")), width = 800, height = 600)
  PlotTO(DCA[[1]], ca = list(DCA[[2]], DCA[[3]]), ylim = c(0, 100))
  dev.off()

  # Dynamic directional spillovers FROM markets
  png(file.path(Asym, paste0("FROM_Asymmetric_", suffix, ".png")), width = 800, height = 600)
  PlotFROM(DCA[[1]], ca = list(DCA[[2]], DCA[[3]]), ylim = c(0, 100))
  dev.off()

  # Net Total Directional Connectedness
  png(file.path(Asym, paste0("NET_Asymmetric_", suffix, ".png")), width = 800, height = 600)
  PlotNET(DCA[[1]], ca = list(DCA[[2]], DCA[[3]]), ylim = c(-100, 100))
  dev.off()
  
  # Dynamic influence connectedness plot
  png(file.path(Asym, paste0("INF_Asymmetric_", suffix, ".png")), width = 800, height = 600)
  PlotINF(DCA[[1]], ca = list(DCA[[2]], DCA[[3]]), ylim = c(-100, 100))
  dev.off()  

    # Dynamic pairwise connectedness plot
  png(file.path(Asym, paste0("PCI_Asymmetric_", suffix, ".png")), width = 800, height = 600)
  PlotPCI(DCA[[1]], ca = list(DCA[[2]], DCA[[3]]), ylim = c(-100, 100))
  dev.off()  

  # Save the prior 
  prior <- MinnesotaPrior(gamma = gamma.setting, k = ncol(asym_series[[1]]), nlag = lag_order)
  prior.bayes <- BayesPrior(return_zoo, size = window_size, nlag = lag_order)
  prior.bayes.unrestricted <- BayesPrior(return_zoo, nlag = lag_order)

  # Convert the prior matrix to a data frame
  prior <- as.data.frame(prior)
  prior.bayes <- as.data.frame(prior.bayes)
  prior.bayes.unrestricted <- as.data.frame(prior.bayes.unrestricted)
  
  # Save the prior matrix to a CSV file
  write.csv(prior, file.path(Asym, paste0("Prior_", suffix, ".csv")), row.names = FALSE)
  write.csv(prior.bayes, file.path(Asym, paste0("Prior_Bayes_", suffix, ".csv")), row.names = FALSE)
  write.csv(prior.bayes.unrestricted, file.path(Asym, paste0("Prior_Bayes_Unrestricted_", suffix, ".csv")), row.names = FALSE)

  return(DCA)
}

# Run TVP-VAR and generate plots for both returns and volatility
DCA_return <- run_and_save_tvp_var(asym_return, "r")
#DCA_vol <- run_and_save_tvp_var(asym_vol, "v")
# Save the TCI series from DCA_return for use in chart below, create a dataframe for the event study
TCI_asym_return <- as.data.frame(DCA_return[[1]]$TCI)

# Name the column "all"
colnames(TCI_asym_return) <- "all"

# Add DCA_return[[2]]$TCI as Positive and DCA_return[[3]]$TCI as Negative
TCI_asym_return$Positive <- DCA_return[[2]]$TCI
TCI_asym_return$Negative <- DCA_return[[3]]$TCI

# Add the Date column
TCI_asym_return$Date <- as.Date(rownames(TCI_asym_return))

# Create a series called net by subtracting the Negative series from the Positive series
TCI_asym_return$Net <- TCI_asym_return$Negative - TCI_asym_return$Positive

# Move the Date column to the front
TCI_asym_return <- TCI_asym_return[, c("Date", "all", "Positive", "Negative", "Net")]

# Save the TCI_asym_return dataframe to a CSV file
write.csv(TCI_asym_return, file.path(Asym, "TCI_asym_return.csv"), row.names = FALSE)

# Plot net, positive and negative connectedness series
ggplot(TCI_asym_return, aes(x = Date)) +
  geom_line(aes(y = Net, color = "Net"), size = 1) +
  geom_line(aes(y = Positive, color = "Positive"), size = 1) +
  geom_line(aes(y = Negative, color = "Negative"), size = 1) +
  labs(x = "Year", y = "Total Connectedness Index (TCI)", title = "Total Asymmetric Connectedness Series") +
  theme_minimal() +
  scale_color_manual(values = c("Net" = "black", "Positive" = "blue", "Negative" = "dark red")) +
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom",
    panel.grid.major = element_line(color = "grey80", size = 0.5),  # Add major gridlines
    panel.grid.minor = element_line(color = "grey90", size = 0.25),  # Add minor gridlines
    axis.line.x.bottom = element_line(color = "black", size = 1),
    axis.line.y.left = element_line(color = "black", size = 1)
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")

#----------------------------------

## Event Study Window Plots - all, positive, negative ##

#----------------------------------

# Define custom colors for each category (adjust these to match the chart)
# This creates a color scheme for the different event categories in the event study.
category_colors <- c(
        "global politics" = "orange", 
        "carbon market" = "red", 
        "weather" = "gold",
        "energy" = "green",
        "finance" = "blue",
        "covid-19" = "purple"
)

max.index <- 100

# Add an EventNumber column to the TCI dataframe mapping the event number to repeat between the StartDate and EndDate
# Here, we are adjusting the event study dates to ensure they fall within the TCI data's date range.
events_study_df$EndDate <- pmin(events_study_df$EndDate, max(TCI_asym_return$Date))
events_study_df$StartDate <- pmax(events_study_df$StartDate, min(TCI_asym_return$Date))

# Calculating the midpoint of the event window for labeling and alternating label positions.
events_study_df <- events_study_df %>%
  mutate(
    Midpoint = as.Date(Midpoint , origin = "1970-01-01"),  # Midpoint for event label positioning
    LabelY = c(seq(max.index, 20, length.out = ceiling(n() / 2)), seq(20, max.index, length.out = floor(n() / 2)))  # First half seq(50, 25), second half seq(25, 50)
  )

# Create the plot
ggplot() + 
  # This adds shaded areas (rectangles) representing the time periods of different events in the event study.
  geom_rect(data = events_study_df, aes(xmin = pmax(StartDate, min(TCI_asym_return$Date)), xmax = pmin(EndDate, max(TCI_asym_return$Date)), ymin = 0, ymax = max.index, fill = Category), alpha = 0.2) +  
  
  # Adding vertical lines at the StartDate and EndDate of each event.
  geom_segment(data = events_study_df, aes(x = StartDate, xend = StartDate, y = 0, yend = max.index), linetype = "solid", color = "grey", size = 0.25, alpha = 0.25) +  
  geom_segment(data = events_study_df, aes(x = EndDate, xend = EndDate, y = 0, yend = max.index), linetype = "solid", color = "grey", size = 0.25, alpha = 0.25) +  
  
  # Drawing a shaded area under the 'All' line representing the total connectedness index for all events.
  geom_ribbon(data = TCI_asym_return, aes(x = Date, ymin = 0, ymax = all), fill = "lightgrey", alpha = 1) +  
  
  # Plotting the line for the 'All' series from the TCI asymmetric return data.
  geom_line(data = TCI_asym_return, aes(x = Date, y = all, color = "All"), size = 0.8) +
  
  # Plotting the line for the 'Positive' series with a dashed line.
  geom_line(data = TCI_asym_return, aes(x = Date, y = Positive, color = "Positive"), linetype = "dashed", size = 0.8) +
  
  # Plotting the line for the 'Negative' series with a dashed line.
  geom_line(data = TCI_asym_return, aes(x = Date, y = Negative, color = "Negative"), linetype = "dashed", size = 0.8) +

  # Placing event numbers at the midpoint of the event window for easier identification.
  geom_text(data = events_study_df, aes(x = Midpoint, y = LabelY, label = EventNumber), angle = 90, vjust = 0.5, hjust = 0) +  
  
  # Adding labels and title to the plot.
  labs(x = "Year", y = "Total Connectedness Index (TCI)", title = "Total Asymmetric Connectedness Event Study" )  +
  
  # Applying a minimal theme to the plot for a cleaner look.
  theme_minimal() +  
  
  # Assigning custom fill colors to the event categories.
  scale_fill_manual(values = category_colors, name = "Category") +  
  
  # Assigning custom colors to the 'All', 'Positive', and 'Negative' lines.
  scale_color_manual(values = c("All" = "black", "Positive" = "blue", "Negative" = "dark red")) +
  
  # Customizing the theme for the plot.
  theme(
    axis.title.x = element_text(size = 12),  # X-axis title size
    axis.title.y = element_text(size = 12),  # Y-axis title size
    axis.text = element_text(size = 10),  # Axis text size
    plot.title = element_text(size = 14, face = "bold"),  # Title size and bold
    legend.position = "bottom",  # Legend position
    panel.grid = element_blank(),  # Removing all gridlines
    axis.line.x.bottom = element_line(color = "black", size = 1),  # Adding a line to the bottom of the x-axis
    axis.line.y.left = element_line(color = "black", size = 1)  # Adding a line to the left of the y-axis
  ) +
  
  # Setting the x-axis and y-axis limits and formats.
  scale_x_date(limits = c(min(TCI_asym_return$Date), max(TCI_asym_return$Date)), date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, max.index))

# Save the plot as a PNG
ggsave(file.path(Asym, paste0("TCI_Asymmetric_with_events_all_r", ".png")), width = 8, height = 6, dpi = 300, bg = "white")

#------------------------------------------------------------------------

#----------------------------------

## Event Study Window Plots - all, positive, negative ##

#----------------------------------

min.index <- -50
max.index <- 100

# Add an EventNumber column to the TCI dataframe mapping the event number to repeat between the StartDate and EndDate
# Here, we are adjusting the event study dates to ensure they fall within the TCI data's date range.
events_study_df$EndDate <- pmin(events_study_df$EndDate, max(TCI_asym_return$Date))
events_study_df$StartDate <- pmax(events_study_df$StartDate, min(TCI_asym_return$Date))

# Calculating the midpoint of the event window for labeling and alternating label positions.
events_study_df <- events_study_df %>%
  mutate(
    Midpoint = as.Date(Midpoint , origin = "1970-01-01"),  # Midpoint for event label positioning
    LabelY = c(seq(max.index, 20, length.out = ceiling(n() / 2)), seq(20, max.index, length.out = floor(n() / 2)))  # First half seq(50, 25), second half seq(25, 50)
  )

# Assuming TCI_asym_return is a data frame with columns Date, Net, and All

# Create the plot
ggplot() + 
  # This adds shaded areas (rectangles) representing the time periods of different events in the event study.
  geom_rect(data = events_study_df, aes(xmin = pmax(StartDate, min(TCI_asym_return$Date)), xmax = pmin(EndDate, max(TCI_asym_return$Date)), ymin = min.index, ymax = max.index, fill = Category), alpha = 0.2) +  
  
  # Adding vertical lines at the StartDate and EndDate of each event.
  geom_segment(data = events_study_df, aes(x = StartDate, xend = StartDate, y = min.index, yend = max.index), linetype = "solid", color = "grey", size = 0.25, alpha = 0.25) +  
  geom_segment(data = events_study_df, aes(x = EndDate, xend = EndDate, y = min.index, yend = max.index), linetype = "solid", color = "grey", size = 0.25, alpha = 0.25) +  
  
  # Drawing a shaded area under the 'Net' line representing the difference between the 'Net' series and the origin.
  geom_ribbon(data = TCI_asym_return, aes(x = Date, ymin = pmin(Net, 0), ymax = pmax(Net, 0)), fill = "lightgrey", alpha = 1) +  
  
  # Plotting the line for the 'Net' series from the TCI asymmetric return data.
  geom_line(data = TCI_asym_return, aes(x = Date, y = Net, color = "Net"), size = 0.8) +
  
  # Plotting the line for the 'All' series with a solid line.
  geom_line(data = TCI_asym_return, aes(x = Date, y = all, color = "All"), size = 0.8) +
  
  # Placing event numbers at the midpoint of the event window for easier identification.
  geom_text(data = events_study_df, aes(x = Midpoint, y = LabelY, label = EventNumber), angle = 90, vjust = 0.5, hjust = 0) +  

  # Adding labels and title to the plot.
  labs(x = "Year", y = "Total Connectedness Index (TCI)", title = "Total Asymmetric Connectedness Event Study") + 
  
  # Applying a minimal theme to the plot for a cleaner look.
  theme_minimal() +  
  
  # Assigning custom fill colors to the event categories.
  scale_fill_manual(values = category_colors, name = "Category") +  
  
  # Assigning custom colors to the 'All' and 'Net' lines.
  scale_color_manual(values = c("All" = "black", "Net" = "red")) +
  
  # Customizing the theme for the plot.
  theme(
    axis.title.x = element_text(size = 12),  # X-axis title size
    axis.title.y = element_text(size = 12),  # Y-axis title size
    axis.text = element_text(size = 10),  # Axis text size
    plot.title = element_text(size = 14, face = "bold"),  # Title size and bold
    legend.position = "bottom",  # Legend position
    panel.grid = element_blank(),  # Removing all gridlines
    axis.line.x.bottom = element_line(color = "black", size = 1),  # Adding a line to the bottom of the x-axis
    axis.line.y.left = element_line(color = "black", size = 1)  # Adding a line to the left of the y-axis
  ) +
  
  # Setting the x-axis and y-axis limits and formats.
  scale_x_date(limits = c(min(TCI_asym_return$Date), max(TCI_asym_return$Date)), date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(min.index, max.index))

# Save the plot as a PNG
ggsave(file.path(Asym, paste0("TCI_Asymmetric_with_events_net_r", ".png")), width = 8, height = 6, dpi = 300, bg = "white")

#------------------------------------------------------------------------

#----------------------------------

## ROBUSTNESS CHECKS ##

#----------------------------------

# Ensure the directory exists
if (!dir.exists(AsymHTesting)) {
  dir.create(AsymHTesting)
}

# Set working directory
setwd(AsymHTesting)

# List of forecast horizons to test
horizons <- c(1, 2, 5, 10)

# Initialize empty data frames to store results for each horizon
TCI_All_df <- data.frame(Date = as.Date(character()))
TCI_Net_df <- data.frame(Date = as.Date(character()))

# Function to run TVP-VAR for each horizon and save TCI results
run_tvp_var_for_horizons <- function(asym_series) {

  for (H in horizons) {
    DCA = list()
    spec = c("All", "Positive", "Negative")
    
    # Run the ConnectednessApproach for each series
    for (i in 1:length(asym_series)) {
      DCA[[i]] <- suppressMessages(ConnectednessApproach(asym_series[[i]], 
                    model = "TVP-VAR",
                    connectedness = "Time",
                    nlag = lag_order,
                    nfore = H,
                    window.size = window_size,
                    VAR_config = PriorChoice))
    }
    
    # Extract TCI series for the "All" and "Net" series
    TCI_asym_return <- as.data.frame(DCA[[1]]$TCI)
    colnames(TCI_asym_return) <- paste0("All_H", H)  # Label based on horizon
    
    # Add DCA_return[[2]]$TCI as Positive and DCA_return[[3]]$TCI as Negative
    TCI_asym_return$Positive <- DCA[[2]]$TCI
    TCI_asym_return$Negative <- DCA[[3]]$TCI
    
    # Add the Date column
    TCI_asym_return$Date <- as.Date(rownames(TCI_asym_return))
    
    # Create a series called Net by subtracting the Negative series from the Positive series
    TCI_asym_return$Net <- TCI_asym_return$Negative - TCI_asym_return$Positive
    
    # Store the "All" and "Net" series in their respective data frames
    if (nrow(TCI_All_df) == 0) {
      TCI_All_df <- TCI_asym_return[, c("Date", paste0("All_H", H))]
      TCI_Net_df <- TCI_asym_return[, c("Date", "Net")]
      colnames(TCI_Net_df)[2] <- paste0("Net_H", H)
    } else {
      TCI_All_df <- merge(TCI_All_df, TCI_asym_return[, c("Date", paste0("All_H", H))], by = "Date", all = TRUE)
      TCI_Net_df <- merge(TCI_Net_df, TCI_asym_return[, c("Date", "Net")], by = "Date", all = TRUE)
      colnames(TCI_Net_df)[ncol(TCI_Net_df)] <- paste0("Net_H", H)
    }
  }
  
  return(list(TCI_All_df = TCI_All_df, TCI_Net_df = TCI_Net_df))
}

# Run the TVP-VAR for the different forecast horizons and collect the data frames
result <- run_tvp_var_for_horizons(asym_return)

# Extract the data frames from the result list
TCI_All_df <- result$TCI_All_df
TCI_Net_df <- result$TCI_Net_df

# Save the TCI data frames to CSV files
write.csv(TCI_All_df, file.path(AsymHTesting, "TCI_All_Horizons.csv"), row.names = FALSE)
write.csv(TCI_Net_df, file.path(AsymHTesting, "TCI_Net_Horizons.csv"), row.names = FALSE)

# Pivot the data to long format for easier plotting with ggplot2
TCI_All_long <- pivot_longer(TCI_All_df, cols = starts_with("All_H"), names_to = "Horizon", values_to = "TCI")
TCI_Net_long <- pivot_longer(TCI_Net_df, cols = starts_with("Net_H"), names_to = "Horizon", values_to = "TCI")

# Plot TCI All series for each horizon
ggplot(TCI_All_long, aes(x = Date, y = TCI, color = Horizon)) +
  geom_line(size = 1) +
  labs(x = "Year", y = "Total Connectedness Index (TCI)", title = "Total Connectedness Index Across Forecast Horizons") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom",
    panel.grid.major = element_line(color = "grey80", size = 0.5),
    panel.grid.minor = element_line(color = "grey90", size = 0.25),
    axis.line.x.bottom = element_line(color = "black", size = 1),
    axis.line.y.left = element_line(color = "black", size = 1)
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")

# Save the plot
ggsave(file.path(AsymHTesting, "TCI_All_Horizons.png"), width = 8, height = 6, dpi = 300, bg = "white")

# Plot TCI Net series for each horizon
ggplot(TCI_Net_long, aes(x = Date, y = TCI, color = Horizon)) +
  geom_line(size = 1) +
  labs(x = "Year", y = "Net Connectedness Index (NegTCI - PosTCI)", title = "Net Connectedness Index Across Forecast Horizons") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom",
    panel.grid.major = element_line(color = "grey80", size = 0.5),
    panel.grid.minor = element_line(color = "grey90", size = 0.25),
    axis.line.x.bottom = element_line(color = "black", size = 1),
    axis.line.y.left = element_line(color = "black", size = 1)
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")

# Save the plot
ggsave(file.path(AsymHTesting, "TCI_Net_Horizons.png"), width = 8, height = 6, dpi = 300, bg = "white")

#----------------------------------

# Window Size Sensitivity Analysis

#----------------------------------

# Ensure the directory exists
if (!dir.exists(AsymWTesting)) {
  dir.create(AsymWTesting)
}

# Set working directory
setwd(AsymWTesting)

# List of window sizes to test
window_sizes <- c(50, 100, 200, 250)

# Initialize empty data frames to store results for each window size
TCI_All_ws_df <- data.frame(Date = as.Date(character()))
TCI_Net_ws_df <- data.frame(Date = as.Date(character()))

# Function to run TVP-VAR for each window size and save TCI results
run_tvp_var_for_window_sizes <- function(asym_series) {
    
  for (window_size in window_sizes) {
    cat("Running model with window size:", window_size, "\n")  # Debugging check to print window size
    DCA = list()
    spec = c("All", "Positive", "Negative")
    
    # Run the ConnectednessApproach for each series
    for (i in 1:length(asym_series)) {
      DCA[[i]] <- suppressMessages(ConnectednessApproach(asym_series[[i]], 
                    model = "TVP-VAR",
                    connectedness = "Time",
                    nlag = lag_order,
                    nfore = H,
                    window.size = window_size,  # Ensure window size is passed here
                    VAR_config = PriorChoice))
    }
    
    # Extract TCI series for the "All" and "Net" series
    TCI_asym_return <- as.data.frame(DCA[[1]]$TCI)
    colnames(TCI_asym_return) <- paste0("All_W", window_size)  # Label based on window size
    
    # Add DCA_return[[2]]$TCI as Positive and DCA_return[[3]]$TCI as Negative
    TCI_asym_return$Positive <- DCA[[2]]$TCI
    TCI_asym_return$Negative <- DCA[[3]]$TCI
    
    # Add the Date column
    TCI_asym_return$Date <- as.Date(rownames(TCI_asym_return))
    
    # Create a series called Net by subtracting the Negative series from the Positive series
    TCI_asym_return$Net <- TCI_asym_return$Negative - TCI_asym_return$Positive
    
    # Debugging check to print sample TCI values to verify variation
    cat("Sample TCI values for window size", window_size, ":\n")
    print(head(TCI_asym_return))
    
    # Store the "All" and "Net" series in their respective data frames
    if (nrow(TCI_All_ws_df) == 0) {
      TCI_All_ws_df <- TCI_asym_return[, c("Date", paste0("All_W", window_size))]
      TCI_Net_ws_df <- TCI_asym_return[, c("Date", "Net")]
      colnames(TCI_Net_ws_df)[2] <- paste0("Net_W", window_size)
    } else {
      TCI_All_ws_df <- merge(TCI_All_ws_df, TCI_asym_return[, c("Date", paste0("All_W", window_size))], by = "Date", all = TRUE)
      TCI_Net_ws_df <- merge(TCI_Net_ws_df, TCI_asym_return[, c("Date", "Net")], by = "Date", all = TRUE)
      colnames(TCI_Net_ws_df)[ncol(TCI_Net_ws_df)] <- paste0("Net_W", window_size)
    }
  }
  
  return(list(TCI_All_ws_df = TCI_All_ws_df, TCI_Net_ws_df = TCI_Net_ws_df))
}

# Run the TVP-VAR for the different window sizes and collect the data frames
result <- run_tvp_var_for_window_sizes(asym_return)

# Extract the data frames from the result list
TCI_All_ws_df <- result$TCI_All_ws_df
TCI_Net_ws_df <- result$TCI_Net_ws_df

# Save the TCI data frames to CSV files
write.csv(TCI_All_ws_df, file.path(AsymWTesting, "TCI_All_Window_Sizes.csv"), row.names = FALSE)
write.csv(TCI_Net_ws_df, file.path(AsymWTesting, "TCI_Net_Window_Sizes.csv"), row.names = FALSE)

# Pivot the data to long format for easier plotting with ggplot2
TCI_All_ws_long <- pivot_longer(TCI_All_ws_df, cols = starts_with("All_W"), names_to = "Window_Size", values_to = "TCI")
TCI_Net_ws_long <- pivot_longer(TCI_Net_ws_df, cols = starts_with("Net_W"), names_to = "Window_Size", values_to = "TCI")

# Plot TCI All series for each window size
ggplot(TCI_All_ws_long, aes(x = Date, y = TCI, color = Window_Size)) +
  geom_line(size = 1) +
  labs(x = "Year", y = "Total Connectedness Index (TCI)", title = "Total Connectedness Index Across Window Sizes") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom",
    panel.grid.major = element_line(color = "grey80", size = 0.5),
    panel.grid.minor = element_line(color = "grey90", size = 0.25),
    axis.line.x.bottom = element_line(color = "black", size = 1),
    axis.line.y.left = element_line(color = "black", size = 1)
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")

# Save the plot
ggsave(file.path(AsymWTesting, "TCI_All_Window_Sizes.png"), width = 8, height = 6, dpi = 300, bg = "white")

# Plot TCI Net series for each window size
ggplot(TCI_Net_ws_long, aes(x = Date, y = TCI, color = Window_Size)) +
  geom_line(size = 1) +
  labs(x = "Year", y = "Net Connectedness Index (NegTCI - PosTCI)", title = "Net Connectedness Index Across Window Sizes") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom",
    panel.grid.major = element_line(color = "grey80", size = 0.5),
    panel.grid.minor = element_line(color = "grey90", size = 0.25),
    axis.line.x.bottom = element_line(color = "black", size = 1),
    axis.line.y.left = element_line(color = "black", size = 1)
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")

# Save the plot
ggsave(file.path(AsymWTesting, "TCI_Net_Window_Sizes.png"), width = 8, height = 6, dpi = 300, bg = "white")

#----------------------------------

#----------------------------------

# Lag Order Sensitivity Analysis

#----------------------------------

# Ensure the directory exists
if (!dir.exists(AsymLagTesting)) {
  dir.create(AsymLagTesting)
}

# Set working directory
setwd(AsymLagTesting)

# List of lag orders to test
lag_orders <- c(1, 2, 3)

# Initialize empty data frames to store results for each lag order
TCI_All_lag_df <- data.frame(Date = as.Date(character()))
TCI_Net_lag_df <- data.frame(Date = as.Date(character()))

# Function to run TVP-VAR for each lag order and save TCI results
run_tvp_var_for_lag_orders <- function(asym_series) {
    
  for (lag_order in lag_orders) {
    cat("Running model with lag order:", lag_order, "\n")  # Debugging check to print lag order
    DCA = list()
    spec = c("All", "Positive", "Negative")
    
    # Run the ConnectednessApproach for each series
    for (i in 1:length(asym_series)) {
      DCA[[i]] <- suppressMessages(ConnectednessApproach(asym_series[[i]], 
                    model = "TVP-VAR",
                    connectedness = "Time",
                    nlag = lag_order,  # Ensure lag order is passed here
                    nfore = H,
                    window.size = window_size,  # Use a fixed window size
                    VAR_config = PriorChoice))
    }
    
    # Extract TCI series for the "All" and "Net" series
    TCI_asym_return <- as.data.frame(DCA[[1]]$TCI)
    colnames(TCI_asym_return) <- paste0("All_Lag", lag_order)  # Label based on lag order
    
    # Add DCA_return[[2]]$TCI as Positive and DCA_return[[3]]$TCI as Negative
    TCI_asym_return$Positive <- DCA[[2]]$TCI
    TCI_asym_return$Negative <- DCA[[3]]$TCI
    
    # Add the Date column
    TCI_asym_return$Date <- as.Date(rownames(TCI_asym_return))
    
    # Create a series called Net by subtracting the Negative series from the Positive series
    TCI_asym_return$Net <- TCI_asym_return$Negative - TCI_asym_return$Positive
    
    # Debugging check to print sample TCI values to verify variation
    cat("Sample TCI values for lag order", lag_order, ":\n")
    print(head(TCI_asym_return))
    
    # Store the "All" and "Net" series in their respective data frames
    if (nrow(TCI_All_lag_df) == 0) {
      TCI_All_lag_df <- TCI_asym_return[, c("Date", paste0("All_Lag", lag_order))]
      TCI_Net_lag_df <- TCI_asym_return[, c("Date", "Net")]
      colnames(TCI_Net_lag_df)[2] <- paste0("Net_Lag", lag_order)
    } else {
      TCI_All_lag_df <- merge(TCI_All_lag_df, TCI_asym_return[, c("Date", paste0("All_Lag", lag_order))], by = "Date", all = TRUE)
      TCI_Net_lag_df <- merge(TCI_Net_lag_df, TCI_asym_return[, c("Date", "Net")], by = "Date", all = TRUE)
      colnames(TCI_Net_lag_df)[ncol(TCI_Net_lag_df)] <- paste0("Net_Lag", lag_order)
    }
  }
  
  return(list(TCI_All_lag_df = TCI_All_lag_df, TCI_Net_lag_df = TCI_Net_lag_df))
}

# Run the TVP-VAR for the different lag orders and collect the data frames
result <- run_tvp_var_for_lag_orders(asym_return)

# Extract the data frames from the result list
TCI_All_lag_df <- result$TCI_All_lag_df
TCI_Net_lag_df <- result$TCI_Net_lag_df

# Save the TCI data frames to CSV files
write.csv(TCI_All_lag_df, file.path(AsymLagTesting, "TCI_All_Lag_Orders.csv"), row.names = FALSE)
write.csv(TCI_Net_lag_df, file.path(AsymLagTesting, "TCI_Net_Lag_Orders.csv"), row.names = FALSE)

# Pivot the data to long format for easier plotting with ggplot2
TCI_All_lag_long <- pivot_longer(TCI_All_lag_df, cols = starts_with("All_Lag"), names_to = "Lag_Order", values_to = "TCI")
TCI_Net_lag_long <- pivot_longer(TCI_Net_lag_df, cols = starts_with("Net_Lag"), names_to = "Lag_Order", values_to = "TCI")

# Plot TCI All series for each lag order
ggplot(TCI_All_lag_long, aes(x = Date, y = TCI, color = Lag_Order)) +
  geom_line(size = 1) +
  labs(x = "Year", y = "Total Connectedness Index (TCI)", title = "Total Connectedness Index Across Lag Orders") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom",
    panel.grid.major = element_line(color = "grey80", size = 0.5),
    panel.grid.minor = element_line(color = "grey90", size = 0.25),
    axis.line.x.bottom = element_line(color = "black", size = 1),
    axis.line.y.left = element_line(color = "black", size = 1)
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")

# Save the plot
ggsave(file.path(AsymLagTesting, "TCI_All_Lag_Orders.png"), width = 8, height = 6, dpi = 300, bg = "white")

# Plot TCI Net series for each lag order
ggplot(TCI_Net_lag_long, aes(x = Date, y = TCI, color = Lag_Order)) +
  geom_line(size = 1) +
  labs(x = "Year", y = "Net Connectedness Index (Net TCI)", title = "Net Connectedness Index Across Lag Orders") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom",
    panel.grid.major = element_line(color = "grey80", size = 0.5),
    panel.grid.minor = element_line(color = "grey90", size = 0.25),
    axis.line.x.bottom = element_line(color = "black", size = 1),
    axis.line.y.left = element_line(color = "black", size = 1)
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")

# Save the plot
ggsave(file.path(AsymLagTesting, "TCI_Net_Lag_Orders.png"), width = 8, height = 6, dpi = 300, bg = "white")


#----------------------------------

# Weekly return event study

#----------------------------------

# set wd
setwd(Git)

# Find the data to include only the relevant dates for the events (event 12 for COVID, event 16 for Russia-Ukraine Crisis)

Covid.StartDate <- events_study_df$StartDate[events_study_df$EventNumber == 12] - 6*5 # 6 weeks before the event
Covid.EndDate <- events_study_df$EndDate[events_study_df$EventNumber == 12] + 6*5 # 6 weeks after the event

RUC.StartDate <- events_study_df$StartDate[events_study_df$EventNumber == 16] - 6*5 # 6 weeks before the event
RUC.EndDate <- events_study_df$EndDate[events_study_df$EventNumber == 16] + 6*5 # 6 weeks after the event

# Subset the data for Covid-19 event
Covid_Data <- subset(return_df, Date >= Covid.StartDate & Date <= Covid.EndDate)

# Convert to data frame
Covid_Data <- as.data.frame(Covid_Data)

# Subset the data for Russia-Ukraine Crisis event
RUC_Data <- subset(return_df, Date >= RUC.StartDate & Date <= RUC.EndDate)

# Convert to data frame
RUC_Data <- as.data.frame(RUC_Data)

# Ensure Date column is in Date format for both datasets
Covid_Data$Date <- as.Date(Covid_Data$Date)
RUC_Data$Date <- as.Date(RUC_Data$Date)

# Melt the data for plotting
Covid_Data_long <- melt(Covid_Data, id.vars = "Date", variable.name = "Series", value.name = "Weekly_Return")
RUC_Data_long <- melt(RUC_Data, id.vars = "Date", variable.name = "Series", value.name = "Weekly_Return")

# Create the scatter plot for Covid-19 event
p1 <- ggplot(Covid_Data_long, aes(x = Date, y = Weekly_Return, color = Series)) +
  geom_point(size = 2) +
  labs(title = "Event 12 - Covid Return Study", x = "Date", y = "Weekly Return") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.position = "bottom",  # Place the legend at the bottom
    panel.grid.major = element_line(color = "grey80", size = 0.5),
    panel.grid.minor = element_line(color = "grey90", size = 0.25)
  )

# Create the scatter plot for Russia-Ukraine Crisis event
p2 <- ggplot(RUC_Data_long, aes(x = Date, y = Weekly_Return, color = Series)) +
  geom_point(size = 2) +
  labs(title = "Event 16 - Russia-Ukraine Crisis Return Study", x = "Date", y = "Weekly Return") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.position = "bottom",  # Place the legend at the bottom
    panel.grid.major = element_line(color = "grey80", size = 0.5),
    panel.grid.minor = element_line(color = "grey90", size = 0.25)
  )

# Convert the ggplot objects to plotly objects
p1_plotly <- ggplotly(p1)
p2_plotly <- ggplotly(p2)

# Export the plotly objects as HTML files
htmlwidgets::saveWidget(p1_plotly, file = file.path(Git, "Covid_Return_Study.html"))
htmlwidgets::saveWidget(p2_plotly, file = file.path(Git, "RUC_Return_Study.html"))

# Export as PNG files
ggsave(file.path(Git, "Covid_Return_Study.png"), p1, width = 8, height = 6, dpi = 300, bg = "white")
ggsave(file.path(Git, "RUC_Return_Study.png"), p2, width = 8, height = 6, dpi = 300, bg = "white")

#----------------------------------

#----------------------------------

# Connectedness Sensitivity Analysis
# Minnesota Prior Gamma Sensitivity Analysis

#----------------------------------

# Ensure the directory exists
if (!dir.exists(AsymGammaTesting)) {
  dir.create(AsymGammaTesting)
}

# Set working directory
setwd(AsymGammaTesting)

# List of gamma values to test
gamma_values <- c(0.1, 1, 2, 5, 10, 15)

# Initialize empty data frames to store results for each gamma value
TCI_All_gamma_df <- data.frame(Date = as.Date(character()))
TCI_Net_gamma_df <- data.frame(Date = as.Date(character()))

# Function to run TVP-VAR for each gamma and save TCI results
run_tvp_var_for_gamma <- function(asym_series) {
    
  for (gamma_value in gamma_values) {
    cat("Running model with gamma:", gamma_value, "\n")  # Debugging check to print gamma value

    # Update the PriorChoice with the current gamma value
    PriorChoice <- list(TVPVAR = list(kappa1 = forgetting_factor_asym, 
                                      kappa2 = decay_factor_asym, 
                                      prior = "MinnesotaPrior", 
                                      gamma = gamma_value))  # Ensure gamma is updated

    DCA = list()
    spec = c("All", "Positive", "Negative")
    
    # Run the ConnectednessApproach for each series
    for (i in 1:length(asym_series)) {
      DCA[[i]] <- suppressMessages(ConnectednessApproach(asym_series[[i]], 
                    model = "TVP-VAR",
                    connectedness = "Time",
                    nlag = lag_order,
                    nfore = H,
                    window.size = window_size,
                    VAR_config = PriorChoice))  # Pass updated PriorChoice
    }
    
    # Extract TCI series for the "All" and "Net" series
    TCI_asym_return <- as.data.frame(DCA[[1]]$TCI)
    colnames(TCI_asym_return) <- paste0("All_G", gamma_value)  # Label based on gamma
    
    # Add DCA_return[[2]]$TCI as Positive and DCA_return[[3]]$TCI as Negative
    TCI_asym_return$Positive <- DCA[[2]]$TCI
    TCI_asym_return$Negative <- DCA[[3]]$TCI
    
    # Add the Date column
    TCI_asym_return$Date <- as.Date(rownames(TCI_asym_return))
    
    # Create a series called Net by subtracting the Negative series from the Positive series
    TCI_asym_return$Net <- TCI_asym_return$Negative - TCI_asym_return$Positive
    
    # Debugging check to print sample TCI values to verify variation
    cat("Sample TCI values for gamma", gamma_value, ":\n")
    print(head(TCI_asym_return))
    
    # Store the "All" and "Net" series in their respective data frames
    if (nrow(TCI_All_gamma_df) == 0) {
      TCI_All_gamma_df <- TCI_asym_return[, c("Date", paste0("All_G", gamma_value))]
      TCI_Net_gamma_df <- TCI_asym_return[, c("Date", "Net")]
      colnames(TCI_Net_gamma_df)[2] <- paste0("Net_G", gamma_value)
    } else {
      TCI_All_gamma_df <- merge(TCI_All_gamma_df, TCI_asym_return[, c("Date", paste0("All_G", gamma_value))], by = "Date", all = TRUE)
      TCI_Net_gamma_df <- merge(TCI_Net_gamma_df, TCI_asym_return[, c("Date", "Net")], by = "Date", all = TRUE)
      colnames(TCI_Net_gamma_df)[ncol(TCI_Net_gamma_df)] <- paste0("Net_G", gamma_value)
    }
  }
  
  return(list(TCI_All_gamma_df = TCI_All_gamma_df, TCI_Net_gamma_df = TCI_Net_gamma_df))
}

# Run the TVP-VAR for the different gamma values and collect the data frames
result <- run_tvp_var_for_gamma(asym_return)

# Extract the data frames from the result list
TCI_All_gamma_df <- result$TCI_All_gamma_df
TCI_Net_gamma_df <- result$TCI_Net_gamma_df

# Save the TCI data frames to CSV files
write.csv(TCI_All_gamma_df, file.path(AsymGammaTesting, "TCI_All_Gamma_Values.csv"), row.names = FALSE)
write.csv(TCI_Net_gamma_df, file.path(AsymGammaTesting, "TCI_Net_Gamma_Values.csv"), row.names = FALSE)

# Pivot the data to long format for easier plotting with ggplot2
TCI_All_gamma_long <- pivot_longer(TCI_All_gamma_df, cols = starts_with("All_G"), names_to = "Gamma", values_to = "TCI")
TCI_Net_gamma_long <- pivot_longer(TCI_Net_gamma_df, cols = starts_with("Net_G"), names_to = "Gamma", values_to = "TCI")

# Plot TCI All series for each gamma value
ggplot(TCI_All_gamma_long, aes(x = Date, y = TCI, color = Gamma)) +
  geom_line(size = 1) +
  labs(x = "Year", y = "Total Connectedness Index (TCI)", title = "Total Connectedness Index Across Gamma Values") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom",
    panel.grid.major = element_line(color = "grey80", size = 0.5),
    panel.grid.minor = element_line(color = "grey90", size = 0.25),
    axis.line.x.bottom = element_line(color = "black", size = 1),
    axis.line.y.left = element_line(color = "black", size = 1)
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")

# Save the plot
ggsave(file.path(AsymGammaTesting, "TCI_All_Gamma_Values.png"), width = 8, height = 6, dpi = 300, bg = "white")

# Plot TCI Net series for each gamma value
ggplot(TCI_Net_gamma_long, aes(x = Date, y = TCI, color = Gamma)) +
  geom_line(size = 1) +
  labs(x = "Year", y = "Net Connectedness Index (NegTCI - PosTCI)", title = "Net Connectedness Index Across Gamma Values") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom",
    panel.grid.major = element_line(color = "grey80", size = 0.5),
    panel.grid.minor = element_line(color = "grey90", size = 0.25),
    axis.line.x.bottom = element_line(color = "black", size = 1),
    axis.line.y.left = element_line(color = "black", size = 1)
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")

# Save the plot
ggsave(file.path(AsymGammaTesting, "TCI_Net_Gamma_Values.png"), width = 8, height = 6, dpi = 300, bg = "white")




# Recreate TVP-VAR model with estimation results to prepare for confidence interval analysis

  # Save the prior 
prior <- MinnesotaPrior(gamma = gamma.setting, k = ncol(return_zoo), nlag = lag_order)
prior.bayes <- BayesPrior(return_zoo, size = window_size, nlag = lag_order)
prior.bayes.unrestricted <- BayesPrior(return_zoo, nlag = lag_order)


# Function to run TVP-VAR, save FEVD, calculate SE and CI, and generate FEVD plots with confidence intervals
run_and_save_tvp_var <- function(return_zoo, suffix) {
  
  # Initialize lists to store outputs
  fit_list <- list()   # Store the TVPVAR model fit objects
  se_list <- list()    # Store the standard errors of the coefficients
  ci_list <- list()    # Store the confidence intervals for the FEVD
  
  cat("Running TVP-VAR for return series\n")
  
  # Fit TVP-VAR model for the return series
  fit <- TVPVAR(return_zoo, configuration = list(nlag = lag_order, prior = prior, l = c(forgetting_factor, decay_factor)))
  fit_list[['return_series']] <- fit  # Store the fit object
  
  # Extract time-specific dimensions
  num_time_points <- dim(fit$B_t)[3]  # Number of time points
  num_vars <- dim(fit$B_t)[1]  # Number of variables in the VAR
  
  # Initialize matrices to store FEVD, standard errors, and confidence intervals
  fevd_matrix <- array(NA, dim = c(num_vars, num_vars, num_time_points))  # FEVD matrix
  ci_upper_matrix <- array(NA, dim = c(num_vars, num_vars, num_time_points))  # Upper confidence intervals
  ci_lower_matrix <- array(NA, dim = c(num_vars, num_vars, num_time_points))  # Lower confidence intervals
  
  # Loop over each time point to calculate FEVD, standard errors, and CIs
  for (t in 1:num_time_points) {
    cat("Calculating FEVD for time step:", t, "\n")
    
    # Extract time-specific VAR coefficients (B_t) and covariances (Q_t)
    Phi_t <- fit$B_t[, , t]
    Sigma_t <- fit$Q_t[, , t]
    
    # Compute FEVD for this time step
    fevd_t <- FEVD(Phi = Phi_t, Sigma = Sigma_t, nfore = 10, type = "time", generalized = TRUE)$FEVD
  
    # Compute the TCI for this time step
    dca = ConnectednessTable(fevd_t)

    # Save the TCI for this time step
    TCI_t <- dca$TCI

    # Store FEVD results
    fevd_matrix[, , t] <- fevd_t
    
    # Extract standard errors (posterior variances) for coefficients (B) and covariances (Q)
    se_B <- sqrt(fit$B_t[, , t])
    se_Q <- sqrt(fit$Q_t[, , t])

    # Calculate confidence intervals
    ci_upper_matrix[, , t] <- fevd_t + 1.96 * se_B  # Upper bound of CI (mean + 1.96 * SE)
    ci_lower_matrix[, , t] <- fevd_t - 1.96 * se_B  # Lower bound of CI (mean - 1.96 * SE)
  }
  
  # Store the results in lists
  se_list[['return_series']] <- se_B
  ci_list[['return_series']] <- list(lower = ci_lower_matrix, upper = ci_upper_matrix)
  
  ## FEVD Plotting with Confidence Intervals ##
  
  # Average FEVD across variables for plotting
  fevd_df <- data.frame(Date = 1:num_time_points, FEVD = apply(fevd_matrix, 3, mean))  # Take mean FEVD for each time point
  
  # Convert confidence intervals to data frames for plotting
  ci_upper_df <- data.frame(Date = 1:num_time_points, Upper_CI = apply(ci_upper_matrix, 3, mean))
  ci_lower_df <- data.frame(Date = 1:num_time_points, Lower_CI = apply(ci_lower_matrix, 3, mean))
  
  # Merge data for plotting
  plot_data <- merge(fevd_df, ci_lower_df, by = "Date")
  plot_data <- merge(plot_data, ci_upper_df, by = "Date")
  
  # Create the FEVD plot with confidence intervals using ggplot2
  p <- ggplot(plot_data, aes(x = Date, y = FEVD)) +
    geom_line(size = 1) +  # FEVD line
    geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), alpha = 0.2) +  # Confidence interval ribbon
    labs(title = paste("FEVD with Confidence Intervals - return_series"),
         x = "Time", y = "Forecast Error Variance Decomposition (FEVD)") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 14),
      legend.title = element_text(size = 16),
      legend.text = element_text(size = 14),
      legend.position = "bottom",
      panel.grid.major = element_line(color = "grey80", size = 0.5),
      panel.grid.minor = element_line(color = "grey90", size = 0.25)
    )
  
  # Save the FEVD plot as PNG
  ggsave(file.path(Asym, paste0("FEVD_with_CI_return_series_", suffix, ".png")), p, width = 10, height = 6, dpi = 300, bg = "white")
  
  return(list(fit_list = fit_list, FEVD_matrix = fevd_matrix, se_list = se_list, ci_list = ci_list))
}

# Running TVP-VAR and generating FEVD plots with confidence intervals for returns
DCA_return_fit <- run_and_save_tvp_var(return_zoo, "r")

