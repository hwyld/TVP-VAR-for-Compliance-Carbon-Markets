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

# Replication paper directory
Rep <- "C:/Users/henry/OneDrive - The University of Melbourne/GitHub/TVP-VAR-for-Compliance-Carbon-Markets/Replication Exercise"
Asym <- "C:/Users/henry/OneDrive - The University of Melbourne/GitHub/TVP-VAR-for-Compliance-Carbon-Markets/Asymmetric Connectedness"

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
return_zoo <- return_zoo[, c("EUA", "NZU", "HBEA", "CCA", "KAU", "ACCU")]
vol_zoo <- vol_zoo[, c("EUA", "NZU", "HBEA", "CCA", "KAU", "ACCU")]

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

## TVP-VAR model - Returns ##

#----------------------------------

# Specify the lag order
lag_order <- 1  # his analysis uses first-order VARs (p = 1) (selected by Schwarz information criterion), 
H <- 10 # with 10-step-ahead forecasts (H = 10).
window_size <- 200 # Window size for the rolling window estimation

# This study considers forgetting factor, kappa1=0.99 and decay factor kappa2=0.96 
# follows Antonakakis et al. to keep the decay factors constant at fixed values.
# See https://gabauerdavid.github.io/ConnectednessApproach/2020AntonakakisChatziantoniouGabauer
forgetting_factor <- 0.99
decay_factor <- 0.96

# TVP_VAR using ConnectednessApproach package
dca = ConnectednessApproach(return_zoo, 
                            nlag=lag_order, 
                            nfore=H,
                            window.size=window_size,
                            model="TVP-VAR",
                            connectedness="Time",
                            VAR_config=list(TVPVAR=list(kappa1=forgetting_factor, kappa2=decay_factor, prior="BayesPrior"))) # TVP-VAR model with forgetting factor and decay factor as specified


#----------------------------------

## EXTRACT DATA ##

#----------------------------------
# Extract the connectedness measures
TCI_return <- as.data.frame(dca$TCI)
TCI_return$Date <- as.Date(row.names(TCI_return))
to_return <- as.data.frame(dca$TO)
from_return  <- as.data.frame(dca$FROM)
NET_return <- as.data.frame(dca$NET)

#----------------------------------

## Total Connectedness Index - TCI ##

#----------------------------------

# Set the working directory to Rep to ensure all outputs go there
setwd(Rep)

# Start PDF device before creating the plot
pdf(file.path(Rep, "TCI_returns.pdf"), width = 8, height = 6)  # Size in inches (default)

# Set larger margins (bottom, left, top, right) to avoid clipping of titles/labels
par(mar=c(5, 5.5, 4, 2) + 0.1)  # Increased left margin

# Plot TCI data with adjusted limits and margins
PlotTCI(dca, 
        ca = NULL,
        ylim = c(-100, 100))

# Close the device and save the plot
dev.off()

#----------------------------------

## Dynamic directional spillovers TO markets ##

#----------------------------------

# Start PDF device before creating the plot
pdf(file.path(Rep, "TO_returns.pdf"), width = 8, height = 6)  # Size in inches (default)

# Set larger margins (bottom, left, top, right) to avoid clipping of titles/labels
par(mar=c(10, 4.5, 5, 2) + 0.1)  # Adjust these numbers as needed

PlotTO(dca, ylim = c(-100, 100))

# Close the device and save the plot
dev.off()

#----------------------------------

## Dynamic directional spillovers FROM markets ##

#----------------------------------

# Start PDF device before creating the plot
pdf(file.path(Rep, "FROM_returns.pdf"), width = 8, height = 6)  # Size in inches (default)

# Set larger margins (bottom, left, top, right) to avoid clipping of titles/labels
par(mar=c(10, 4.5, 5, 2) + 0.1)  # Adjust these numbers as needed

PlotFROM(dca, ylim = c(0, 50))  # Adjust these numbers as needed

# Close the device and save the plot
dev.off()

#----------------------------------

## Net Total Directional Connectedness - NET ##

#----------------------------------

# Start PDF device with adjusted height for more header space
pdf(file.path(Rep, "NET_returns.pdf"), width = 8, height = 6)

# Plot the connectedness measures - Net Pairwise Total Connectedness
PlotNET(dca, ylim = c(-30, 30), mar = c(5, 4, 6, 2), oma = c(0, 0, 4, 0))

# Close the PDF device
dev.off()

#----------------------------------

## Forecast Error Variance Decomposition (FEVD) ##

#----------------------------------
# The average connectedness matrix of the system is calculated as the average of the connectedness matrices over the entire sample period.

# Forecast Error Variance Decomposition (FEVD)
FEVD_returns <- dca$TABLE

# Remove the rows named "Inc.Own" and "NPT"
FEVD_returns <- FEVD_returns[!(rownames(FEVD_returns) %in% c("Inc.Own", "NPT")), ]

# Ensure FEVD_returns is a data frame
FEVD_returns <- as.data.frame(FEVD_returns)

#----------------------------------

## TVP-VAR model - Volatility ##

#----------------------------------

dca = ConnectednessApproach(vol_zoo, 
                            nlag=lag_order, 
                            nfore=H,
                            window.size=200,
                            model="TVP-VAR",
                            connectedness="Time",
                            VAR_config=list(TVPVAR=list(kappa1=forgetting_factor, kappa2=decay_factor, prior="BayesPrior"))) # TVP-VAR model with forgetting factor and decay factor as specified


## EXTRACT DATA ##

#----------------------------------

# Extract the connectedness measures
TCI_vol <- as.data.frame(dca$TCI)
TCI_vol$Date <- as.Date(row.names(TCI_vol))
to_vol <- as.data.frame(dca$TO)
from_vol  <- as.data.frame(dca$FROM)
NET_vol <- as.data.frame(dca$NET)

#----------------------------------

## Total Connectedness Index - TCI ##

#----------------------------------

# Start PDF device before creating the plot
pdf(file.path(Rep, "TCI_vol.pdf"), width = 8, height = 6)  # Size in inches (default)

# Set larger margins (bottom, left, top, right) to avoid clipping of titles/labels
par(mar=c(5, 5.5, 4, 2) + 0.1)  # Increased left margin

# Plot TCI data with adjusted limits and margins
PlotTCI(dca, 
        ca = NULL,
        ylim = c(0, 50))

# Close the device and save the plot
dev.off()

#----------------------------------

## Dynamic directional spillovers TO markets ##

#----------------------------------

# Start PDF device before creating the plot
pdf(file.path(Rep, "TO_vol.pdf"), width = 8, height = 6)  # Size in inches (default)

# Set larger margins (bottom, left, top, right) to avoid clipping of titles/labels
par(mar=c(10, 4.5, 5, 2) + 0.1)  # Adjust these numbers as needed

PlotTO(dca, ylim = c(0, 70))

# Close the device and save the plot
dev.off()

#----------------------------------

## Dynamic directional spillovers FROM markets ##

#----------------------------------

# Start PDF device before creating the plot
pdf(file.path(Rep, "FROM_vol.pdf"), width = 8, height = 6)  # Size in inches (default)

# Set larger margins (bottom, left, top, right) to avoid clipping of titles/labels
par(mar=c(10, 4.5, 5, 2) + 0.1)  # Adjust these numbers as needed

PlotFROM(dca, ylim = c(0, 50))  # Adjust these numbers as needed

# Close the device and save the plot
dev.off()

#----------------------------------

## Net Total Directional Connectedness - NET ##

#----------------------------------

# Start PDF device with adjusted height for more header space
pdf(file.path(Rep, "NET_vol.pdf"), width = 8, height = 6)

# Plot the connectedness measures - Net Pairwise Total Connectedness
PlotNET(dca, ylim = c(-50, 40), mar = c(5, 4, 6, 2), oma = c(0, 0, 4, 0))

# Close the PDF device
dev.off()

#----------------------------------

## Forecast Error Variance Decomposition (FEVD) ##

#----------------------------------
# The average connectedness matrix of the system is calculated as the average of the connectedness matrices over the entire sample period.

# Forecast Error Variance Decomposition (FEVD)
FEVD_vol <- dca$TABLE

# Remove the rows named "Inc.Own" and "NPT"
FEVD_vol <- FEVD_vol[!(rownames(FEVD_vol) %in% c("Inc.Own", "NPT")), ]

# Ensure FEVD_vol is a data frame
FEVD_vol <- as.data.frame(FEVD_vol)

# Put the both FEVD dataframes into a single stargazer table
stargazer::stargazer(FEVD_returns, type = "html", summary = FALSE, 
                     title = "Table 3. Average connectedness matrix of the system.", 
                     out = file.path(Rep, "FEVD_returns.html"), out.header = FALSE)

stargazer::stargazer(FEVD_vol, type = "html", summary = FALSE, 
                     title = "", 
                     out = file.path(Rep, "FEVD_volatility.html"), out.header = FALSE)

# Combine HTML files
html_lines_returns <- readLines(file.path(Rep, "FEVD_returns.html"))
html_lines_volatility <- readLines(file.path(Rep, "FEVD_volatility.html"))

# Insert the caption for returns
caption_returns <- '<caption style="text-align:left;">Panel A: Return connectedness (%)</caption>'
table_start_index_returns <- which(grepl("<table", html_lines_returns, fixed = TRUE))[1]
html_lines_returns <- append(html_lines_returns, caption_returns, after = table_start_index_returns)

# Insert the caption for volatility
caption_volatility <- '<caption style="text-align:left;">Panel B: Volatility connectedness (%)</caption>'
table_start_index_volatility <- which(grepl("<table", html_lines_volatility, fixed = TRUE))[1]
html_lines_volatility <- append(html_lines_volatility, caption_volatility, after = table_start_index_volatility)

# Combine both HTML tables into a single HTML file
combined_html_lines <- c(html_lines_returns, "<br><br>", html_lines_volatility)

# Add final caption at the bottom
caption_connectedness <- '<tr><td colspan="7" style="text-align:left;">Aligning with Lyu and Scholtens (2022), this analysis uses first-order VARs (p = 1) as selected by Schwarz information criterion, with 10-step-ahead forecasts (H = 10).</td></tr>'
table_end_index_volatility <- which(grepl("</table>", html_lines_volatility, fixed = TRUE))[1]
combined_html_lines <- append(combined_html_lines, caption_connectedness, after = length(combined_html_lines) - (length(html_lines_volatility) - table_end_index_volatility))

# Write the combined HTML content back to a new file
writeLines(combined_html_lines, file.path(Rep, "combined_connectedness.html"))

#----------------------------------

# Plot TCI data with event study window

#----------------------------------

# Define custom colors for each category (adjust these to match the chart)
category_colors <- c(
        "global politics" = "orange", 
        "carbon market" = "red", 
        "weather" = "gold",
        "energy" = "blue",
        "finance" = "blue",
        "covid-19" = "purple")

# Add an EventNumber column to the TCI dataframe mapping the event number to repeat between the StartDate and EndDate
events_study_df$EndDate <- pmin(events_study_df$EndDate, max(TCI_return$Date))
events_study_df$StartDate <- pmax(events_study_df$StartDate, min(TCI_return$Date))
events_study_df <- events_study_df %>%
  mutate(
    Midpoint = as.Date((as.numeric(StartDate) + as.numeric(EndDate)) / 2, origin = "1970-01-01"),
    LabelY = rep(seq(35, 40, length.out = n()), length.out = n())  # Alternate y positions
  )

# Create the plot
ggplot() + 
  geom_rect(data = events_study_df, aes(xmin = pmax(StartDate, min(TCI_return$Date)), xmax = pmin(EndDate, max(TCI_return$Date)), ymin = 0, ymax = 40, fill = Category), alpha = 0.2) +  
  geom_segment(data = events_study_df, aes(x = StartDate, xend = StartDate, y = 0, yend = 40), linetype = "solid", color = "grey", size = 0.25, alpha = 0.25) +  
  geom_segment(data = events_study_df, aes(x = EndDate, xend = EndDate, y = 0, yend = 40), linetype = "solid", color = "grey", size = 0.25, alpha = 0.25) +  
  geom_text(data = events_study_df, aes(x = Midpoint, y = LabelY, label = EventNumber), angle = 90, vjust = 0.5, hjust = 0) +  
  geom_ribbon(data = TCI_return, aes(x = Date, ymin = 0, ymax = TCI), fill = "darkgrey", alpha = 1) +  
  geom_line(data = TCI_return, aes(x = Date, y = TCI), color = "black", size = 0.5) +  
  labs(x = "year", y = "Total spillover index", title = "Total Return Connectedess Event Study") +
  theme_minimal() +  
  scale_fill_manual(values = category_colors, name = "Category") +  
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom",
    panel.grid = element_blank(),
    axis.line.x.bottom = element_line(color = "black", size = 1),
    axis.line.y.left = element_line(color = "black", size = 1)
  ) +
  scale_x_date(limits = c(min(TCI_return$Date), max(TCI_return$Date)), date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 40))

# Save the plot as a PNG with white background
ggsave(file.path(Rep, "TCI_returns_with_events.png"), width = 8, height = 6, dpi = 300, bg = "white")

#----------------------------------

# Plot TCI data with event study window for volatility

#----------------------------------

# Create the plot
ggplot() + 
  geom_rect(data = events_study_df, aes(xmin = pmax(StartDate, min(TCI_vol$Date)), xmax = pmin(EndDate, max(TCI_vol$Date)), ymin = 0, ymax = 40, fill = Category), alpha = 0.2) +  
  geom_segment(data = events_study_df, aes(x = StartDate, xend = StartDate, y = 0, yend = 40), linetype = "solid", color = "grey", size = 0.25, alpha = 0.25) +  
  geom_segment(data = events_study_df, aes(x = EndDate, xend = EndDate, y = 0, yend = 40), linetype = "solid", color = "grey", size = 0.25, alpha = 0.25) +  
  geom_text(data = events_study_df, aes(x = Midpoint, y = LabelY, label = EventNumber), angle = 90, vjust = 0.5, hjust = 0) +  
  geom_ribbon(data = TCI_vol, aes(x = Date, ymin = 0, ymax = TCI), fill = "black", alpha = 1) +  
  geom_line(data = TCI_vol, aes(x = Date, y = TCI), color = "black", size = 1) +  
  labs(x = "year", y = "Total spillover index", title = "Total Volatility Connectedess Event Study") +
  theme_minimal() +  
  scale_fill_manual(values = category_colors, name = "Category") +  
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom",
    panel.grid = element_blank(),
    axis.line.x.bottom = element_line(color = "black", size = 1),
    axis.line.y.left = element_line(color = "black", size = 1)
  ) +
  scale_x_date(limits = c(min(TCI_vol$Date), max(TCI_vol$Date)), date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 40))

# Save the plot as a PNG with white background
ggsave(file.path(Rep, "TCI_volatility_with_events.png"), width = 8, height = 6, dpi = 300, bg = "white")

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

# Function to run TVP-VAR, save FEVD, and generate plots
run_and_save_tvp_var <- function(asym_series, suffix) {
  forgetting_factor_asym <- 0.99
  decay_factor_asym <- 0.99
  lag_order <- 1
  H <- 10
  window_size <- 200

  DCA = list()
  spec = c("all", "positive", "negative")
  
  FEVD_list <- list()  # Initialize list to store FEVDs
  
  for (i in 1:length(asym_series)) {
    DCA[[i]] = suppressMessages(ConnectednessApproach(asym_series[[i]], 
                                model = "TVP-VAR",
                                connectedness = "Time",
                                nlag = lag_order,
                                nfore = H,
                                window.size = window_size,
                                VAR_config = list(TVPVAR = list(kappa1 = forgetting_factor_asym, kappa2 = decay_factor_asym, prior = "MinnesotaPrior", gamma = 0.1))))
    
    FEVD_list[[spec[i]]] <- DCA[[i]]$TABLE  # Store the FEVD for each specification
  }

  ## Save FEVD tables ##
  for (i in 1:length(FEVD_list)) {
    FEVD_df <- FEVD_list[[spec[i]]]
    # Remove rows like "Inc.Own" and "NPT"
    FEVD_df <- FEVD_df[!(rownames(FEVD_df) %in% c("Inc.Own", "NPT")), ]
    # Save to HTML file
    stargazer::stargazer(FEVD_df, type = "html", summary = FALSE, 
                         title = paste("FEVD -", spec[i], suffix), 
                         out = file.path(Asym, paste0("FEVD_", spec[i], "_", suffix, ".html")))
  }

  ## Plots ##
  # Total Connectedness Index (TCI)
  pdf(file.path(Asym, paste0("TCI_Asymmetric_", suffix, ".pdf")), width = 8, height = 6)
  PlotTCI(DCA[[1]], ca = list(DCA[[2]], DCA[[3]]), ylim = c(-100, 100))
  dev.off()

  # Dynamic directional spillovers TO markets
  pdf(file.path(Asym, paste0("TO_Asymmetric_", suffix, ".pdf")), width = 8, height = 6)
  PlotTO(DCA[[1]], ca = list(DCA[[2]], DCA[[3]]), ylim = c(0, 100))
  dev.off()

  # Dynamic directional spillovers FROM markets
  pdf(file.path(Asym, paste0("FROM_Asymmetric_", suffix, ".pdf")), width = 8, height = 6)
  PlotFROM(DCA[[1]], ca = list(DCA[[2]], DCA[[3]]), ylim = c(0, 100))
  dev.off()

  # Net Total Directional Connectedness
  pdf(file.path(Asym, paste0("NET_Asymmetric_", suffix, ".pdf")), width = 8, height = 6)
  PlotNET(DCA[[1]], ca = list(DCA[[2]], DCA[[3]]), ylim = c(-50, 50))
  dev.off()
  
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

# Save the TCI_asym_return dataframe to a CSV file
write.csv(TCI_asym_return, file.path(Asym, "TCI_asym_return.csv"), row.names = FALSE)


#----------------------------------

## Event Study Window Plots ##

#----------------------------------

# Define custom colors for each category (adjust these to match the chart)
# This creates a color scheme for the different event categories in the event study.
category_colors <- c(
        "global politics" = "orange", 
        "carbon market" = "red", 
        "weather" = "gold",
        "energy" = "blue",
        "finance" = "blue",
        "covid-19" = "purple"
)

# Add an EventNumber column to the TCI dataframe mapping the event number to repeat between the StartDate and EndDate
# Here, we are adjusting the event study dates to ensure they fall within the TCI data's date range.
events_study_df$EndDate <- pmin(events_study_df$EndDate, max(TCI_asym_return$Date))
events_study_df$StartDate <- pmax(events_study_df$StartDate, min(TCI_asym_return$Date))

# Calculating the midpoint of the event window for labeling and alternating label positions.
events_study_df <- events_study_df %>%
  mutate(
    Midpoint = as.Date((as.numeric(StartDate) + as.numeric(EndDate)) / 2, origin = "1970-01-01"),  # Midpoint for event label positioning
    LabelY = rep(seq(30, 50, length.out = n()), length.out = n())  # Alternating y positions for labels to avoid overlap
  )

# Create the plot
ggplot() + 
  # This adds shaded areas (rectangles) representing the time periods of different events in the event study.
  geom_rect(data = events_study_df, aes(xmin = pmax(StartDate, min(TCI_asym_return$Date)), xmax = pmin(EndDate, max(TCI_asym_return$Date)), ymin = 0, ymax = 50, fill = Category), alpha = 0.2) +  
  
  # Adding vertical lines at the StartDate and EndDate of each event.
  geom_segment(data = events_study_df, aes(x = StartDate, xend = StartDate, y = 0, yend = 50), linetype = "solid", color = "grey", size = 0.25, alpha = 0.25) +  
  geom_segment(data = events_study_df, aes(x = EndDate, xend = EndDate, y = 0, yend = 50), linetype = "solid", color = "grey", size = 0.25, alpha = 0.25) +  
  
  # Placing event numbers at the midpoint of the event window for easier identification.
  geom_text(data = events_study_df, aes(x = Midpoint, y = LabelY, label = EventNumber), angle = 90, vjust = 0.5, hjust = 0) +  
  
  # Drawing a shaded area under the 'All' line representing the total connectedness index for all events.
  geom_ribbon(data = TCI_asym_return, aes(x = Date, ymin = 0, ymax = all), fill = "lightgrey", alpha = 1) +  
  
  # Plotting the line for the 'All' series from the TCI asymmetric return data.
  geom_line(data = TCI_asym_return, aes(x = Date, y = all, color = "All"), size = 0.8) +
  
  # Plotting the line for the 'Positive' series with a dashed line.
  geom_line(data = TCI_asym_return, aes(x = Date, y = Positive, color = "Positive"), linetype = "dashed", size = 0.8) +
  
  # Plotting the line for the 'Negative' series with a dotted line.
  geom_line(data = TCI_asym_return, aes(x = Date, y = Negative, color = "Negative"), linetype = "dotted", size = 0.8) +
  
  # Adding labels and title to the plot.
  labs(x = "Year", y = "Total Connectedness Index (TCI)", title = "Total Asymmetric Connectedness Event Study" )  +
  
  # Applying a minimal theme to the plot for a cleaner look.
  theme_minimal() +  
  
  # Assigning custom fill colors to the event categories.
  scale_fill_manual(values = category_colors, name = "Category") +  
  
  # Assigning custom colors to the 'All', 'Positive', and 'Negative' lines.
  scale_color_manual(values = c("All" = "black", "Positive" = "blue", "Negative" = "red")) +
  
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
  scale_y_continuous(limits = c(0, 50))

# Save the plot as a PNG
ggsave(file.path(Asym, paste0("TCI_Asymmetric_with_events_r", ".png")), width = 8, height = 6, dpi = 300, bg = "white")

#------------------------------------------------------------------------