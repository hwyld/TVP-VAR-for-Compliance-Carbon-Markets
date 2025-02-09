### Econometrics Research Project , Semester 2, 2024
## TVP-VAR model estimation procedures in R using the ConnectednessApproach package
## Author: Henry Wyld
## Date of creation: 2024-03-20

## References
## https://sites.google.com/view/davidgabauer/econometric-code?authuser=0
## https://sites.google.com/site/fk83research/code?authuser=0

#-------------------------------------
# # clear memory
# rm(list=ls())    
# #----------------------------------

# ## Packages ##
# #----------------------------------
# # Source the package setup script
# Git <- "C:/Users/henry/OneDrive - The University of Melbourne/GitHub/TVP-VAR-for-Compliance-Carbon-Markets"
# setwd(Git)
# source("Packages.R")

# Replication paper directory
# Rep <- "C:/Users/henry/OneDrive - The University of Melbourne/GitHub/TVP-VAR-for-Compliance-Carbon-Markets/Replication Exercise/Bayes"
Rep <- "C:/Users/henry/OneDrive - The University of Melbourne/GitHub/TVP-VAR-for-Compliance-Carbon-Markets/Replication Exercise/Minnesota"

# Ensure the directory exists
if (!dir.exists(Rep)) {
  dir.create(Rep)
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

## TVP-VAR model - Returns ##

#----------------------------------

# Model Parameters

  lag_order <- 1
  H <- 10
  window_size <- 200

# TVP-VAR Parameters
# Follows Adekoya, Akinseye, Antonakakis, Chatziantoniou, Gabauer, and Oliyide (2021)
# https://gabauerdavid.github.io/ConnectednessApproach/2022Adekoya
  forgetting_factor_asym <- 0.99
  decay_factor_asym <- 0.99


# follows Antonakakis et al. to keep the decay factors constant at fixed values.
# See https://gabauerdavid.github.io/ConnectednessApproach/2020AntonakakisChatziantoniouGabauer
  forgetting_factor <- 0.99
  decay_factor <- 0.96

# BayesPrior
# more appropriate for capturing large swings in connectedness, particularly around extreme events
# weaker restrictions  allow the model to capture these irregularities and extreme behavior more naturally. 
# It offers flexibility in capturing shifts in connectedness caused by large market moves or political events.   

  # PriorChoice =list(TVPVAR=list(kappa1=forgetting_factor, kappa2=decay_factor, prior="BayesPrior"))

# MinnesotaPrior

# Prior shrinks the parameters toward an assumption that each market is primarily driven by its own dynamics rather than by shocks from other markets
# The Minnesota prior works well in cases where theoretical or empirical justification supports weak relationships between variables. 

 PriorChoice = list(TVPVAR = list(kappa1 = forgetting_factor_asym, kappa2 = decay_factor_asym, prior="MinnesotaPrior", gamma=0.1))


# TVP_VAR using ConnectednessApproach package
dca = ConnectednessApproach(return_zoo, 
                            nlag=lag_order, 
                            nfore=H,
                            window.size=window_size,
                            model="TVP-VAR",
                            connectedness="Time",
                            VAR_config = PriorChoice) # TVP-VAR model with forgetting factor and decay factor as specified

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

# Start PNG device before creating the plot
png(file.path(Rep, "TCI_returns.png"), width = 800, height = 600)  # Size in pixels

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

# Start PNG device before creating the plot
png(file.path(Rep, "TO_returns.png"), width = 800, height = 600)  # Size in pixels

# Set larger margins (bottom, left, top, right) to avoid clipping of titles/labels
par(mar=c(10, 4.5, 5, 2) + 0.1)  # Adjust these numbers as needed

PlotTO(dca, ylim = c(0, 100))

# Close the device and save the plot
dev.off()

#----------------------------------

## Dynamic directional spillovers FROM markets ##

#----------------------------------

# Start PNG device before creating the plot
png(file.path(Rep, "FROM_returns.png"), width = 800, height = 600)  # Size in pixels

# Set larger margins (bottom, left, top, right) to avoid clipping of titles/labels
par(mar=c(10, 4.5, 5, 2) + 0.1)  # Adjust these numbers as needed

PlotFROM(dca, ylim = c(0, 75))  # Adjust these numbers as needed

# Close the device and save the plot
dev.off()

#----------------------------------

## Net Total Directional Connectedness - NET ##

#----------------------------------

# Start PNG device with adjusted height for more header space
png(file.path(Rep, "NET_returns.png"), width = 800, height = 600)

# Plot the connectedness measures - Net Pairwise Total Connectedness
PlotNET(dca, ylim = c(-50, 50), mar = c(5, 4, 6, 2), oma = c(0, 0, 4, 0))

# Close the PNG device
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
                            window.size=window_size,
                            model="TVP-VAR",
                            connectedness="Time",
                            VAR_config=PriorChoice) # TVP-VAR model with forgetting factor and decay factor as specified

dca$config

## Model Testing ##

#----------------------------------

# Test the model residuals for normality
# Shapiro-Wilk test for normality
#shapiro.test(dca$residuals)



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

# Start PNG device before creating the plot
png(file.path(Rep, "TCI_vol.png"), width = 800, height = 600)  # Size in pixels

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

# Start PNG device before creating the plot
png(file.path(Rep, "TO_vol.png"), width = 800, height = 600)  # Size in pixels

# Set larger margins (bottom, left, top, right) to avoid clipping of titles/labels
par(mar=c(10, 4.5, 5, 2) + 0.1)  # Adjust these numbers as needed

PlotTO(dca, ylim = c(0, 70))

# Close the device and save the plot
dev.off()

#----------------------------------

## Dynamic directional spillovers FROM markets ##

#----------------------------------

# Start PNG device before creating the plot
png(file.path(Rep, "FROM_vol.png"), width = 800, height = 600)  # Size in pixels

# Set larger margins (bottom, left, top, right) to avoid clipping of titles/labels
par(mar=c(10, 4.5, 5, 2) + 0.1)  # Adjust these numbers as needed

PlotFROM(dca, ylim = c(0, 50))  # Adjust these numbers as needed

# Close the device and save the plot
dev.off()

#----------------------------------

## Net Total Directional Connectedness - NET ##

#----------------------------------

# Start PNG device with adjusted height for more header space
png(file.path(Rep, "NET_vol.png"), width = 800, height = 600)

# Plot the connectedness measures - Net Pairwise Total Connectedness
PlotNET(dca, ylim = c(-50, 40), mar = c(5, 4, 6, 2), oma = c(0, 0, 4, 0))

# Close the PNG device
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
# This creates a color scheme for the different event categories in the event study.
category_colors <- c(
        "global politics" = "orange", 
        "carbon market" = "red", 
        "weather" = "gold",
        "energy" = "green",
        "finance" = "blue",
        "covid-19" = "purple"
)

max.index <- 50

# Add an EventNumber column to the TCI dataframe mapping the event number to repeat between the StartDate and EndDate
# Here, we are adjusting the event study dates to ensure they fall within the TCI data's date range.
events_study_df$EndDate <- pmin(events_study_df$EndDate, max(TCI_return$Date))
events_study_df$StartDate <- pmax(events_study_df$StartDate, min(TCI_return$Date))

# Calculating the midpoint of the event window for labeling and alternating label positions.
events_study_df <- events_study_df %>%
  mutate(
    Midpoint = as.Date(Midpoint , origin = "1970-01-01"),  # Midpoint for event label positioning
    LabelY = c(seq(max.index, 20, length.out = ceiling(n() / 2)), seq(20, max.index, length.out = floor(n() / 2)))  # First half seq(50, 25), second half seq(25, 50)
  )


# Create the plot
ggplot() + 
  geom_rect(data = events_study_df, aes(xmin = pmax(StartDate, min(TCI_return$Date)), xmax = pmin(EndDate, max(TCI_return$Date)), ymin = 0, ymax = max.index, fill = Category), alpha = 0.2) +  
  geom_segment(data = events_study_df, aes(x = StartDate, xend = StartDate, y = 0, yend = max.index), linetype = "solid", color = "grey", size = 0.25, alpha = 0.25) +  
  geom_segment(data = events_study_df, aes(x = EndDate, xend = EndDate, y = 0, yend = max.index), linetype = "solid", color = "grey", size = 0.25, alpha = 0.25) +  
  geom_ribbon(data = TCI_return, aes(x = Date, ymin = 0, ymax = TCI), fill = "darkgrey", alpha = 1) +  
  geom_line(data = TCI_return, aes(x = Date, y = TCI), color = "black", size = 0.5) +  
  geom_text(data = events_study_df, aes(x = Midpoint, y = LabelY, label = EventNumber), angle = 90, vjust = 0.5, hjust = 0) + 
  labs(x = "year", y = "Total Connectedness Index (TCI)", title = "Total Return Connectedess Event Study") +
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
  scale_y_continuous(limits = c(0, max.index))

# Save the plot as a PNG with white background
ggsave(file.path(Rep, "TCI_returns_with_events.png"), width = 8, height = 6, dpi = 300, bg = "white")

#----------------------------------

# Plot TCI data with event study window for volatility

#----------------------------------

# Create the plot
ggplot() + 
  geom_rect(data = events_study_df, aes(xmin = pmax(StartDate, min(TCI_vol$Date)), xmax = pmin(EndDate, max(TCI_vol$Date)), ymin = 0, ymax = max.index, fill = Category), alpha = 0.2) +  
  geom_segment(data = events_study_df, aes(x = StartDate, xend = StartDate, y = 0, yend = max.index), linetype = "solid", color = "grey", size = 0.25, alpha = 0.25) +  
  geom_segment(data = events_study_df, aes(x = EndDate, xend = EndDate, y = 0, yend = max.index), linetype = "solid", color = "grey", size = 0.25, alpha = 0.25) +  
  geom_ribbon(data = TCI_vol, aes(x = Date, ymin = 0, ymax = TCI), fill = "grey", alpha = 1) +  
  geom_line(data = TCI_vol, aes(x = Date, y = TCI), color = "black", size = 1) +  
  geom_text(data = events_study_df, aes(x = Midpoint, y = LabelY, label = EventNumber), angle = 90, vjust = 0.5, hjust = 0) + 
  labs(x = "year", y = "Total Connectedness Index (TCI)", title = "Total Volatility Connectedess Event Study") +
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
  scale_y_continuous(limits = c(0, max.index))

# Save the plot as a PNG with white background
ggsave(file.path(Rep, "TCI_volatility_with_events.png"), width = 8, height = 6, dpi = 300, bg = "white")

#