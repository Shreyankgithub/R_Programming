# NPHET Visualizations and Report"

# Load required packages
library(ggplot2)
library(maps)
library(lubridate)
library(dplyr)
library(ggthemes)
library(mapproj)

# Read the dataset
data <- read.csv("C:/country_data.csv")

# World Map chart
suppressWarnings({
  WorldData <- map_data('world') %>% filter(region != "Antarctica") %>% fortify
  df <- data.frame(location = c('Mayotte', 'Mauritania', 'Lithuania', 'Ireland', 'Indonesia', 'Guernsey', 'Greenland', 'Comoros', 'Burundi', 'Bolivia'), 
                   value = c(4, 10, 11, 5, 8, 12, 9, 3, 6, 7),
                   stringsAsFactors = FALSE)
  world_map <- ggplot() +
    geom_map(data = WorldData, map = WorldData,
             aes(x = long, y = lat, group = group, map_id = region),
             fill = "white", colour = "#7f7f7f", size = 0.5) +
    geom_map(data = df, map = WorldData,
             aes(fill = value, map_id = location),
             colour = "#7f7f7f", size = 0.5) +
    coord_map("rectangular", lat0 = 0, xlim = c(-180, 180), ylim = c(-60, 90)) +
    scale_fill_continuous(low = "thistle2", high = "darkred", guide = "colorbar") +
    scale_y_continuous(breaks = NULL) +
    scale_x_continuous(breaks = NULL) +
    labs(fill = "Total Cases", title = "COVID-19 Total Cases Worldwide", x = "", y = "") +
    theme_bw()
  world_map
}) 

# Bar chart
suppressWarnings({
  bar_chart <- ggplot(data, aes(x = location, y = total_cases, fill = location)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(title = "COVID-19 Total Cases by Location", x = "Location", y = "Total Cases") +
    theme_minimal() +
    theme(panel.background = element_rect(fill = "white")) +
    scale_y_continuous(labels = scales::comma)
  bar_chart
})

# Scatterplot with linear regression line
# Remove missing and non-finite values
data_clean <- data[is.finite(data$total_cases_per_million) & is.finite(data$new_cases_per_million), ]
# Create scatterplot
scatterplot <- ggplot(data_clean, aes(x = total_cases_per_million, y = new_cases_per_million, color = location)) +
  geom_point(na.rm = TRUE) +
  geom_smooth(method = "lm", se = FALSE, color = "red", na.rm = TRUE) +
  labs(title = "Total Cases per Million vs. New Cases per Million", x = "Total Cases per Million", y = "New Cases per Million") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white")) +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma)
scatterplot

# Time-series chart
data$year <- as.numeric(data$year)
data$total_cases <- as.numeric(data$total_cases)
# Create a time-series chart
time_series <- ggplot(data, aes(x = year, y = total_cases, color = factor(year))) +
  geom_line(size = 1) +
  labs(title = "Total Cases Over Time", x = "Year", y = "Total Cases") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white")) +
  scale_x_continuous(labels = scales::comma, breaks = scales::pretty_breaks()) +
  scale_y_continuous(labels = scales::comma)
time_series

# Visualization directory
setwd("C:/visualizations")

# Save the visualizations as separate files
ggsave("World_Map.png", world_map, width = 8, height = 4, bg = "white")
ggsave("Bar_Chart.png", bar_chart, width = 8, height = 4, bg = "white")
ggsave("Scatterplot.png", scatterplot, width = 8, height = 4, bg = "white")
ggsave("Time_Series.png", time_series, width = 8, height = 4, bg = "white")
