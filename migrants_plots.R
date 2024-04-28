# In this script we:
#   1. Plot the proportions across any stored variables.
# Usage:
#   1. Function plot_variable_proportions() takes on two inputs: the data,
#      and the variable that we'd like to plot. Choose the variable and run the
#      function. This will print the results and save the figure to the 
#      'Figures/' folder.

rm(list=ls()) 
packages <- c("dplyr", "survey", "ggplot2", "tidyr", "usethis", "arrow", "readr")
lapply(packages, require, character=TRUE)

# change path to 'Migrants' folder
setwd('/Users/naiacasina/Library/CloudStorage/OneDrive-UCB-O365/Projects/Ag Workforce/Migrants')

# load data
results_with_regions <- read_parquet('Results/results_with_regions.parquet')

# -------------------- plotting -------------------
plot_variable_proportions <- function(data, variable_prefix) {
  variable_columns <- grep(paste0("^", variable_prefix, "\\."), names(data), value = TRUE)
  
  if (length(variable_columns) == 0) {
    stop("No variables found with the specified prefix.")
  }
  
  data$FY <- factor(data$FY)
  data$MIGRANT <- factor(data$MIGRANT, labels = c("Non-Migrant", "Migrant"))  # Labeling for clarity in plots
  
  # wide to long
  long_data <- data %>%
    select(all_of(variable_columns), everything()) %>%  # Use `all_of` for column selection
    pivot_longer(
      cols = all_of(variable_columns),  # Using `all_of` here
      names_to = "Category",
      values_to = "Proportion",
      names_prefix = paste0(variable_prefix, "\\.")
    ) %>%
    mutate(Year = FY)  
  
  long_data <- long_data[!is.na(long_data$Proportion), ]
  
  # Set manual alpha values
  unique_years <- sort(unique(long_data$Year))
  alpha_values <- seq(0.3, 1, length.out = length(unique_years))  # Create a sequence from 0.5 to 1 for alpha values
  names(alpha_values) <- unique_years
  
  p <- ggplot(long_data, aes(x = Category, y = Proportion, fill = MIGRANT, alpha = Year)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.7) +
    scale_fill_manual(values = c("Non-Migrant" = "#FC8D62", "Migrant" = "#66C2A5"), name = "Migrant Status") +
    scale_alpha_manual(values = alpha_values, guide = guide_legend(title = "Year")) +  # Manually set alpha values
    facet_wrap(~RegionName, scales = "free_x") +
    labs(x = "Category", y = "Proportion", title = paste("Proportions for", variable_prefix)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
  print(p)
  fig_path <- paste0('Figures/prop_plot_', variable_prefix, '.png' )
  ggsave(fig_path, plot = p, width = 10, height = 8, units = "in", dpi = 300)
}

plot_variable_proportions(results_with_regions, "CROP")

