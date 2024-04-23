# In this script we:
#   1. Plot the proportions across any stored variables.
# Usage:
#   1. Function plot_variable_proportions() takes on two inputs: the data,
#      and the variable that we'd like to plot. Choose the variable and run the
#      function. This will print the results and save the figure to the 
#      'Figures/' folder.

rm(list=ls()) 
packages <- c("dplyr", "survey", "ggplot2", "tidyr", "usethis", "arrow")
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
  
  # wide to long
  long_data <- data %>%
    pivot_longer(
      cols = variable_columns,
      names_to = "Category",
      values_to = "Proportion",
      names_prefix = paste0(variable_prefix, "\\.")
    ) %>%
    mutate(Year = FY)  
  
  long_data <- long_data[!is.na(long_data$Proportion), ]
  
  p <- ggplot(long_data, aes(x = Category, y = Proportion, fill = RegionName, alpha = Year)) +
    geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
    scale_alpha_manual(values = c(0.5, 1), guide = guide_legend(title = "Year")) +  # Adjust opacities and show legend
    facet_wrap(~RegionName, scales = "free_x") +
    labs(x = "Category", y = "Proportion", title = paste("Proportions for", variable_prefix)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
  print(p)
  fig_path <- paste0('Figures/prop_plot_', variable_prefix, '.png' )
  ggsave(fig_path, plot = p, width = 10, height = 8, units = "in", dpi = 300)
}

plot_variable_proportions(results_with_regions, "GENDER")

