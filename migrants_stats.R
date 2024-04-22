# This script computes distributions of variables across states for 
# the NAWS survey (using weights)

rm(list=ls()) 
packages <- c("dplyr", "survey", "ggplot2", "tidyr")
lapply(packages, require, character=TRUE)

setwd('/Users/naiacasina/Library/CloudStorage/OneDrive-UCB-O365/Projects/Ag Workforce/Migrants')
naws_ae <- read.csv('Data/NAWS_A2E197.csv')
naws_fz <- read.csv('Data/NAWS_F2Y197.csv')

# merger
naws_full <- merge(naws_ae, naws_fz, by = "FWID")

# drop years and non-migrants
naws <- naws_full %>%
  filter(FY.x %in% c(2022, 2017, 2012)) %>%
  filter(MIGRANT %in% c(1))

# drop cols
threshold <- nrow(naws) / 2

# cleaned df
naws_df <- naws %>%
  select_if(~sum(is.na(.)) < threshold)

naws_df <- mutate(naws_df, PWTYCRD = ifelse(is.na(PWTYCRD.x), PWTYCRD.y, PWTYCRD.x))
naws_design <- svydesign(ids = ~1, data = naws_df, weights = ~PWTYCRD)

average_age <- svyby(~AGE, ~REGION6, naws_design, svymean)
gender_distribution <- svyby(~GENDER, ~REGION6, naws_design, svymean)

ggplot(gender_distribution, aes(x = REGION6, y = GENDER, fill = REGION6)) +
  geom_bar(stat = "identity") +
  labs(x = "Region", y = "Proportion of Females", title = "Gender Distribution by Region") +
  theme_minimal()

b17_region_distribution <- svytable(~B17CODE + REGION6, naws_design)

# test plot
b17_region_df <- as.data.frame(b17_region_distribution)
b17_region_df$Proportion <- b17_region_df$Freq / sum(b17_region_df$Freq)
b17_region_df <- b17_region_df %>%
  group_by(REGION6) %>%
  mutate(Region_Proportion = Freq / sum(Freq)) %>%
  ungroup()

ggplot(b17_region_df, aes(x = B17CODE, y = Region_Proportion, fill = B17CODE)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(~REGION6) +  # Facet by REGION6 to separate plots by region
  labs(x = "Country of Origin", y = "Proportion within Region", 
       title = "Distribution of Country of Origin by Survey Region") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



# --------- Building a dataframe -------
# -----------

compute_global_bins <- function(data, variable) {
  # calculate range across all data
  min_val <- min(data[[variable]], na.rm = TRUE)
  max_val <- max(data[[variable]], na.rm = TRUE)
  
  #dDefine breaks for binning - creating 5 bins as an example
  breaks <- seq(from = min_val, to = max_val, length.out = 6)  # Creates 5 bins
  return(breaks)
}

compute_proportions <- function(variable, data, weights_col, regions, breaks=NULL) {
  results_df <- data.frame()
  
  for (region in regions) {
    subset_data <- data[data$REGION6 == region & !is.na(data[[variable]]), ]
    
    if (length(unique(subset_data[[variable]])) > 15) {
      # use global breaks for continuous variables
      subset_data$Category <- cut(subset_data[[variable]], breaks = breaks, include.lowest = TRUE, right = FALSE)
    } else {
      # treat as a categorical variablees
      subset_data$Category <- as.factor(subset_data[[variable]])
    }
    
    # calculate the sum of weights for each category
    category_weights <- tapply(subset_data[[weights_col]], subset_data$Category, sum)
    total_weight <- sum(category_weights, na.rm = TRUE)
    proportions <- category_weights / total_weight
    
    # build category
    for (cat in names(proportions)) {
      results_df <- rbind(results_df, data.frame(
        REGION6 = region,
        Variable = paste(variable, gsub("\\((.*),.*", "\\1", cat), sep = "."),
        Proportion = proportions[cat]
      ))
    }
  }
  
  return(results_df)
}

categorical_vars <- c("B14CODE","B17CODE", "A21Cx", "A22Cx", "A24a", "A24b",
                      "HHFAMGRD", "HHGRDKID", "HHKID", "HHOTHFAM",
                      "HHPARENT", "HHSIB", "HHYTH018", "K018USFW",
                      "K018USNF","CROP", "D22", "D23", "D26", "D30", "D33A",
                      "D34Ax", "D35x", "D37A", "E01x", "G01")

binary_vars <- c("FTC", "GENDER", "INDIGENOUS", "INTLSHTL", "MARRIED",
               "FAMPOV", "SPOUSE", 'SPOUSEFW', 'SPOUSENF', 'YOUTH',
               'BLWAGE', "CROWDED1","CROWDED2", "D11", "D12WG4")

all_vars <- c(binary_vars, categorical_vars)

continuous_vars <- c("C09WEEKS", "FWRDAYS", "FWWEEKS")


# compute bins for each and apply proportions calculation
all_results <- data.frame()
for (var in continuous_vars) {
  var_breaks <- compute_global_bins(naws_df, var)
  var_results <- compute_proportions(var, naws_df, "PWTYCRD.x", unique(naws_df$REGION6), var_breaks)
  all_results <- rbind(all_results, var_results)
}

# for categorical variables
for (var in all_vars) {
  var_results <- compute_proportions(var, naws_df, "PWTYCRD.x", unique(naws_df$REGION6))
  all_results <- rbind(all_results, var_results)
}

# convert to wide format as needed
all_results_wide <- pivot_wider(all_results, names_from = Variable, values_from = Proportion, id_cols = REGION6)

region_mapping <- data.frame(
  REGION6 = 1:6,
  RegionName = c("EAST", "SOUTHEAST", "MIDWEST", "SOUTHWEST", "NORTHWEST", "CALIFORNIA"),
  States = c(
    "North Carolina, Virginia, Kentucky, Tennessee, West Virginia, Connecticut, Maine, Massachusetts, New Hampshire, New York, Rhode Island, Vermont, Delaware, Maryland, New Jersey, Pennsylvania",
    "Arkansas, Louisiana, Mississippi, Alabama, Georgia, South Carolina, Florida",
    "Illinois, Indiana, Ohio, Iowa, Missouri, Kansas, Nebraska, North Dakota, South Dakota, Michigan, Minnesota, Wisconsin",
    "Arizona, New Mexico, Oklahoma, Texas",
    "Idaho, Montana, Wyoming, Colorado, Nevada, Utah, Oregon, Washington",
    "California"
  )
)

results_with_regions <- merge(all_results_wide, region_mapping, by = "REGION6", all.x = TRUE)


# -------- plotting -------
plot_variable_proportions <- function(data, variable_prefix) {
  # Create a list to collect all categories related to the variable prefix
  variable_columns <- grep(paste0("^", variable_prefix, "\\."), names(data), value = TRUE)
  
  # If no columns match, provide a message and stop
  if (length(variable_columns) == 0) {
    stop("No variables found with the specified prefix.")
  }
  
  # Gather the data from wide to long format for the specified variable categories
  long_data <- tidyr::pivot_longer(data, 
                                   cols = variable_columns,
                                   names_to = "Category",
                                   values_to = "Proportion",
                                   names_prefix = paste0(variable_prefix, "\\."))
  
  # Filter out rows with NA proportions if any
  long_data <- long_data[!is.na(long_data$Proportion), ]
  
  # Create the plot
  p <- ggplot(long_data, aes(x = Category, y = Proportion, fill = RegionName)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    facet_wrap(~RegionName, scales = "free_x") +
    labs(x = "Category", y = "Proportion", title = paste("Proportions for", variable_prefix)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x labels for better readability
  
  print(p)
}

# Example usage of the updated function
plot_variable_proportions(results_with_regions, "CROP")

