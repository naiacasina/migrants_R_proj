# GIT README FILE
*Aim:* integrate survey and census data to better understand the characteristics and distribution of migrant ag workers across different states in the US.

## Files
1. migrants_stats.R: Computes distributions of variables across states for the NAWS survey using weights and performs the merger of survey and census data.
2. migrants_plots.R: Plots the proportions across any stored variables. The function plot_variable_proportions() takes on two inputs: the data, and the variable that we'd like to plot. The output creates different bins for years 2012 and 2017 and categories "Migrant" and "Non-migrant". Choose the variable and run the function. This will print the results and save the figure to the 'Figures/' folder.

## Data
### Census Data 

- Source: USDA National Agricultural Statistics Service, https://quickstats.nass.usda.gov/.
- Scope: Attempts to cover every individual within the population, capturing comprehensive demographic and occupational data.
- Time-span: Every 5 years, available from 2012. Last year: 2022.
- Coverage: National level, with data granularity down to the county level. However, this project primarily uses state-level data due to variations in data accuracy at more granular levels when it comes to merging with survey data.

### Survey data

- Source: National Agricultural Workers Service (NAWS), https://www.dol.gov/agencies/eta/national-agricultural-workers-survey/data.
- Scope: Focuses on a sample of the population, specifically designed to reflect the characteristics of farmworkers.
- Time-span: Every year, available from -. Last year: 2020.
- Limitations: 
  - The survey does not claim to represent all farmworker demographics comprehensively, notably excluding H-2A visa holders. 
  - The data is representative at the state level for California, but assumptions of representativeness for larger regions are usually statistically inappropriate.   
  
## Methodology 
1. Data Matching: The survey data is matched to corresponding regions within the census data to ensure geographic alignment, under the assumption stated above. Continuous variables are first categorized before proportion calculation. Up to 5 categories are considered and this can be modified in the code. 
2. Assumptions: The survey data, while not representative at the county level, is considered representative at the state level for specific variables. This approach assumes that regional divisions within the survey are based on homogeneity across the states considered, thereby justifying uniform application across each state within a region.

## Future Considerations
- Data Enrichment: Consider integrating ancillary variables that could allow for more nuanced analysis across states, such as household size variations.
- Survey Expansion: Evaluate inclusion of H-2A visa holders in future surveys for representativeness.

