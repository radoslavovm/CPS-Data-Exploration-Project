#' ---
#' title: "Group 7 Data Science I Final Project"
#' author: "Rosalie Daniels, Martina Radoslavov, Ayaka Sanui"
#' date: "December 3, 2025"
#' ---

# Set-Up
library(tidyr)
library(tidycensus)
library(dplyr)
library(purrr)
library(here)
library(tidyverse)
library(psych)
library(ggplot2)
library(cluster)
library(GGally)
library(factoextra)
library(ggcorrplot)
library(gtsummary)
library(gt)
library(clustertend)
library(NbClust)
library(clValid)
library(mclust)
library(sf)
library(cluster)

# Variable list for ACS pull with explanation of variable from ACS Tables
variables <- 
  c(
    # Race
    'B02001_001', # Total Population for Races
    'B02001_002', # White Alone
    'B02001_003', # Black or African American Alone
    'B02001_005', # Asian Alone
    'B02001_009', # Two or more races
    
    # Educational Attainment
    'B15003_001', # Total Education Population
    'B15003_017', # High School Diploma
    'B15003_022', # Bachelors Degree
    'B15003_025', # Doctorate Degree
    
    # Housing Status
    'B25003_001', # Total Housing Units
    'B25003_002', # Owner Occupied
    'B25003_003', # Renter Occupied
    
    # Geographic Mobility by Citizenship
    'B07007_001', # Total Population for Mobility
    'B07007_002', # Native Population
    'B07007_003', # Foreign Born Population
    
    # Poverty Status
    "B17001_001", # Total 
    "B17001_002", # Income in the past 12 months below poverty level
    
    # Transportation to Work
    "B08006_001",  # Total workers
    "B08006_008",  # Public transportation
    
    # Travel time to work
    "B08303_001", # Total workers
    "B08303_013", # Workers with commute time 90 or more minutes
    
    # Family Characteristics/Structure 
    "B09002_001",  # Total population in families
    "B09002_002",  # In married-couple families
    
    # Employment Status
    "B23025_001", # Total Population 16+
    "B23025_005",  # Unemployed
    
    # Internet 
    "B28002_001", # Total
    "B28002_002" # With Internet Subscription in Household
  )

# Retrieves ACS estimates for counties in TX (2019-2023 5-year ACS)
tx_counties <- get_acs(
  geography = "county", 
  state = "TX", 
  variables = variables, 
  year = 2023,
  survey = "acs5"
)

# Pivot to Wide Format
tx_counties <- tx_counties %>%
  select(GEOID, NAME, variable, estimate) %>%
  pivot_wider(names_from = variable, values_from = estimate)

# Converting variables to percentages
acs_clean <- tx_counties %>%
  mutate(
    # Race
    percent_white = B02001_002 / B02001_001 * 100,
    percent_black = B02001_003 / B02001_001 * 100,
    percent_asian = B02001_005 / B02001_001 * 100,
    percent_two_or_more  = B02001_009 / B02001_001 * 100,
    
    # Education
    percent_hs_diploma = B15003_017 / B15003_001 * 100,
    percent_bachelors = B15003_022 / B15003_001 * 100,
    percent_doctorate = B15003_025 / B15003_001 * 100,
    
    # Housing
    percent_owner_occupied = B25003_002 / B25003_001 * 100,
    percent_renter = B25003_003 / B25003_001 * 100,
    
    # Mobility
    percent_native_us = B07007_002 / B07007_001 * 100,
    percent_foreign_born = B07007_003 / B07007_001 * 100,
    
    # Poverty
    percent_poverty = B17001_002 / B17001_001 * 100,
    
    # Public Transit
    percent_public_transit = B08006_008 / B08006_001 * 100,
    
    # Long Commute to Work
    percent_long_commute = B08303_013 / B08303_001 * 100,
    
    # Family Structure
    percent_married_family = B09002_002 / B09002_001 * 100,
    
    # Unemployment
    percent_unemployed = B23025_005 / B23025_001 * 100,
    
    # Internet
    percent_internet = B28002_002 / B28002_001 * 100
  )

# Dataset With the Percentage Variables/Data
# Rename to cleaner names
data <- acs_clean %>%
  select(NAME, percent_white, percent_black, percent_asian, percent_two_or_more,
         percent_hs_diploma, percent_bachelors, percent_doctorate, percent_owner_occupied, 
         percent_renter, percent_native_us, percent_foreign_born,
         percent_poverty, percent_public_transit, percent_long_commute, 
         percent_married_family, percent_unemployed, percent_internet) %>%
  rename(
    White = percent_white,
    Black = percent_black,
    Asian = percent_asian,
    Two_Races = percent_two_or_more,
    HS_Diploma = percent_hs_diploma,
    Bachelors = percent_bachelors,
    Doctorate = percent_doctorate,
    Owner_Occupied = percent_owner_occupied,
    Renter = percent_renter,
    Native = percent_native_us,
    Foreign_Born = percent_foreign_born,
    Poverty = percent_poverty,
    Public_Transit = percent_public_transit,
    Long_Commute = percent_long_commute,
    Married_Family = percent_married_family,
    Unemployed = percent_unemployed,
    Internet_Subscription = percent_internet
  )

# EDA
# Create summary table with tbl_summary
summary_table <- data %>%
  select(-NAME) %>%
  tbl_summary(
    statistic = all_continuous() ~ 
      "{mean} ({sd}); {median} [{p25}, {p75}]",
    digits = all_continuous() ~ 2,
    label = list(
      White ~ "White (%)",
      Black ~ "Black (%)",
      Asian ~ "Asian (%)",
      Two_Races ~ "Two or More Races (%)",
      HS_Diploma ~ "High School Diploma (%)",
      Bachelors ~ "Bachelor’s Degree (%)",
      Doctorate ~ "Doctorate Degree (%)",
      Owner_Occupied ~ "Owner-Occupied Housing (%)",
      Renter ~ "Renter-Occupied Housing (%)",
      Native ~ "Native Population (%)",
      Foreign_Born ~ "Foreign-Born Population (%)",
      Poverty ~ "Below Poverty Line (%)",
      Public_Transit ~ "Public Transit Use (%)",
      Long_Commute ~ "Long Commute (90+ min) (%)",
      Married_Family ~ "Married-Couple Family (%)",
      Unemployed ~ "Unemployment Rate (%)",
      Internet_Subscription ~ "Home Internet Subscription (%)"
    )
  ) %>%
  modify_caption("Table 1: Socioeconomic Indicators at County Level")

# Output
summary_table

# Output
summary_table

# Correlation Heatmap
ggcorr(data %>% 
         select(-NAME),
       hjust = .9, 
       size = 2)

# Histogram Plot of Everything
data %>%
  select(-NAME) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  ggplot(aes(value)) +
  geom_histogram(bins = 20, fill = "blue", color = "white") +
  facet_wrap(~ variable, scales = "free") +
  labs(title = "Distribution of All Variables", x = "Value", y = "Count")

# Have to perform Log Transform on some variables
skewed_vars <- c(
  "Black", "Asian", "Two_Races", "Foreign_Born", "Public_Transit",
  "Poverty", "Long_Commute", "Renter", "Unemployed",
  "Doctorate", "Bachelors"
)

# Maps Set-Up
# Extract Shape File
tx_shapes <- st_read("tl_rd22_us_county.shp")

# Read Texas Only
tx_shapes <- tx_shapes %>% 
  filter(STATEFP == "48")

# Join to acs_clean
map_data <- tx_shapes %>% 
  left_join(acs_clean, by = "GEOID")

# Log Transform
skewed_vars <- c(
  "Black", "Asian", "Two_Races", "Foreign_Born", "Public_Transit",
  "Poverty", "Long_Commute", "Renter", "Unemployed",
  "Doctorate", "Bachelors"
)

data_transformed <- data %>%
  mutate(across(all_of(skewed_vars), ~log1p(.x)))

data_transformed %>%
  select(all_of(skewed_vars)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
  ggplot(aes(x = Value)) +
  geom_histogram(bins = 30, fill = "blue", color = "white") +
  facet_wrap(~ Variable, scales = "free", ncol = 4) +
  theme_minimal() +
  labs(title = "Distributions After Log1p Transformation")

clean_data <- data_transformed %>%
  select(-NAME) %>%
  mutate(across(everything(), as.numeric))


# PCA
# Check missing values (excluding NAME)
colSums(is.na(data %>% select(-NAME)))

# Remove NAs and scale for PCA
clean_data <- data %>%
  select(-NAME) %>%
  na.omit()

# Run PCA
pca_result <- prcomp(clean_data, center = TRUE, scale. = TRUE)

# Convert PCA scores to dataframe
pc_scores <- as.data.frame(pca_result$x)

# PCA Summary
summary(pca_result)

# Proportion of Variance Explained (PVE)
explained_variance <- summary(pca_result)$importance[2,]

# PVE Plot
plot(explained_variance, type = "b",
     xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     ylim = c(0, 1))

# Cumulative PVE
plot(cumsum(explained_variance), type = "b",
     xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     ylim = c(0, 1))

# Scree Plot
fviz_eig(pca_result, addlabels = TRUE)

# PCA Biplot
fviz_pca_biplot(pca_result, geom.ind = "point", repel = TRUE)

# Loading Plots
plot_loading <- function(loading_vector, pc_number) {
  df <- data.frame(
    variable = names(loading_vector),
    loading  = as.numeric(loading_vector)
  )
  
  ggplot(df, aes(
    x = reorder(variable, abs(loading)),
    y = loading
  )) +
    geom_col() +
    coord_flip() +
    labs(
      title = paste("Loadings for PC", pc_number),
      x = "Variable",
      y = "Loading"
    ) +
    theme_minimal(base_size = 11)
}

plots <- lapply(1:5, function(i) {
  plot_loading(pca_result$rotation[, i], i)
})

plots[[1]] | plots[[2]]
plots[[3]] | plots[[4]]
plots[[5]]


# Clustering
# Use first 5 PCs 
pc_for_clustering <- pc_scores[, 1:5]

# Scale PC scores for clustering
pc_for_clustering <- scale(pc_for_clustering)

# Distance visualization using PCs
fviz_dist(dist(pc_for_clustering), show_labels = FALSE) +
  labs(title = "Distance Matrix – PCA-Based")

# Compare Clustering Methods
set.seed(2025)
cl_methods <- c("hierarchical", "kmeans", "pam", "model")

internal_valid <- clValid(
  pc_for_clustering,
  nClust = 2:6,
  clMethods = cl_methods,
  validation = "internal"
)
summary(internal_valid)

stab_valid <- clValid(
  pc_for_clustering,
  nClust = 2:6,
  clMethods = cl_methods,
  validation = "stability"
)
optimalScores(stab_valid)


# Hierarchical Clustering 
set.seed(2025)

# Compute distance on PCs
dist_pc <- dist(pc_for_clustering, method = "euclidean")

# Hierarchical clustering (Ward’s method)
hc_complete <- hclust(dist_pc, method = 'ward.D2')

# Plot Dendrogram
fviz_dend(hc_complete) +
  theme_minimal() +
  labs(title = "Dendrogram Based on PCA Scores")

# WSS, Silhouette, Gap Statistic
set.seed(2025)
fviz_nbclust(pc_for_clustering, FUN = hcut, method = "wss", k.max = 10)
fviz_nbclust(pc_for_clustering, FUN = hcut, method = "silhouette", k.max = 10)
fviz_nbclust(pc_for_clustering, FUN = hcut, method = "gap_stat", k.max = 10)

# Cut into the number of clusters
clusters_pca <- cutree(hc_complete, k = 3)
fviz_dend(hc_complete, k = 3) +
  theme_minimal() +
  labs(title = "Dendrogram Based on PCA Scores")

# Maps for PCA and Clustering
# PCA
rows_used <- rownames(clean_data)

# Create full PC vectors for mapping
PC1_full <- rep(NA, nrow(acs_clean))
PC2_full <- rep(NA, nrow(acs_clean))

PC1_full[rownames(acs_clean) %in% rows_used] <- pc_scores$PC1
PC2_full[rownames(acs_clean) %in% rows_used] <- pc_scores$PC2

pca_df <- acs_clean %>%
  mutate(
    PC1 = PC1_full,
    PC2 = PC2_full
  )

# Join to shapefile
map_data_pca <- tx_shapes %>%
  left_join(pca_df, by = "GEOID")

# Map PC1
ggplot(map_data_pca) +
  geom_sf(aes(fill = PC1), color = "white", size = 0.15) +
  scale_fill_viridis_c(option = "plasma") +
  labs(title = "PCA Component 1 Across Texas Counties", fill = "PC1") +
  theme_void()

# Map PC2
ggplot(map_data_pca) +
  geom_sf(aes(fill = PC2), color = "white", size = 0.15) +
  scale_fill_viridis_c(option = "inferno") +
  labs(title = "PCA Component 2 Across Texas Counties", fill = "PC2") +
  theme_void()

# Clustering Map
# Full cluster vector
cluster_full <- rep(NA, nrow(acs_clean))
cluster_full[rownames(acs_clean) %in% rows_used] <- clusters_pca

cluster_df <- acs_clean %>%
  mutate(cluster = cluster_full)

map_data_cluster <- tx_shapes %>%
  left_join(cluster_df, by = "GEOID")

# Plot clusters
ggplot(map_data_cluster) +
  geom_sf(aes(fill = factor(cluster)), color = "white") +
  scale_fill_viridis_d(option = "plasma") +
  labs(title = "Hierarchical Clusters (PCA-Based)", fill = "Cluster") +
  theme_void()

