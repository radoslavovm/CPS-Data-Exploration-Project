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
library(maps)
library(gtsummary)
library(gt)
library(clustertend)
library(NbClust)
library(clValid)
library(mclust)
library(sf)
library(hopkins)

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
    'B15003_022', # Bachelor's Degree
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
    statistic = all_continuous() ~ "{mean} ({sd}) [{min}, {max}]", # Summary Stats
    digits = all_continuous() ~ 2, # 2 decimal places
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
      Internet_Subscription ~ "Home Internet Subscription (%)")
  ) %>% 
  modify_caption(
    "Table 1: Socioeconomic Indicators at County Level")

# Output
summary_table

# Correlation Heatmap
ggcorr(data %>% 
         select(-NAME),
       hjust = .9, 
       size = 2)

# Maps Set-Up
# Extract Shape File
tx_shapes <- st_read("tl_rd22_us_county.shp")

# Read Texas Only
tx_shapes <- tx_shapes %>% 
  filter(STATEFP == "48")

# Join to acs_clean
map_data <- tx_shapes %>%
  left_join(acs_clean, by = "GEOID")



# PCA
# Check which have NAs
colSums(is.na(data %>% select(-NAME)))

# Remove NAs for PCA
clean_data <- data %>%
  select(-NAME) %>%
  na.omit() # Removes states with missing values

# Run PCA
pca_result <- prcomp(clean_data, center = TRUE, scale. = TRUE)

# Results as df
pc_scores <- as.data.frame(pca_result$x)

# PCA Summary
summary(pca_result)

# Compute Proportion of Variance and Cummulative Variance
# Compute manually 
explained_variance <- pca_result$sdev^2 / sum(pca_result$sdev^2)

# Use summary output
explained_variance <- summary(pca_result)$importance[2,]

# Proportion of Variance explained (PVE) by each PC
plot(explained_variance, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained", ylim = c(0, 1),
     type = "b")

# Cumulative PVE across all PCs
plot(cumsum(explained_variance), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     ylim = c(0, 1), type = "b")

# Scree Plot through Function fviz_eig
fviz_eig(pca_result, addlabels = TRUE)

# Biplot 
fviz_pca_biplot(pca_result, geom.ind = "point", repel = TRUE)

# Loadings and Scores
# Extract loadings from the PCA result
loadings <- pca_result$rotation[, 1]

# Loadings results
loadings

# Plot loadings 
ggplot() +
  geom_bar(
    aes(
      x = reorder(names(loadings), 
                  abs(loadings)),
      y = loadings), stat = "identity") +
  coord_flip() +
  labs(title = paste("Loadings for the first PC"), x = "Variable", y = "Loading")




# Clustering
# Identify which rows have no missing values
rows_used <- complete.cases(data %>% select(-NAME))

# Standardize the dataset (Removing name and scaling)
df <- data %>%
  select(-NAME) %>%
  na.omit() %>%
  scale() 

# Plot the Dataset with prcomp (Lab 3)
fviz_pca_ind(
  prcomp(df),
  title = "PCA – Texas County-Level Socioeconomic Data",
  geom = "point",
  ggtheme = theme_classic()
)

# Compute the Hopkins statistic
set.seed(2025)
m <- round(nrow(df) * 0.1) # 10% of the rows
hopkins_stat <- hopkins(df, m)
hopkins_stat


# NbClust() for best number of clusters
pc <- prcomp(df, scale.=TRUE)
df_pc <- pc$x[, 1:5]  # stops at the elbow

nb <- NbClust(df_pc, distance="euclidean", min.nc=2, max.nc=10, method="kmeans")

# Performing K-Means Clustering
set.seed(2025)
# 3 Clusters
km.res1 <- kmeans(df, 3)
# Plot 
fviz_cluster(list(data = df, cluster = km.res1$cluster), 
             ellipse.type = "norm", geom = "point", 
             stand = FALSE, palette = "jco", 
             ggtheme = theme_classic())

# Visualize Pairwise Distance
fviz_dist(dist(df), show_labels = FALSE) +
  labs(title = "Distance Matrix – Texas County Data")

# Compare Clustering Methods
set.seed(2025)
cl_methods <- c("hierarchical", "kmeans", "pam", "model")

internal_valid <- clValid(df, 
                          nClust = 2:6,
                          clMethods = cl_methods,
                          validation = "internal")

summary(internal_valid)

stab_valid <- clValid(df,
                      nClust = 2:6,
                      clMethods = cl_methods,
                      validation = "stability")

optimalScores(stab_valid)

# Hierarchical Clustering 
Xsc <- scale(df)

set.seed(100)

m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")
ac <- function(x) {
  agnes(Xsc, method = x)$ac
}
purrr::map_dbl(m, ac)

distance <- dist(Xsc, method = "euclidean")
hc_complete <- hclust(distance, method = 'ward.D2')
plot(hc_complete)

plot <- fviz_dend(hc_complete, k = 2)
plot +
  theme_minimal() +
  labs(title = "Dendrogram based on Hierarchical Clustering")

sub_grp <- cutree(hc_complete, 2)

# Maps for PCA and Clustering
rows_used_pca <- complete.cases(data %>% select(-NAME))

# Create empty vectors (full length = number of counties) to have PC1 & PC2 later
PC1_full <- rep(NA, nrow(data))
PC2_full <- rep(NA, nrow(data))

# Insert PCA scores only for rows that were used in the PCA 
PC1_full[rows_used_pca] <- pc_scores$PC1
PC2_full[rows_used_pca] <- pc_scores$PC2

# Create a dataset with PC1 and PC2 values full dataset and cleaned names
pca_df <- data %>%
  mutate(
    # Add PC1 and PC2 back into the dataset
    PC1 = PC1_full,
    PC2 = PC2_full,
    # Make name lowercase
    county = tolower(NAME),
    # Remove any text after a comma
    county = gsub(",.*", "", county),
    # Remove the word " county" at the end
    county = gsub(" county$", "", county),
    # Remove any extra whitespace
    county = trimws(county)
  )

# Merge with map 
merged_map_pca <- county_map %>%
  left_join(pca_df, by = c("subregion" = "county"))

# PC 1 Map
ggplot(merged_map_pca, aes(long, lat, group = group, fill = PC1)) +
  geom_polygon(color = "white") +
  coord_map() +
  scale_fill_viridis_c(option = "plasma") +
  labs(
    title = "PCA Component 1 Across Texas Counties",
    fill = "PC1"
  ) +
  theme_void()

# PC 2 Map
ggplot(merged_map_pca, aes(long, lat, group = group, fill = PC2)) +
  geom_polygon(color = "white") +
  coord_map() +
  scale_fill_viridis_c(option = "inferno") +
  labs(
    title = "PCA Component 2 Across Texas Counties",
    fill = "PC2"
  ) +
  theme_void()

# Cluster Map
# Full-length cluster vector to match data 
cluster_vec <- rep(NA, nrow(data))   

# Insert clusters for rows used in k-means
cluster_vec[rows_used] <- km.res1$cluster

# Attach the clusters and also clean county names 
# Same cleaning as PC
cluster_df <- data %>%
  mutate(
    cluster = cluster_vec,
    county = tolower(NAME),
    county = gsub(",.*", "", county),
    county = gsub(" county$", "", county),
    county = trimws(county)
  )

# Texas counties
county_map <- map_data("county") %>%
  filter(region == "texas")

# Merge cluster info with map
merged_map_cluster <- county_map %>%
  left_join(cluster_df, by = c("subregion" = "county"))

# Plot cluster map
ggplot(merged_map_cluster, aes(long, lat, group = group, fill = factor(cluster))) +
  geom_polygon(color = "white") +
  coord_map() +
  scale_fill_viridis_d(option = "plasma") +
  labs(
    title = "K-Means Clusters of Texas Counties Based on Socioeconomic Indicators",
    fill = "Cluster"
  ) +
  theme_void()

