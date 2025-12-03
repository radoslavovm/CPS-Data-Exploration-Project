#' ---
#' title: "Group 7 Data Science I Final Project"
#' author: "Rosalie Daniels, Martina Radoslavov, Ayaka Sanui"
#' date: "December 3, 2025"
#' ---

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

# Variable list for ACS pull with explanation of variable
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

tx_counties <- tx_counties %>%
  select(GEOID, NAME, variable, estimate) %>%
  pivot_wider(names_from = variable, values_from = estimate)

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
