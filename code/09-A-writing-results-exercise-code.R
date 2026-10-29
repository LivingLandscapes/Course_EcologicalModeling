# # Clear environment?
# rm(list=ls())

# List of packages necessary to run this script:
require(librarian)
shelf(tidyverse, 
      here, 
      sf, 
      mgcv,
      gratia,
      MuMIn
      )

# Set path
repo_url <- "https://raw.githubusercontent.com/LivingLandscapes/Course_EcologicalModeling/master/data/"

# Set seed
set.seed(2252)

# Load data
dat_list <- 
  list(RouteDictionary = read_csv(paste0(repo_url, "QuailWhistle_Nebraska_RouteDictionary.csv")),
       RouteUTMs = read_csv(paste0(repo_url, "QuailWhistle_Nebraska_RouteUTMs.csv")),
       RouteWhistleData = read_csv(paste0(repo_url, "QuailWhistle_Nebraska_RouteWhistleData.csv")))

#=============================================================================
## Tidying data

# Tidy whistle data
whistle <- 
  dat_list$RouteWhistleData %>%
  pivot_longer(cols = Cass:Lancaster_YH,
               names_to = "Route.Name",
               values_to = "WhistleCount") %>%
  mutate(Route.Name = case_when(Route.Name == "Cass" ~ "Cass North",
                                Route.Name == "GageN" ~ "Gage North",
                                Route.Name == "GageS" ~ "Gage South",
                                Route.Name == "Jeffersn" ~ "Jefferson",
                                Route.Name == "JohnsonN" ~ "Johnson North",
                                Route.Name == "JohnsonS" ~ "Johnson South",
                                Route.Name == "Lancaster.East" ~ "Lancaster East",
                                Route.Name == "Lancaster.North" ~ "Lancaster North",
                                Route.Name == "Lancaster.Branched.Oak" ~ "Lancaster-Branched Oak",
                                Route.Name == "OtoeN" ~ "Otoe North",
                                Route.Name == "OtoeS" ~ "Otoe South",
                                Route.Name == "Richardn" ~ "Richardson",
                                .default = Route.Name)) %>%
  left_join(dat_list$RouteDictionary %>% mutate(Route.Name = `Route Name`)) %>%
  mutate(Route.Number = `Route Number`) %>%
  filter(Route.Name != "Lancaster_YH") %>%
  left_join(dat_list$RouteUTMs %>%
              mutate(Route.Number = Route) %>%
              group_by(Route.Number) %>%
              filter(row_number() == 1)) %>%
  mutate(Route.Name = factor(Route.Name),
         Year_factor = factor(Year))

# Convert to sf object and buffer points
whistle_sf <- 
  st_as_sf(whistle,
           coords = c("UTMYE", "UTMXN"),
           crs = st_crs(26914)) %>%
  st_buffer(500) %>%
  st_write(here("data"),
           "QuailWhistleShp",
           driver = "ESRI Shapefile")

# Load RAP tree cover data
treeDat <- 
  read_csv(here("data/TreeCover_QuailWhistleNebraska.csv")) %>%
  # For some reason, lots of duplicates...
  distinct() %>%
  # Replace NAs with 0
  replace(is.na(.), 0) %>% 
  # Rename columns for our sanity
  rename(Tree_mean = "mean", Tree_sd = "stdDev") %>%
  # Remove unnecessary columns
  select(-c(count, sum))

# Joint with whistle count data
allDat <- 
  left_join(whistle, treeDat) %>%
  # We only have tree cover data since 1986, so remove earlier data
  filter(Year >= 1986) %>%
  na.omit() %>%
  mutate(across(c(Year, UTMYE, UTMXN, Tree_mean, Tree_sd),
                function(X) (X - mean(X)) / sd(X),
                .names = "{.col}_scaled"))

# Check correlations
cor(allDat[ , c("Tree_mean_scaled" , "Tree_sd_scaled")])
# Probably shouldn't put both of these in the same model!!

#=============================================================================
## Fit a GAM and check diagnostics

# Our first GAM!
fit_gam1 <-
  gam(WhistleCount  ~ s(Tree_mean_scaled) + s(Route.Name, bs = "re"),
      family = gaussian("log"),
      data = allDat,
      # NOTE: REML is default for GAMs.
      method = "REML")

### Challenge 1: 

# Interpret the GAM output with the summary() and plot() functions.

# Check model diagnostics with mgcv::gam.check and  gratia::appraise

# Now, change the family to  ' Gamma("log") ' and compare that the two model
# fits. HINT: You may have to do something else to get the model to run...

#=============================================================================
## Model selection with GAMs

### Challenge 2:

# 1. There are fewer rules for comparing GAMs with AICc (yes--you can compare
# with AICc!) than with LMM/GLMM. Construct models that account for time/space
# in different ways while still using "Tree_mean_scaled" as a smoothed term.

# 2. Compare the models you created with AICc.

#=============================================================================
## Predictions

### Challenge 3:

# Using your top-ranked model, create marginal effects plots for all covariates.

#=============================================================================
## Interpret

### Challenge 4:

# Interpret your model selection results and marginal effects plots in words!
