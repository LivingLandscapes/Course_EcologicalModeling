###################### HGAM lab coding exercise ##############################


# List of packages necessary to run this script:
require(librarian, quietly = TRUE)
shelf(tidyverse,
      map_data,
      mgcv, # For checking model convergence
      MuMIn, # for model selection
      gratia, # for ggplot functionality with mgcv
      modelr, # for cross-validation
      vroom, # for loading data FAST
      sf, # making spatial objects and maps
      lib = tempdir(),
      quiet = TRUE)

# Set random number
set.seed(9903)

# Set the web address where R will look for files from this repository
# Do not change this address
repo_url <- "https://raw.githubusercontent.com/LivingLandscapes/Course_EcologicalModeling/master/"

# Load invasive species cover data
fia <- read_csv(paste0(repo_url, "data/FIA_Invasives_Cover.csv"))

# Load invasive species metadata
spp_info <- read_csv(paste0(repo_url, "data/FIA_Invasives_GrowthTaxonomy.csv"))

# Join the cover and metadata
fia <- left_join(fia, spp_info)

# Some preparations for mgcv
fia <-
  fia %>%
  mutate(across(c(Order, GrowthHabit1, GrowthHabit2, GrowthHabit3, SCIENTIFIC_NAME, pltID),
                as.factor), # mgcv wants factors for random effects
         across(c(Year, Latitude, Longitude),
                ~ (.x - mean(.x)) / sd(.x),
                .names = "{.col}_scaled")) # scale covariates

#==============================================================================
## Data Exploration

# Below, you can see that we're dealing with >100k rows and a few potential
# grouping variables.

# Number of columns and rows?
dim(fia) # > 100k rows!

# How many unique values in each column?
fia %>%
  summarize(across(everything(), ~ length(unique(.x))))

# What's the max, min, mean, and median of the "cover" column?
fia %>%
  summarize(Mean = mean(cover),
            Median = median(cover),
            Min = min(cover),
            Max = max(cover))

# How many cover values = 0?
sum(fia$cover == 0)

# How many cover values = 1?
sum(fia$cover == 1)

## QUESTIONS: 

# 1. Given there is only a single zero in the cover data, how does that affect
# the scope of our inference?

# 2. What error distributions could be useful here?

#=============================================================================
## Should you 'gam' or 'bam'?

# This is a logistic issue relevant to using (H)GAMs with large datasets. The
# 'gam' function is the main mgcv way to run a (hierarchical) generalized
# additive model, but if you're analyzing a lot of data (e.g., >100k rows), you
# may consider switching to the 'bam' function. The outputs are very similar,
# but there are some differences due to some shortcuts the 'bam' function uses
# behind the curtains. If you're interested, read about that
# [here](https://www.rdocumentation.org/packages/mgcv/versions/1.9-0/topics/bam)
# and [also
# here](https://stackoverflow.com/questions/71926851/different-estimates-between-bam-and-gam-model-mgcv-and-interaction-term-estima).


# If you have a ton of spare time, you can check out the difference in runtimes:
  

# # runtime for 'bam'
# system.time(
#   bam(cover_0.001 ~
#               te(Longitude_scaled, Latitude_scaled) +
#               s(Year_scaled) +
#               s(Year_scaled, GrowthHabit1, bs = "fs"),
#       family = Gamma("log"),
#       data = fia,
#       discrete = TRUE,
#       method = "fREML")
#   )
#
# # runtime for 'gam'
# system.time(
#   gam(cover_0.001 ~
#               te(Longitude_scaled, Latitude_scaled) +
#               s(Year_scaled) +
#               s(Year_scaled, GrowthHabit1, bs = "fs"),
#       family = Gamma("log"),
#       data = fia,
#       method = "REML")
#   )

#=============================================================================
## Abundance changes: Growth habit or taxonomy?

# Let's try our first HGAM: estimate a global function for our main covariate of
# interest (Year) plus a individual-level random slope for Year per growth
# habitat/order. We'll also throw latitude and longitude into a tensor smooth to
# account for spatial autocorrelation.

### CHALLENGE #1:

# 1. Create two GS models per Pedersen et al (2019) using bam(). Set
# "GrowthHabit1" as your factor in one and "Order" as your factor in the other.
# Name the models 'growth_modGS' and 'order_modGS'.

# 2. Create two GI models per Pedersen et al (2019) using bam(). Set
# "GrowthHabit1" as your factor in one and "Order" as your factor in the other.
# Name the models 'growth_modGI' and 'order_modGI'.

# 3. On your own, run the model diagnostics and compare models. Are there
# improvements, and if so, where do you see them? Should we consider increasing
# "k" for any of the smoothers?

#=============================================================================
## Model selection

# We learned from the Pedersen et al. (2019) paper that we can use AIC to
# compare HGAMs, which is awesome! But--model selection via AIC should not be
# our stopping point. In this section, we're going to dip our toes into cross
# validation to compare the *predictive power* of HGAMs.

# But first, let's just see what AIC(c) has to say about our two HGAMs:
model.sel(list(growth_modGS = growth_modGS,
               order_modGS = order_modGS,
               growth_modI = growth_modI,
               order_modI = order_modI))


### CHALLENGE #2: 

# In your own words, articulate what this model selection table means
# ecologically.

#==============================================================================
## Cross validation

# Model selection is not the only way to check for model performance. Here,
# we'll do a "quick and dirty" cross validation exercise to see how our models
# perform relative to the observations--and in comparison with each other. For
# brevity's sake, we'll just look at the top two models from our model
# selection.

### CHALLENGE #3

# 1. Create training and testing data.frames. Hint: use 'sample_n' and
# 'anti_join' to do this.

# 2. Train the first and second ranked models per model selection.

# 3. Use RMSE and MAE to a) test the models and b) compare the models.


#==============================================================================
## Visualize HGAM smooth estimates
  
# Now, let's plot the smooth estimates from our top model. Recall what a "smooth
# estimate" is in a generalized additive model. How does it differ from a
# "prediction?"

# There are multiple ways to visualize the smooth estimates, but here, we'll
# take the easy way out and use functions from the "gratia" package.

# Luckily, the gratia package has some nice functions for creating customized
# model prediction plots:

# See the exact names of the smooths in the model object
smooths(order_modGS)

# Save smooth estimates.
sm <- smooth_estimates(order_modGS)

# Plot the global "s(Year_scaled)" estimate
sm %>%
  filter(.smooth == "s(Year_scaled)") |>
  add_confint(coverage = 0.80) |> # 80% confidence intervals
  ggplot(aes(y = .estimate,
             x = Year_scaled)) +
  geom_ribbon(aes(ymin = .lower_ci,
                  ymax = .upper_ci),
              alpha = 0.3,
              fill = "gray70") +
  geom_line(linewidth = 1) +
  geom_hline(data = data.frame(yintercept = 0),
             mapping = aes(yintercept = yintercept),
             linetype = 2,
             color = "darkred") + # Adding to make it clear where confidence intervals encompass zero.
  theme_bw() +
  ylab("Estimate")

# Plot the factor smooths for each growth habit
sm %>%
  filter(.smooth == "s(Year_scaled,Order)") |>
  add_confint(coverage = 0.80) |> # 80% confidence intervals
  ggplot(aes(y = .estimate,
             x = Year_scaled,
             group = Order)) +
  geom_ribbon(aes(ymin = .lower_ci,
                  ymax = .upper_ci),
              alpha = 0.3,
              fill = "gray70") +
  scale_color_viridis_d() +
  geom_line(linewidth = 1,
            mapping = aes(color = Order)) +
  geom_hline(data = data.frame(yintercept = 0),
             mapping = aes(yintercept = yintercept),
             linetype = 2,
             color = "darkred") + # Adding to make it clear where confidence intervals encompass zero.
  theme_bw() +
  # facet_wrap(~ Order) +
  ylab("Estimate")


# CHALLENGE #5: 

# 1. Interpret the prediction plots. How does the global smoother (
# "s(Year_scaled)" ) estimate compare to the individual factor smooths (
# "s(Year_scaled,Order)" )? What does it mean for the estimate to be above the
# zero line, encompassing the zero line, and below the zero line?
  
# 2. Circle back to a question from the beginning of the lab. Given the trend
# you're seeing in the "s(Year_scaled)" plot, what does it mean for our inferece
# that there are no zeroes in this dataset???

#==============================================================================
## Spatial predictions

# In this final section, we're going to deviate from our hypotheses a bit in
# order to show you how to do some spatial predictions--and then create maps
# with your spatial predictions. There are MANY resources that show how to do
# this--particularly for raster data. So to be additive and not repetitive,
# here, we will predict an HGAM's outputs across a vector dataset.

# To be even more fun, we'll make a "honeycomb" (hexagonal) map. There are some
# actual reasons to use a honeycomb instead of a "fishnet" (square grid), some
# of which are listed in this
# [paper](https://link.springer.com/article/10.1007/s10980-015-0295-0). Also,
# the nature of the FIA data lends itself to being re-aggregated into large
# polygons.

# As a first step, let's make the honeycomb!

# First, convert the FIA data to an sf object
fia_sf <- st_as_sf(fia,
                   crs = st_crs(4326), # This number is the code for the Lat/Long Coordinate Reference System. You can look other codes up (e.g., UTM zone 14 = 32614)
                   coords = c("Longitude", "Latitude"))

# Read in a shapefile with all extensions of the US states of interest
states <-
  st_read(paste0("/vsicurl/", # need to add this to get all shapefile extensions
                 repo_url,
                 "data/US_States/cb_2018_us_state_5m.shp")) %>%
  filter(NAME %in% c("Kentucky","Tennessee", "Mississippi",
                     "Alabama", "Georgia", "Florida",
                     "Arkansas", "South Carolina", "Virginia",
                     "Louisiana", "North Carolina"))

# Create hexagonal grid...
honeycomb_grid <-
  st_make_grid(st_union(states),  # Need to combine all state polygons
               cellsize = 0.5,
               what = "polygons",
               square = FALSE) %>%
  st_sf() %>% # Force geometry into a sf "data.frame" like object
  rowid_to_column('hex_id')  %>% # Make an ID column for the hex polygons
  st_make_valid() # Not sure why we need this, but it was necessary for the intersection to work haha

# ... and clip to our study extent (states)
honeycomb_grid <-
  st_intersection(honeycomb_grid,
                  st_union(states))

# Now that we've made the honeycomb polygons, let's make the data to predict
# onto. To do that, we'll need coordinates (latitude and longitude, scaled).
# But... which coordinates do we choose? For raster cells, it's pretty easy as
# every pixel has an associated coordinate. But for vector data, there are an
# infinite number of coordinates we could choose from.

# **On your own, think of a solution to this problem!** Once you've got an idea, reveal the code below.






# We did it!
  
  # ## Discussion questions
  # 
  # 1. Given the global time smooth estimate ( "s(Year_scaled)" ), what would the prediction maps look like in the final year of our FIA data?
  # 
  # 2. What are similarities and differences in the prediction maps? What aspects of the HGAM we used makes them similar/different?
  # 
  # 3. What kind(s) of HGAM would create very different predictive maps/smooth estimates for the different order?
  # 
  # 4. Given there were no zeroes in our response variable (cover), what can these models actually tell us? What can they **not** tell us? What would be a way to add zeroes to the data?