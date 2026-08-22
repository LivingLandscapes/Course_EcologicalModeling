# Code for data exploration lab
# Author: Dr. Roberts
# Date: Aug 22, 2026
# Description: Challenges for the data exploration lab

### Citations:

# # Manuscript:
# Roberts, C. P., Scholtz, R., Fogarty, D. T., Twidwell, D., & Walker Jr, T. L.
# (2022). Large‐scale fire management restores grassland bird richness for a
# private lands ecoregion. Ecological Solutions and Evidence, 3(1), e12119.

#=============================================================================
## Preparations

# If librarian package not installed, install it.
list.of.packages <- c("librarian")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Load librarian ...
require(librarian, 
        quietly = TRUE)

# .. and then install packages
shelf(tidyverse,
      sf,
      lib = tempdir(),
      quiet = TRUE)

#=============================================================================
## Load data

# Load Roberts et al. (2022) data:
dat_grass <- 
  read.csv("https://raw.githubusercontent.com/LivingLandscapes/LargeScaleFireRestoresGrasslandBirdRichness/LargeScaleFireRestoresGrasslandBirdRichness/LargeScaleFireRestoresGrasslandBirdRichness_RProj/LoessCanyons_BBS_Data/LoessCanyonsBBS_DataRaw.csv")

##### Data column descriptions: 
# - Route: name of breeding bird survey route. 
# - Stop: ID for each stop along survey route+
# - Year: year of survey
# - Rich_Grass: grassland bird species richness 
# - Route_factor: name of breeding bird survey route. 
# - Year_Num: survey years re-numbered from 1 (2010) to 7 (2016)
# - Burned: if this stop was burned on or after the given year, 1, else 0
# - Year_Burned: the year that a given stop was burned
# - easting/northing: easting/northing coordinates in UTM 
# - count: number of 30x30m pixels within 400m of each stop. Number of pixels
#          vary because some are masked because they were not 'rangeland' 
#          pixels. See Methods/Data Collection/Tree Cover for details.
# - mean: mean percent tree cover across all 30x30m pixels within 400m of
#         each stop
# - stdDev: standard deviation of percent tree cover across all 30x30m pixels 
#           within 400m of each stop
# - TSF: years-since-fire

#=============================================================================
## Outliers, zero trouble, balance

# Boxplot of grassland bird species richness
ggplot(data = dat_grass,
       mapping = aes(y = Rich_Grass)) + 
  geom_boxplot() +
  ylab("Grassland Bird Richness") + 
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank())

# Violin plots of grassland bird species richness by year
ggplot(data = dat_grass,
       mapping = aes(x = as.factor(Year), y = Rich_Grass, group = Year)) + 
  geom_violin(draw_quantiles = c(0.1, 0.5, 0.9)) +
  ylab("Grassland Bird Richness") + 
  xlab("Year")

# Histogram of grassland bird species richness
ggplot() +
  geom_histogram(data = dat_grass,
                 mapping = aes(x = Rich_Grass),
      # Notice we're specifying the binwidth. Why should (or shouldn't) we do this?
                 binwidth = 1) + 
  ylab("Frequency") +
  xlab("Grassland Bird Richness") + 
  theme_bw() + 
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))

#### Challenge 1: 

# 1. Check the minimum and maximum values (i.e., the 'range') for the response
# variable and each predictor variable. What do you notice about the ranges? Are
# we going to run into "zero trouble" with the response variable?

# 2. Create histograms for i) mean tree cover, ii) TSF, iii) and standard
# deviation in tree cover. **BONUS:** Make them all in a single ggplot call.

# 3. Check for outliers in the response variable (grassland bird species
# richness) and predictor variables.

# 4. Are the data 'balanced' in terms of sampling? Create figure(s) or table(s)
# to answer this. Some of the figures you've already created may also help...

#=============================================================================
## Normality and Homogeneity in Y 

# Quantile-Quantile plot for grassland bird species richness
ggplot(data = dat_grass,
       mapping = aes(sample = Rich_Grass)) +
  geom_qq() +
  geom_qq_line(color = "red") + 
  ylab("Grassland Bird Richness") +
  xlab("Theoretical Quantiles")

### Challenge 2:

# Does the response variable fit the "normality" assumption?

#=============================================================================
## Collinearity, relationships between X and Y

### Challenge 3:

# 1. Check for pairwise correlations between predictor variables graphically
# using the `pairs` function.

# 2. Check for pairwise correlations between predictor variables statistically
# using the `cor` function.

# 3. Are there "linear" relationships between the response variable and
# predictor variables? How can you use output from the `pairs` function to tell?

#=============================================================================
## Is there actually a relationship between X and Y?!

# Making individual plots
rich_treeMean <-
  dat_grass %>%
  ggplot(aes(x = mean, y = Rich_Grass)) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x)) + # a simple generalized additive model!
  ylab("Grassland Bird Richness") +
  xlab("Mean % Tree Cover") +
  theme_classic() # FYI: there are lots of fun pre-made themes in ggplot2
rich_TSF <-
  dat_grass %>%
  ggplot(aes(x = TSF, y = Rich_Grass)) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x)) +
  ylab("Grassland Bird Richness") +
  xlab("Years-since-fire") +
  theme_bw() # Another theme

# Combine plots
cowplot::plot_grid(rich_treeMean, rich_TSF, ncol = 2)


#=============================================================================
## Interactions between predictor variables

### Challenge 4:

# 1. Are there interactions between predictor variables that we should consider?
# Recreate some ggplot2::facet_wrap plots to check.

#=============================================================================
## Bonus Round: Spatial Patterns

# To get you started, run this code to transform our data.frame into an `sf`
# object:
dat_sf <- 
  sf::st_as_sf(dat_grass,
               coords = c("easting", "northing"),
               crs = st_crs(32614)) # This is the code for UTM Zone 14

### Bonus Challenge:

# Map grassland bird species richness by year. Do you see any spatial patterns?
# Use the `ggplot` and `sf` packages to do this!




