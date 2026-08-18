# Code for the Introduction to tidyverse, loops, and functions
# Author: Dr. Roberts
# Date: Aug 17, 2026
# Description: Downloads an example data file from the course website and reads it into R.
#              Manipulates the data using dplyr functions.
#              Comments are intentionally missing and should be filled in by student as a part of the exercise.

### Citations:

# # Manuscript:
# Roberts, C. P., Donovan, V. M., Nodskov, S. M., Keele, E. B., Allen, C. R.,
# Wedin, D. A., & Twidwell, D. (2020). Fire legacies, heterogeneity, and the
# importance of mixed-severity fire in ponderosa pine savannas. Forest Ecology
# and Management, 459, 117853.

# # Data:
# Roberts, Caleb; Allen, Craig; Keele, Emma; Nodskov, Sarah; Donovan, Victoria;
# Twidwell, Dirac; Wedin, David (2020), “Data for: Fire legacies, heterogeneity,
# and the importance of mixed-severity fire in ponderosa pine savannas”,
# Mendeley Data, V1, doi: 10.17632/s78xwkxyrb.1

#=============================================================================
## Preparations

# If librarian package not installed, install it.
list.of.packages <- c("librarian")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Load librarian ...
require(librarian, 
        quietly = TRUE)

# .. and then install tidyverse in a temporary directory
shelf(tidyverse,
      lib = tempdir(),
      quiet = TRUE)

#=============================================================================
## Load data

# Pine Ridge tree data
trees_raw <- 
  read_csv("https://github.com/LivingLandscapes/Course_EcologicalModeling/raw/refs/heads/master/data/PineRidge_30x30Tree_ALL.csv")

# Pine Ridge coarse woody debris data
cwd_raw <- 
  read_csv("https://raw.githubusercontent.com/LivingLandscapes/Course_EcologicalModeling/master/data/PineRidge_CWD_ALL.csv")

### NOTES: 

# # plot.code: unique identifier for a sampling location. These are the sampling
# units.

# # burn: burn = "D" is the Dawes Fire that burnt 10 years prior to data
# collection. burn = "FR" is the Fort Robinson Fire that burnt 27 years prior
# to data collection.

# # severity: first letter = cover type (forest = F, grassland = G); second letter
# = burn severity (U = unburned, L = low severity, M = moderate severity, H =
# high severity, B = burned grassland)

# # decay.class: L = live tree; numbers = ascending decay stages with 1 being
# still intact and 5 being near collapse.

# # species: PP = Ponderosa Pine

#=============================================================================
## Data wrangling with tidyverse!      

# 
str(trees_raw)

# 
trees_selected <- 
  trees_raw %>%
  select(plot.code:severity, species, decay.class, dbh.cm)

#
trees_selected <-
  trees_selected %>%
  #
  mutate(YearsSinceFire = case_when(burn == "D" ~ 10,
                                    burn == "FR" ~ 27,
                                    .default = 100),
         #
         CoverType = str_sub(severity, 1, 1),
         #
         BurnSeverity = str_sub(severity, 2, 2))


# 
trees_selected <- 
  trees_selected %>%
  filter(decay.class == "L" & species == "PP") %>%
  # 
  select(-severity, - decay.class, -species)

# 
trees_summarized <- 
  trees_selected %>%
  # 
  group_by(plot.code, burn, CoverType, BurnSeverity, YearsSinceFire) %>%
  #
  summarize(DBH_mean = mean(dbh.cm)) %>%
  # 
  ungroup()

#=============================================================================
## Challenge 1: Joining tables

# 1. Summarize the CWD table such that we get the total line cover per plot.code
# and each row corresponds with exactly one plot.code.

# 2. Join the Tree and CWD tables, and check that you have not lost rows (i.e.,
# plot.codes) or metadata.

# Open vingnette on join functions that merge two tables. Read through this to
# get a roadmap for joining the tree and cwd tables.
vignette("two-table", package = "dplyr")

##########################################################################################
#      Doing the "Plotting data with ggplot2" exercise from the class web site           #

# Exercise 1

qplot(Lon, Lat, colour = Chao1, data = lichen)

ggplot(lichen, aes(Lon, Lat, colour = Chao1)) +
 geom_point() 

# Exercise 2 (plot for each of the three tree species)

ggplot(lichen, aes(Lon, Lat, colour = Chao1)) +
  geom_point() +
  facet_grid(~ Genus_species.tree)

ggplot(lichen, aes(Lon, Lat, colour = Chao1)) +
  geom_point() +
  facet_wrap(~ Genus_species.tree)











