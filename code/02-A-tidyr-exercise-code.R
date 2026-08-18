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

# 1. Summarize the CWD table such that we get the total coarse woody debris
# volume (line cover x width x height) per plot.code and each row corresponds
# with exactly one plot.code. Name the total volume column "CWD_volume".

# 2. Join the Tree and CWD tables, and name the result "allData". Check that you
# have not lost rows (i.e., plot.codes) or metadata.

# Open vingnette on join functions that merge two tables. Read through this to
# get a roadmap for joining the tree and cwd tables.
vignette("two-table", package = "dplyr")

#=============================================================================
## tidyverse continued: ggplot

# 
dbhVScwd_plot <- 
  ggplot(data = allData, 
         mapping = aes(x = DBH_mean, 
                       y = CWD_volume, 
                       color = BurnSeverity)) + 
  geom_point()

# 
dbhVScwd_plot

# 
dbhVScwd_plot + 
  #
  scale_color_viridis() + 
  #
  #
  theme_bw() + 
  #
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        legend.title = element_blank()) + 
  #
  xlab("Mean DBH (cm)") + 
  ylab("Total CWD (cm^3)")

# 
ggplot(data = allData, 
       mapping = aes(x = DBH_mean, 
                     y = CWD_volume)) + 
  geom_point() + 
  #
  facet_wrap(~ BurnSeverity)


#=============================================================================
## Challenge 2: boxplots and facets

# 1. A boxplot of mean DBH by fire severity.

# 2. Modify the above plot so that two plots are shown, one for each value of
# the "burn" column.

#=============================================================================
## Loops

# 
sillyVector <- 1:100

# 
sillyVector_out <- c()

# 
for (i in 1:length(sillyVector)) {
  
  # 
  sillyVector_out[i] <- sillyVector[i] * 3
  
}

# 
sillyVector_out

# 
sillyVector_outNew <- 
  vector("numeric", 
         # 
         length = length(sillyVector))

# 
for (i in 1:length(sillyVector)) {
  
  # 
  sillyVector_outNew[i] <- sillyVector[i] * 3
  
}

# 
sillyVector_outNew

# 
sillyDataframe <- 
  data.frame(ID = sillyVector,
             Group = rep(1:10, each = 10))

# 
sillyList <- 
  vector("list", 
         #
         length = length(unique(sillyDataframe$Group)))

# 
for (i in 1:length(unique(sillyDataframe$Group))) {
  
  #
  groups <- sort(unique(sillyDataframe$Group))
  
  #
  temp <- 
    sillyDataframe %>%
    filter(Group == groups[i])
  
  #
  temp$LetterCol <- NA
  
  for (j in 1:nrow(temp)) {
    
    #
    temp[j , "LetterCol"] <- LETTERS[j]
    
  }
  
  #
  sillyList[[i]] <- temp
  
}

#
names(sillyList) <- fruit[1:length(sillyList)]

# 
sillyList

#=============================================================================
## Challenge 3: Looping through models

# List of models
modList <- 
  list(Global = "DBH_mean ~ BurnSeverity + YearsSinceFire + CWD_Volume",
       Severity = "DBH_mean ~ BurnSeverity",
       YSF = "DBH_mean ~ YearsSinceFire",
       CWD = "DBH_mean ~ CWD_Volume",
       Null = "DBH_mean ~ 1")

# Examples of running one linear regression. These two lines will output the
# same model.
lm(DBH_mean ~ BurnSeverity + YearsSinceFire + CWD_Volume, data = allData)
lm("DBH_mean ~ BurnSeverity + YearsSinceFire + CWD_Volume", data = allData)

# 1. Using the joined Tree/CWD table, create a loop to run the models provided
# in the Rscript and outputs a list where each element in the list is a model
# object.

# 2. This isn't a loop, but it's useful addendum: give each model in the list a
# descriptive name.

#=============================================================================
## Functions

# 
?lm

# 
sillyFunction <- 
  function(mod, myData) {
    
    #
    fit <- lm(mod, data = myData)
    
    #
    fitSummary <- summary(fit)
    
    # 
    return(list(ModelFit = fit,
                ModelSummary = fitSummary))
    
  }

# 
sillyFunction(mod = "DBH_mean ~ 1",
              myData = allData)

# 
badFunction <- 
  function(mod, myData) {
    
    #
    fit <- lm(mod, data = myData)
    
    #
    fitSummary <- summary(FIT)
    
    # 
    return(list(ModelFit = fit,
                ModelSummary = fitSummary))
    
  }

# 
badFunction(mod = "DBH_mean ~ 1",
            myData = allData)

# 
debug(badFunction)
badFunction(mod = "DBH_mean ~ 1",
            myData = allData)


#=============================================================================
## Challenge 4: Custom function for running lists of models

# 1. Using the loop and naming code you wrote in the previous challenge, create
# a function that can run any number of linear regression models through a loop
# and then name them.