################ 
# Cleaning the PineRidge data a bit



# List of packages necessary to run this script:
require(librarian, quietly = TRUE)
shelf(tidyverse, 
      # cowplot, 
      # performance, 
      brms,
      # MuMIn,
      # StanHeaders,
      lterdatasampler, # For LTER data
      lib = tempdir(),
      quiet = TRUE)

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
  mutate(YearsSinceFire = case_when(burn == "D" ~ "10",
                                    burn == "FR" ~ "27",
                                    .default = ">27"),
         #
         CoverType = str_sub(severity, 1, 1),
         #
         BurnSeverity = str_sub(severity, 2, 2))


# # 
# trees_selected <- 
#   trees_selected %>%
#   filter(decay.class == "L" & species == "PP") %>%
#   # 
#   select(-severity, - decay.class, -species)

# 
trees_summarized <- 
  trees_selected %>%
  na.omit() %>%
  # 
  filter(decay.class == "L") %>%
  group_by(plot.code, burn, CoverType, BurnSeverity, YearsSinceFire) %>%
  #
  summarize(DBH_mean = mean(dbh.cm, na.rm = TRUE),
            N_trees = n()) %>%
  # 
  ungroup()

cwd_summarized <- 
  cwd_raw %>%
  mutate(YearsSinceFire = case_when(burn == "D" ~ "10",
                                    burn == "FR" ~ "27",
                                    .default = ">27"),
         #
         CoverType = str_sub(severity, 1, 1),
         #
         BurnSeverity = str_sub(severity, 2, 2)) %>%
  select(plot.code, 
         CoverType, BurnSeverity, YearsSinceFire, burn,
         line.cover.cm, diameter1.width.cm, diameter2.height.cm) %>%
  mutate(CWD_volume = line.cover.cm * diameter1.width.cm * diameter2.height.cm) %>%
  group_by(plot.code, CoverType, BurnSeverity, YearsSinceFire, burn) %>%
  summarize(CWD_volume_sum = sum(CWD_volume, na.rm = TRUE))

pineRidge <- 
  left_join(cwd_summarized,
            trees_summarized) %>%
  filter(BurnSeverity != "B") %>%
  mutate(CWD_volume_sum = ifelse(is.na(CWD_volume_sum), 0, CWD_volume_sum),
         DBH_mean = ifelse(is.na(DBH_mean), 0, DBH_mean),
         N_trees = ifelse(is.na(N_trees), 0, N_trees),
         Severity_Num = as.numeric(factor(BurnSeverity,
                                          levels = c("U", "L", "M", "H"))))
  
write_csv(pineRidge,
          "~/GitHub/Course_EcologicalModeling/data/PineRidge_Trees_cleaned.csv")  

pineRidge <- read_csv("~/GitHub/Course_EcologicalModeling/data/PineRidge_Trees_cleaned.csv")

#====================

mods <- 
  list(Global = DBH_mean ~ Severity_Num + YearsSinceFire + CWD_volume_sum + N_trees,
       Global2 = DBH_mean ~ BurnSeverity + YearsSinceFire + CWD_volume_sum + N_trees,
       Severity_YSF = DBH_mean ~ Severity_Num + YearsSinceFire,
       Severity_YSF2 = DBH_mean ~ BurnSeverity + YearsSinceFire,
       Biotic = DBH_mean ~ CWD_volume_sum + N_trees,
       Severity = DBH_mean ~ Severity_Num,
       Severity2 = DBH_mean ~ BurnSeverity,
       Null = DBH_mean ~ 1)
# fits <- 
#   lapply(mods, 
#          function(X) {lm(X, data = pineRidge)})
# names(fits) = names(mods)
# model.sel(fits)
# summary(fits$Severity_YSF)

# ## 
# fits_brms <- 
#   lapply(mods[1:3], 
#          function(X) {
#            brm(X, 
#                family = gaussian,
#                iter = 500,
#                chains = 3,
#                cores = 3,
#                data = pineRidge)
#            })
tst <- 
  brm(DBH_mean ~ 1, 
      family = gaussian,
      iter = 500,
      chains = 1,
      cores = 3,
      data = pineRidge)
