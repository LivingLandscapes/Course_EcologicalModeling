# # Clear environment?
# rm(list=ls())

# List of packages necessary to run this script:
require(librarian)
shelf(tidyverse, here, sf, brms, maps, terra, tidybayes,
      lib = tempdir())

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
  mutate(YearNum = Year - 1964,
         YearNum2 = (Year - 1964)^2,
         Easting_scaled = scale(UTMYE),
         Northing_scaled = scale(UTMXN))

#=============================================================================
## Frequentist

fit_lm <-
  lm(WhistleCount ~ YearNum + YearNum2 + Easting_scaled * Northing_scaled, 
     data = whistle)
summary(fit_lm)
plot(fit_lm)
fit_glm <-
  glm((WhistleCount + 0.0001)  ~ YearNum + YearNum2 + Easting_scaled * Northing_scaled, 
      family = Gamma("log"),
      data = whistle)
summary(fit_glm)
plot(fit_glm)

#=============================================================================
## Bayesian

# Let students run a model.

# Work through brm function.
fit_brm <- 
  brm(bf(WhistleCount  ~ YearNum, 
         hu = ~ YearNum),
      family = hurdle_lognormal(),
      data = whistle,
      chains = 3,
      iter = 2000,
      threads = 3)
fit_brm
plot(fit_brm)
nd <- 
  data.frame(YearNum = sort(unique(whistle$Year)),
             YearNum2 = sort(unique(whistle$YearNum2)))
pred_df <- 
  predict(fit_brm, 
          nd,
          # type = "response",
          probs = c(0.025, 0.975))
head(pred_df)
