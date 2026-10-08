###############################################################################
#### Linear mixed models exercise code
###############################################################################


# List of packages necessary to run this script:
require(librarian, quietly = TRUE)
shelf(tidyverse, cowplot, performance, 
      AICcmodavg, # for model selection
      lme4, # For mixed modeling
      # lmerTest, # Loading this package forces lme4 to print p-values.
      DHARMa, # For checking mixed model residuals, etc.
      pander,
      broom.mixed,
      lib = tempdir(),
      quiet = TRUE)

# Set the web address where R will look for files from this repository
# Do not change this address
repo_url <- "https://raw.githubusercontent.com/LivingLandscapes/Course_EcologicalModeling/master/"

# Load datasets
# NOTE: 'sleepstudy' data comes loaded in lme4
mice <- read.table(paste0(repo_url, "/data/mice.txt"), 
                   header = TRUE)

#=============================================================================
## Exploring the Sleep Study

### Simple linear regression

# First, let's simply model the response of reaction time to number of days
# without sleep. **On your own:**

# - Do some quick data exploration (e.g., familiarize yourself with the data 
#   columns, their ranges, etc.)
# - Interpret the results of the linear regression below.


# Simple linear regression between reaction time and number of days without
# sleep. Check out the summary.
ss.lm <- lm(Reaction ~ Days, sleepstudy)
summary(ss.lm)

# Hopefully, you will have determined that (unsurprisingly) reaction time tends
# to slow as days without sleep increased. However, you will also hopefully have
# noticed that there were several "subjects" of the experiment and that each
# subject's response times were measured multiple times... Do we think there's
# any variation between subject response times? Let's check it out:
  
ssBase <- 
  ggplot(sleepstudy, 
         aes(x = Days, 
             y = Reaction)) + 
  geom_point(aes(color = Subject)) + 
  scale_color_viridis_d() + 
  labs(x="Days with no sleep",
       y="Reaction time [ms]") + 
  geom_smooth(method = "lm", formula = y ~ x)
ssBase


# Yep, look like some variation to me! Let's plot the relationship for each
# subject individually and see what happens:

### Challenge #1

# 1. Create a single ggplot with individual regression lines for each individual
# subject.

ggplot(sleepstudy,
       aes(x = Days,
           y = Reaction,
           group = Subject)) + 
  geom_point(size = 1, 
             color = "orange") + 
  geom_smooth(method = "lm") + 
  facet_wrap(~Subject) + 
  theme_bw() + 
  ylab("Reaction time (ms)") + 
  xlab("Days without sleep")

# 2. What do you notice from these individual regressions?



# We could fit a separate model to each subject and combine them. This is
# effectively a Subject * Days interaction model.

# Fit a separate linear regression to each subject. The `lmList()` function fits
# a different linear model to each subset of a data.frame defined by the
# variable after the “|”, in this case “Subject”.
fm1 <- lme4::lmList(Reaction ~ Days | Subject, sleepstudy)

# Get the coefficients from the list of linear regressions:
coef(fm1)

### Challenge #2:

# 1. Calculate the mean intercept and Days coefficients from the matrix created
# by `coef(fm1)`.

colMeans(coef(fm1))

# 2. Compare these with the estimates from the single regression model. 

coef(ss.lm)

# 3. Use `summary(fm1)` to see the residual variance and degrees of freedom for
# the pooled list. Compare these values with the residual variance and degrees
# of freedom from the single regression model.

#==============================================================================
## Our first mixed model: sleep study

# We can use a mixed model approach to get the best of both worlds - an estimate
# of the population average intercept and slope, and an idea of how much
# variation there is among individuals. The function lmer() in package lme4 is
# the method of choice here. 

### Challenge #3:

# 1. Fit your first linear mixed effects model using "lme4::lmer" and creating a
# random effect structure to reflect the patterns we saw above.

ss.lmer <- 
  lme4::lmer(Reaction ~ Days + (Days | Subject),
           data = sleepstudy,
           REML = TRUE)
summary(ss.lmer)

# 2. Compare the estimates and standard errors with those from the "lmList" and
# "lm" models above.**


# We can also get the random perturbations for each group, and combining those
# with the population level coefficients gives us the coefficients for the line
# in each group, as shown in the following table. When the random effects (shown
# in the left 2 columns) are negative, the coefficients for the group are less
# than the population coefficients, and when they are positive, the group
# coefficients are greater.
cbind(ranef(ss.lmer)$Subject, coef(ss.lmer)$Subject)

#=============================================================================
## Mouse Example 

# Let's look at the breakdown of the relationship between ear size and foot
# length by geographic site and sex. Do some quick data exploration of the
# 'mice' data to familiarize yourself with it. Also, please ignore that a normal
# distribution may or may not be the best for this data. Now, check out the
# faceted plot below:

# Remove rows with unknown sex.
mice <- filter(mice, sex != "U")

# Plot relationship
basemouse <- 
  ggplot(mice,
         aes(x = foot, y = ear, color = sex)) +
  geom_point(alpha = 0.2) +
  xlab('Foot Length [mm]') + ylab('Ear Length [mm]')

basemouse +
  geom_smooth(method = "lm") +
  facet_wrap( ~ site)
   
# A few things are obvious from this plot. First, there is a lot of data at a
# few sites, and much less at others. Some of the sex size effects are clear in
# a few sites (two groups of points), but much less clear in others. But most
# interesting is the variation in slope. In many sites the bigger the feet the
# bigger the ears. But not everywhere! Put it all in one plot, adjusting the
# alpha level so overplotting is clearer.
 basemouse +
   geom_smooth(aes(group = site),
               method = "lm", 
               se = FALSE)

#=============================================================================
### Mixed model diagnostics

# So we want to check a model that lets the relationship between feet and ears
# vary by species and sex. The sites are a random sample of possible places we
# could look for mice, so we'll treat site as a random effect. We can also see
# that the slope of the effect varies a lot between sites, so we'll let the
# coefficient of foot vary between sites too, at least initially. Here's the
# global model, and check the residuals:
  
# Global mixed mouse model!
mice.global <- 
  lmer(ear ~ foot * species * sex + (foot | site),
       data = mice)

# Check residuals with DHARMa
simulateResiduals(mice.global, plot = TRUE)


# How do the simulated residuals look for you? Given they're simulated, they
# could look a bit different each time you run them. However, the significant KS
# test indicates we may not be using the best probability distribution (he says
# sheepishly). Regardless, at your leisure, you can dig into the DHARMa
# vignette a bit and learn about what you're seeing:

# (https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html#general-remarks-on-interperting-residual-patterns-and-tests)

### Model selection
  
# Let's fit a range of models with different random effects structures, and
# compare using AICc. Check out this table from the lme4 package
# vignette (https://cran.r-project.org/web/packages/lme4/vignettes/lmer.pdf)
# describing random effect syntax and meaning:


# The problem with comparing different random effects using Likelihood Ratio
# tests is that our null hypothesis is "on the boundary" of the parameter space,
# because you can't have negative variances. Choosing a model that has the
# minimum AICc won't be bothered by this consideration, but this would affect
# the calculation of the weights.

### Challenge #4: 
  
# 1. Create 3 - 4 models with different (reasonable) random effect structures,
# keeping the fixed effects the same in all models.
  
mice_randInt <- 
  lmer(ear ~ foot * species + (1 | site),
       data = mice)
mice_randSlopeInt <- 
  lmer(ear ~ foot * species + (1 | site) + (1 | sex),
       data = mice)
mice_UncorrRandSlopeInt <- 
  lmer(ear ~ foot * species + (sex | site),
       data = mice)

# 2. Use AICc to determine the "best" random effect structure.

aictab(list(mice_UncorrRandSlopeInt, mice_randSlopeInt, mice_randInt))
sapply(list(mice_UncorrRandSlopeInt, mice_randSlopeInt, mice_randInt),
       AICc)

# 3. Once you've chosen your random effect structure via model selection, create 3 - 4 candidate models varying by fixed effects.

#==============================================================================
## Predictions with mixed models

# You can make predictions from these models just as you would with other
# models. Below, we have a plot showing all predicted ear/foot relationships by
# site.

# Example of final model
mice.final <- lmer(ear ~ foot + (1+foot|site),
                   REML = FALSE,
                   data = mice)

### Challenge #5: 

# Predict ear response, showing predictions for all site/foot combinations.

nd <- 
  expand.grid(foot = seq(min(mice$foot),
                         max(mice$foot),
                         0.1),
              site = unique(mice$site))
predDF <- 
  cbind(fit = predict(mice.final, nd, type = "response", re.form = NULL),
        nd)
ggplot(predDF, 
       aes(x = foot,
           y = fit, 
           group = site,
           color = site))  +
  geom_line() + 
  theme_classic()
### To be continued...

# What about confidence intervals on our predictions? A very good question, but
# one that will have to wait until next week!