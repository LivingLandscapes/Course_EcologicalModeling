##############################################################################
############## Mixed model confidence intervals exercise #####################


# List of packages necessary to run this script:
require(librarian, quietly = TRUE)
library(librarian)
shelf(tidyverse, broom.mixed, broom, lme4, boot, snow, here, parallel,
      lib = tempdir())

# Set seed
set.seed(7871)

# Set the web address where R will look for files from this repository
# Do not change this address
repo_url <- "https://raw.githubusercontent.com/LivingLandscapes/Course_EcologicalModeling/master/"

# Load data
ecervi <- 
  read.table(paste0(repo_url, "/data/DeerEcervi.txt"), 
             header = TRUE)

#=============================================================================
## Confidence intervals for LMMs

# We'll start with something easy - a linear mixed effects model with the
# familiar sleepstudy data.

ssBase <- 
  ggplot(sleepstudy, aes(x = Days, y = Reaction)) + 
  geom_point(aes(color=Subject)) + 
  scale_color_discrete() + 
  labs(x="Days with no sleep",
       y="Reaction time [ms]")
ssBase + 
  geom_smooth(method="lm") + 
  facet_wrap(~Subject) + 
  guides(color=FALSE)

# So there's the basic data, and now we fit a mixed model with both the
# intercept and Days varying across subjects.

# fit a mixed model
summary(ss.mm <- lmer(Reaction ~ Days + (1 + Days | Subject),
                      sleepstudy,REML=FALSE))



#### Profile and Wald intervals

# There are actually three ways to get confidence intervals for mixed models (in
# the frequentist framework): profile, Wald, and bootstrapping. Profile
# intervals and Wald intervals are fine, but they aren't the "gold standard".
# Bootstrapping is the gold standard for sure. Howevever, bootstrapping is much
# slower, than profile or Wald intervals, so that's a minus. Let's compare them
# all!

# profile limits
profCI <-
  as.data.frame(confint(ss.mm, 
                        parm = "Days", 
                        method = "profile", 
                        signames = FALSE))
# Wald limits
WaldCI <-
  as.data.frame(confint(ss.mm, 
                        parm = "Days",
                        method = "Wald", 
                        signames = FALSE))
# Bootstrap limits
bootCI <-
  as.data.frame(confint(ss.mm, 
                        parm = "Days",
                        method = "boot", 
                        signames = FALSE))

# get rid of rownames
rownames(profCI) <- NULL
rownames(WaldCI) <- NULL
rownames(bootCI) <- NULL

# and change names to include type of CI
names(profCI) <- c("CI2.5", "CI97.5")
names(WaldCI) <- c("CI2.5", "CI97.5")
names(bootCI) <- c("CI2.5", "CI97.5")

# Get the coefficients from the top model, add the Wald and profile intervals,
# do some data manipulation to prepare for ggplot.
profWaldBoot.CI <-
  data.frame(ConfType = c("Profile", "Wald", "Bootstrap"),
             rbind(profCI,
                   WaldCI,
                   bootCI), 
             Mean = rep(summary(ss.mm)$coefficients[2, 1], 3))

# Plot the profile and Wald confidence intervals on coefficients.
ggplot(profWaldBoot.CI,
       aes(x = ConfType,
           y = Mean,
           color = ConfType)) +
  scale_color_viridis_d(option = "plasma") +
  geom_point(position = position_dodge(width = 0.5),
             size = 3) +
  geom_errorbar(aes(ymin = CI2.5,
                    ymax = CI97.5),
                position = position_dodge(width = 0.5),
                width = 0.2) +
  geom_hline(data = data.frame(yIntercept = 0),
             mapping = aes(yintercept = yIntercept),
             color = "darkred") +
  theme_bw() + 
  xlab("Term") +
  ylab("Coefficient Estimate")

# They're all pretty much the same!

# However, there's no great way (that I know of) to make marginal effects plots
# for mixed models with Profile or Wald intervals. Only bootstrapping can do
# that!

# get fitted values
ss.fitted <- broom.mixed::augment(ss.mm)
names(ss.fitted)


# `augment()` gives us a bunch of columns for merMod objects; the one we're
# interested in here is `.fitted`. There is also a column `.mu` that has
# identical values in it. For a model that has a link function other than the
# identity link (e.g. the default logit link for the binomial family) `.fixed`
# will be on the *link* scale, and `.mu` will be on the *response* scale. We'll
# come back to that in the next example.

# x is inherited from ssBase
# if we don't specify color=Subject in geom_line()
# we only get one line ... oh, because I specified color=Subject
# in geom_points() not ggplot()
ssBase + geom_line(aes(y=.fitted, color=Subject), data=ss.fitted) + guides(color=FALSE)

# So each line represents the fitted values including the effects of the random
# perturbations on the intercept and slope. Now get the population level
# predictions using predict() and a new data.frame.

nd <- data.frame(Days=0:9)
# re.form tells predict what to do with the random effects
# ~0 says ignore them; population level predictions
nd$PopPred <- predict(ss.mm, newdata=nd, re.form=~0)
ssBase + geom_line(aes(y=.fitted, color=Subject), data=ss.fitted, alpha=0.5) + 
  geom_line(aes(y=PopPred), data=nd, linewidth =2) +
  guides(color=FALSE)


# OK, but we want to know how much confidence to have in that prediction. Really
# the only option here is to use bootstrapping. This is slow, but gets all the
# uncertainty in our prediction. Fortunately `lme4` includes a function
# `bootMer` to do bootstrapping by generating a random sample of data and then
# fitting the model to that new data. Repeat that 1000's of times and you can
# get a distribution of possible model fits. Unfortunately we have to write a
# function to extract the predictions from the random samples the way we want
# them. Start by getting a single random sample.

# bootMer takes a fitted merMod object
# then the function that takes a fitted merMod object and returns the thing we want as a vector
# fitted() will give us the coefficients from a fit
test <- bootMer(ss.mm, fixef)
# a (fairly) compact look at the *str*ucture of an object
str(test)


# There's alot of stuff in there, most of which relates to internal stuff that
# would allow us to recreate the simulation. We are mostly interested in `t0`
# and `t`. `t0` is the result of applying our FUN to the original object. In
# this case it gives us a vector of the fixed effects coefficients. `t` is a
# matrix of the simulation output. In each row is the result of our FUN applied
# to a simulated result. By default `bootMer` only does one simulation. If we
# want more:
  
test <- bootMer(ss.mm, fixef, nsim=10)
test$t

# If we set `nsim = 10000` and then took quantiles of the resulting columns, we
# could get bootstrapped confidence limits on our fixed effects. That's what is
# happening when we do `confint(ss.mm, method="boot")`. We want a function that
# makes a prediction from a fitted model. Just using `predict()` won't work,
# because the function only takes one argument, the fitted model. So we have to
# make a "wrapper" that will call `predict()` with the right arguments.

myFunc <- function(mm){
  # forgot the re.form argument on first try
  predict(mm, newdata=nd, re.form=~0)
}
myFunc(ss.mm) # works
# try with bootMer()
test <- bootMer(ss.mm, myFunc)
test$t #works


# OK, this next bit takes a while ... about 10 ten seconds on my laptop
bigBoot <- bootMer(ss.mm, myFunc, nsim=100)
head(bigBoot$t)

# Now we want quantiles of each column of that thing, turned around so the
# columns are rows like the original `nd`.

# apply(xx, MARGIN = 2, FUN, ...) 'applies' FUN to each column (because MARGIN = 2). arguments in ... get passed to FUN
# t() transposes the result
predCL <- t(apply(bigBoot$t, MARGIN = 2, 
                  FUN = quantile, 
                  probs=c(0.025, 0.975)))
# ggplot only works with data.frames so
# add to nd
# '%' in column names is a disaster so be explicit
nd$lci <- predCL[,1]
nd$uci <- predCL[,2]

ssBase + geom_line(aes(y=.fitted, color=Subject), data=ss.fitted, alpha=0.5) + 
  geom_line(aes(y=PopPred), data=nd, size=2) +
  geom_ribbon(aes(x=Days, ymin=lci, ymax=uci),
              data=nd, inherit.aes = FALSE, 
              alpha=0.2) +
  guides(color=FALSE)


# So that's pretty nice. I can add a geom_smooth() to show how much wider the
# confidence intervals are compared to ignoring all the extra variability.

ssBase + geom_line(aes(y=.fitted, color=Subject),
                   data=ss.fitted, alpha=0.5) + 
  geom_line(aes(y=PopPred), data=nd, size=2) +
  # set inherit.aes to FALSE here otherwise 
  # geom_ribbon looks for a y aesthetic 
  # called Reaction
  geom_ribbon(aes(x=Days, ymin=lci, ymax=uci),
              data=nd, inherit.aes = FALSE, 
              alpha=0.2) +
  geom_smooth(method="lm", alpha=0.8, 
              color="black") +
  guides(color=FALSE)


# It is interesting that there isn't much difference when Days == 0. Note that
# you wouldn't ever do this in practice! However, it might be nice to build a
# few polygons at different levels of confidence.

predCL <- t(apply(bigBoot$t, MARGIN = 2, 
                  FUN = quantile, 
                  probs=c(0.05, 0.95, 
                          0.1, 0.9, 
                          0.25, 0.75)))
# try making it into a dataframe
predCL <- data.frame(predCL)
# names(predCL)
# so puts a X at the front bc can't start with a number, 
# and replaces % with . 
# put them into nd
nd <- bind_cols(nd, predCL)
# brute force multi-ribbons:
ssBase + 
  geom_line(aes(y=.fitted, color=Subject),
            data=ss.fitted, alpha=0.5) + 
  geom_line(aes(y=PopPred), data=nd, size=2) +
  # set inherit.aes to FALSE here otherwise 
  # geom_ribbon looks for a y aesthetic 
  # called Reaction
  geom_ribbon(aes(x=Days, ymin=lci, ymax=uci),
              data=nd, inherit.aes = FALSE, 
              alpha=0.2) +
  # add 90% ribbon
  geom_ribbon(aes(x=Days, ymin=X5., ymax=X95.),
              data=nd, inherit.aes = FALSE, 
              alpha=0.2) +
  # add 80% ribbon
  geom_ribbon(aes(x=Days, ymin=X10., ymax=X90.),
              data=nd, inherit.aes = FALSE, 
              alpha=0.2) +
  # add 50% ribbon
  geom_ribbon(aes(x=Days, ymin=X25., ymax=X75.),
              data=nd, inherit.aes = FALSE, 
              alpha=0.2) +
  
  guides(color=FALSE)



# That doesn't look as good as I'd hoped. The idea here is to draw the eye
# closer to the expected value rather than the outer edges of the confidence
# region.

#=============================================================================
## Doing it with a GLMM

# Now, let's work with the roe deer/parasite data.

ecervi <- ecervi %>%
  mutate(Ecervi.pa = Ecervi > 0,
         fSex = factor(Sex, labels=c("Male","Female")),
         cLength = Length - mean(Length),
         csLength = cLength / sd(Length))

# Also, the scaling between Sex and Length is very different. A 1 cm change in
# length is a much smaller change than a change from male to female. This kind
# of discrepancy can make it harder for a model to converge. If I rescale
# 'Length' by dividing through with the standard deviation the coefficients for
# those two variables will be closer, and this can help with convergence. I will
# also change the contrasts on Sex to "sum to zero" contrasts, which has a
# similar beneficial effect to centering a continuous variable.
contrasts(ecervi$fSex) <- contr.sum(2)

# Let's make the model we'll use.
M0 <- 
  glmer(Ecervi.pa ~ fSex * csLength + (1 + csLength | Farm),
        data = ecervi,
        family = binomial)

# New data for prediction
lr <- range(ecervi$csLength)
nd <- expand.grid(csLength = seq(lr[1], lr[2], length=20),
                  fSex=factor(levels(ecervi$fSex)))

# Predict
nd$pp <- predict(M0,
                newdata = nd,
                type = "response", 
                re.form = ~0)

# Plot just the mean estimate
ggplot(nd, aes(x=csLength, y=pp, col=fSex)) +
  geom_line(alpha=0.5) +
  ylab("Probability of E. cervi infection") +
  geom_line(aes(x=csLength, y=pp, col=fSex), data=nd, inherit.aes = FALSE, linewidth=2) +
  geom_rug(aes(x=csLength), data=filter(ecervi, Ecervi.pa),sides="t", inherit.aes = FALSE) +
  geom_rug(aes(x=csLength), data=filter(ecervi, !Ecervi.pa),sides="b", inherit.aes = FALSE) +
  facet_wrap(~fSex) +
  guides(color=FALSE)

# OK, now we want to do the bootstrap. But fitting this model already is slow,
# so doing it 1000 times is even slower! Bootstrapping is one of the things that
# benefits from parallel computing very easily. The bit of code below sets up a
# "cluster" on my computer and splits the computation up 4 ways. 4 times faster.
# Most modern CPU's have multiple cores that can run independently. In normal
# operation R uses just one core. Depending on how many cores your computer has,
# running this code in parallel could speed up this computation by a lot.


# Make the cluster
cl <- parallel::makeCluster(4, type = "SOCK")

# Export to the cluster
clusterEvalQ(cl, library("lme4"))
clusterExport(cl, varlist = c("myFunc", "M0", "cl", "nd"))

# now we pass the cluster object to bootMer
# note that I can use the same myFunc!
bigBoot2 <- bootMer(x = M0,
                    FUN = myFunc,
                    parallel = "snow",
                    use.u = FALSE,
                    type = "parametric",
                    # ncpus = 2,
                    nsim = 50,
                    cl = cl)

stopCluster(cl) # can take a while

# # Without running parallel
# bigBoot2 <- bootMer(M0, myFunc, nsim=50)

# the object returned by bootMer is of type boot
# and there are some useful functions there.
envPred <- boot::envelope(boot.out = bigBoot2)
str(envPred)


# The matrix `point` in there is the "pointwise" 95% confidence intervals for
# each of the 40 rows in `nd`. The error rate for those intervals is 5% on each
# value, which means that 24% of the points will be outside the interval over
# the entire curve. The matrix `overall` expands those limits until the number
# of points over the entire curve is 5%. Time to add these to a plot. First we
# have to get them into the nd data.frame.

nd$lower.point <- envPred$point[1,]
nd$upper.point <- envPred$point[2,]
ggplot(nd, aes(x=csLength, y=pp, col=fSex)) +
  geom_line(alpha=0.5) +
  ylab("Probability of E. cervi infection") +
  geom_line(aes(x=csLength, y=pp, col=fSex), data=nd, inherit.aes = FALSE, size=2) +
  geom_ribbon(aes(x=csLength, ymin=lower.point, ymax=upper.point, fill=fSex),
              data=nd, inherit.aes=FALSE,
              alpha = 0.25) +
  facet_wrap(~fSex) +
  guides(color=FALSE)



# OK. That's not right! Look at the range, those values go from -5 to nearly 8.
# They can't be probabilities.

### Challenge: 

# How can we correct this issue with the confidence intervals??

# Once you've done that, make a prediction plot similar to the Shipley et al.
# (2022) paper. The figure is on the course website "Confidence Intervals for
# Mixed Models" lab.



