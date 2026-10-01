
# List of packages necessary to run this script:
require(librarian, quietly = TRUE)
shelf(tidyverse, cowplot,
      lterdatasampler, # For LTER data
      sf, # For geospatial analyses and plotting
      ncf, # For spline.correlog
      nlme, # For gls
      lib = tempdir(),
      quiet = TRUE)

# Read in data files
data("nwt_pikas")
data("ntl_icecover")

#==============================================================================
## Example 1: Peaked pikas

# Read the data description for the [nwt_pikas
# datset](https://lter.github.io/lterdatasampler/reference/nwt_pikas.html), and
# then **do a bit of data exploration on your own.** For example:
#
# - Check out the ranges of elev_m and concentration_pg_g.
# - What are UTMs? What do they mean, and what is the rough location they telling us the pikas are?
#
# Okay, now let's create our first maps to explore the data further and look for
# any obvious signs of spatial autocorrelation:

### Challenges:

# Convert to sf object


# Map the pika locations with variables of interest varying by size/color


# Plot maps together


# Do you see any evidence of spatial autocorrelation in these maps? **On your
# own, try changing the color scales, the dot sizes, etc. to further
# investigate.**

# Visualizing spatial patterns is **critical**, but we often need more
# rigor--and specific information on distances and directionality of
# autocorrelation. There are multiple tests for spatial autocorrelation (some
# popular ones being Moran's I and semivariograms). However, one of my favorites
# is the spline correlogram because 1) it's easy to run, and 2) it gives just as
# much (and maybe more) information as other methods. The downside is that,
# depending on the size of your data, it can take a *long* time to run.

# **On your own, run the spline correlogram code below and interpret the plots.**
# If you need help understanding the plots, read the ?spline.correlog help page
# and/or our Zuur et al. (2009) book (page 480).



# Spline Correlograms using the "ncf" package.
# NOTE: depending on your data, these can take a LONG time. Beware.
pika_elev_splcor <-
  spline.correlog(x = nwt_pikas$utm_easting,
                  y = nwt_pikas$utm_northing,
                  z = nwt_pikas$elev_m,
                  resamp = 500) # Number of bootstrap samples.

# Plot the spline correlograms!
plot(pika_elev_splcor)

### Repear this for 'conc'


#=============================================================================
## Example 2: Thawing lakes

# Read the data description for the [ntl_icecover
# datset](https://lter.github.io/lterdatasampler/reference/ntl_icecover.html),
# and then **do a bit of data exploration on your own.** For example:

# - How many years of data are there??
# - How many lakes are *actually* in the data?
# - Can you tell which months are most common in the "ice_off" column?

# At this point, you may be thinking to yourself "uh, dealing with dates in R is
# super annoying." Your feelings are valid. However, there are some ways to make
# dates less annoying. For instance, converting dates to the POSIX* format (see
# ?POSIXlt):



# Convert the "ice off" and "ice_on" dates to the POSIX* format and extract Julian date
ntl_icecover <- 
  ntl_icecover %>%
  mutate(ice_off_julian = as.POSIXlt(ntl_icecover$ice_off,
                                     format = "%d%b%y")$yday,
         ice_on_julian = as.POSIXlt(ntl_icecover$ice_on,
                                     format = "%d%b%y")$yday) 

# Print the data.frame to see what we did:
head(ntl_icecover)

# Now that we've done that, first make sure you understand what "Julian date" is. 

# Okay, let's check out the patterns in how long ice is staying on the lakes
# over time, and when the lakes are thawing over time:

# Plot ice duration by lake using an 'lm' geom_smooth. Use different colors for
# the different lakes.


# Do the same for Julian day

# Plot patterns together



# Looks like some clear patterns! However, you may notice a few things:

# - These patterns are repeated measurements from only two lakes!
# - There seems to be some inter-annual patterns over time. **Describe them to yourself.**

# As with the maps, I hope you can visually detect the temporal autocorrelation in these lake data. There are ways to statistically test for temporal autocorrelation, but unlike our spatial example, they're very quick to run! Check out this code using the acf() "autocorrelation function" and ccf() "cross correlation function". On your own, interpret the resultant plots.


# Plot temporal auto-correlation for ice duration at Lake Mendota:
acf_df <- # Prepare for acf
  ntl_icecover %>%
  na.omit() %>%
  filter(lakeid == "Lake Mendota") %>%
  select(ice_duration) %>%
  as.matrix()
acf(acf_df[ , 1]) # Run acf

# Cross-correlation for ice duration and ice off
ccf_df <- 
  ntl_icecover %>%
  na.omit() %>%
  filter(lakeid == "Lake Mendota") %>%
  select(c("ice_duration", "ice_off_julian")) %>%
  as.matrix()
ccf(x = ccf_df[ , 1], # ice duration
    y = ccf_df[ , 2], # ice off julian
    ylab = "Cross-correlation")



# **On your own, interpret these acf and ccf plots.** In particular, ask yourself:
  
  # - On the plots, what does the blue dashed line mean?
  # - What does "lag" mean?
  # - What do the y-axis values mean?
  # 
  # Discussion questions:
  
# 1. If you were trying to model pika stress levels via the nwt_pikas dataset,
# how might the spatial autocorrelation you detected affect your conclusions/the
# model outputs?
  
# 2. If you were trying to model ice duration change over time, how might the
# temporal autocorrelation you detected affect your conclusions/model outputs?