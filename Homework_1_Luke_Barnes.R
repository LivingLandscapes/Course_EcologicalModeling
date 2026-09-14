# List of packages necessary to run this script:
require(librarian, quietly = TRUE)
shelf(tidyverse, sf, vegan, quiet = TRUE)

# Load riparian vegetation monitoring dataset:
dat <- read.csv("https://github.com/LivingLandscapes/Course_EcologicalModeling/raw/refs/heads/master/data/rip.dat.02-14.csv")

dat=na.omit(dat)

#######################################################################################
#1. Modify dataframe
#######################################################################################
dat_rev <-
  dat %>%
  #1a.Revise the site column to be the first three characters in the current site
  #column
  mutate(site = str_sub(site, 1, 3),
  #1b.Create a new column named plotCode with the final 1 - 2 characters in the
  #current site column.#Have to either use the original dataframes site column or
  #reverse the order in which we mutate, since the current order mutates "site" where
  #there is no 4th or 5th character in the string.        
  plotCode = str_sub(dat$site, 4, 5))

#######################################################################################
#2. Calculate species richness and inverse simpson's diversity index
#######################################################################################
#2a.
#easy way using vegan package's functions:
plants=
  dat_rev %>%
  select(ACLE:VIAMM)
dat_rev$Richness_total=specnumber(plants)

#2b. 
#easy way using vegan package's functions
dat_rev$Diversity_InvSimp = diversity(plants, index = "simpson", inverse=TRUE)

#######################################################################################
#3.Look for outliers in species richness and inverse simpson's index using boxplot and
#histogram
#######################################################################################
#Doesn't appear to be any outliers for species richness
boxplot(dat_rev$Richness_total)
#Right skewed, but continuous, so does not appear to be any outliers for species
#richness
hist(dat_rev$Richness_total, breaks=20)

#Looks like 3 potential outliers for Inverse Simpson's Index
boxplot(dat_rev$Diversity_InvSimp)
#Looks like there's a break where there could be some outliers for Inverse Simpson's
#Index
hist(dat_rev$Diversity_InvSimp, breaks=20)

#######################################################################################
#4.Use table() to determine if data is balanced across site, year, and treatment
#######################################################################################
#Look at all 3 at once: Does not look balanced; Treatment C seems to have more
#observations, site RS1 has a lot more observations as well, and RS7 and RS* seem to
#have much fewer observations than other sites.
table(dat_rev[,1:3])
#Look at just year: Looks pretty balanced; 2002 is a little underrepresented, but not
#by much
table(dat_rev$year)
#Look at just site: Does not look balanced; RS1 is over represented and RS7 and RS8 are
#underrepresented
table(dat_rev$site)
#Look at just treatment: Does not look balanced; treatment C is overrepresented.
table(dat_rev$treatment)

#######################################################################################
#5.Plots to visualize relationships between response and explanatory variables
#######################################################################################

#Response variables:
#Looks like some collinearity between species richness and simpson's diversity. Looks
#like a positive ~linear relationship between the two.
plot(dat_rev$Richness_total ~ dat_rev$Diversity_InvSimp)

#Relationships between richness and explanatory variables
#There seems to be a relationship between richness and site, it varies from site to
#site in both medians and variation.
plot(dat_rev$Richness_total ~ dat_rev$site)
#Same as before, there is year to year variation
plot(dat_rev$Richness_total ~ dat_rev$year)
#Treatment does not seem to have much of an effect. The spread of the data and the
#medians are pretty similar across all 3 treatments.
plot(dat_rev$Richness_total ~ dat_rev$treatment)

#Relationship between inverse of simpson's diversity index and explanatory variables
#Same as for richness, there seems to be variation in diversity from site to site.
plot(dat_rev$Diversity_InvSimp ~ dat_rev$site)
#There is some year to year variation, but not as much as seen in richness
plot(dat_rev$Diversity_InvSimp ~ dat_rev$year)
#There is some variation in the spread of the data, but the medians seem very similar
plot(dat_rev$Diversity_InvSimp ~ dat_rev$treatment)

#######################################################################################
#6. Worry about collinearity or no?
#######################################################################################
#I wouldn't use both variables in a model since they are collinear. Simpson's diversity
#index includes richness in it's calculation, so it is pretty dependent on it. I would
#worry about overfitting my model by including both when they are an attempt to explain
#the same thing. 

#######################################################################################
#7.Plot year and treatment to look for a relationship
#######################################################################################
#This just visually demonstrates what we saw by tabling treatments. You can see that
#treatment C is overrepresented and there is imbalance between the treatments across
#years. I don't know that you would call this an interaction, it doesn't really vary
#much by year, there just isn't equal representation of treatments.This would also be 
#something the researchers controlled, rather than something biologically driven or 
#some natural phenomenon
mosaicplot(table(dat_rev$year, dat_rev$treatment), 
main = "Mosaic Plot", 
xlab = "Year", 
ylab = "Treatment")

#######################################################################################
#Can ignore: I wanted to try out the hard way to calculate richness by hand 
#######################################################################################
#Make year a character so my loop does not recognize it as numeric
#dat_rev$year = as.character(dat_rev$year) 
#Initialize the Richness_total column so the loop can feed the calculated values into 
#the rows of the new column
#dat_rev$Richness_total = 0
#for loop that will sum up the number of species columns that have at least one 
#observation  
#for(i in 1:(nrow(dat_rev))){
#  for(j in 1:(ncol(dat_rev)-1)){
#  if (is.numeric(dat_rev[i,j]) == TRUE & dat_rev[i,j] > 0) {
#    dat_rev$Richness_total[i] = dat_rev$Richness_total[i] + 1
#  } else {
#    dat_rev$Richness_total[i] = dat_rev$Richness_total[i] + 0
#  }}
#}


#Ignore; I was trying to do the calculation by hand, but somewhere my logic is off 
#because my calculation does not = diversity()'s calculation :(
#dat_rev$Diversity_InvSimp_Test = 0
#  for(i in 1:(nrow(dat_rev))){
#    littlen = 0
#    bign = 0
#    for(j in 1:(ncol(dat_rev)-1)){
#      if (is.numeric(dat_rev[i,j]) == TRUE & dat_rev[i,j] > 0) {
#        littlen = littlen + ((dat_rev[i,j])*(dat_rev[i,j]-1))
#        bign = bign + dat_rev[i,j]
#      } else {
#        littlen = littlen + 0
#      }}
#    dat_rev$Diversity_InvSimp_Test[i] = (1 / (littlen/ (bign * (bign - 1))))
#  }
