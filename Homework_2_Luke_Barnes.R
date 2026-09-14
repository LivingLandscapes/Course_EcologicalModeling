# List of packages necessary to run this script:
require(librarian, quietly = TRUE)
shelf(tidyverse, # for tidying data
      mgcv, #forgg.gam
      quiet = TRUE,
      lib = tempdir())

# Load data:
bleach <- 
  read.csv("https://github.com/LivingLandscapes/Course_EcologicalModeling/raw/master/data/bleach.csv")

#####################################################################################
## Homework questions

#1. Create two models:
  #a. a linear regression model ( function = lm() ) relating LOGBLEACH to SST,
  #with LOGBLEACH being the response variable. 
#b. a generalized linear model ( function = glm() ) relating MASS to SST, with 
#MASS being the response variable.+ NOTE: for this model, you will need to 
#decide on the family and link function.

# a. Create a linear regression model that relates LOGBLEACH to SST, with LOGBLEACH
# being the response variable
Model1 = lm(LOGBLEACH ~ SST, data = bleach)

# b. Create a generalized linear model that relates MASS to SST, with MASS being
#the response variable
Model2 = glm(MASS ~ SST, data = bleach, family = binomial(link="logit"))

#####################################################################################
#2. Assess model assumptions (i.e., check model diagnostics) for both models.
plot(LOGBLEACH ~ SST, data= bleach)
plot(Model1)
plot(Model2)

a. Linear regression: revisit the Linear models and probability distributions 
#lab for information on how to do this.
#Assumptions: 
#  Relationship should be roughly linear (scatterplot) - Seems good
#Residuals must be normally distributed (diagnostics plot) - Looks good, 
#residuals seem equally distributed along horizontal line/no patterns
#No heteroskedasticity (variance must be ~ equal) - There does appear to be some
#heteroskedasticity.
#Data must be independent
#If doing multiple regression (multiple independent variables), no 
#multicollinearity (strong correlation between 1 or more of you independent variables).


#b. Generalized linear model: depending on your family/link choices, model diagnostics that assume normality may be useless. Instead, check out the mgcv::qq.gam function documentation and examples. Also, check out [Ben Bolker's logistic regression walkthrough](https://bbolker.github.io/stat4c03/notes/logistic.pdf), especially page 5.
    #Assumptions:
    #Relationship should be roughly linear (scatterplot) - Looks good
    #Data must be independent
    #Distribution requirements differ from linear models. If you're data is 
#binomially distributed you're not going to have a normal distribution, but GLMs 
#can accomodate this unlike regular LMs (specify the family that matches your 
#data in the model)
    #Same goes for heterogeneity of variance, if your data is binomial you're 
#going to have different variances, but again, GLMs can accomodate this unlike 
#regular LMs
    
######################################################################################
#results for linear model
summary(Model1)
#AIC to compare linear model and generalized linear model
AIC(Model1)
#Results for generalized linear model
summary(Model2)
#Get a summary of the residuals of the generalized linear model. Look at median, since it is close to 0 that suggests the model is not biased in one direction or the other.
summary(residuals(Model2))

#3. Summarize results from both models, as well as your model diagnostics. 
#Be careful to differentiate the linear regression results from the generalized 
#linear model results.

#Model1 summary:
  #SST is statistically sigificant (p<0.05). When x is = to 0, LOGBLEACH is 
#predicted to be 0.69681. For every 1 unit increase in SST, LOGBLEACH increases 
#by 1.32 units. R^2 = 0.85 (I prefer Adj. R^2, but it is .84 so interpretation 
#remains the same), meaning that SST explains 85% of the variance in the LOGBLEACH.
#Standard Error = .14, 

#Model2 summary:
  #SST is statistically significant (p=0.05). When x is = to 0, there is a 
#baseline probability of 0.076 of observing a mass bleaching event. For every 1 
#unit increase in SST, the odss of observing a mass bleaching event increase 
#e^8.612 times (5,497.232 fold increase). . It has an AIC of 13.88, which is 
#greater than Model1's (-7.027). Going by AIC alone would suggest that Model1 
#is most parsimonious. The median residual deviances are close the 0, suggesting
#there is no bias in one direction or the other. The residual deviance is also 
#lower than the null deviance, suggesting it is more appropriate than an 
#intercept-only model. 

######################################################################################
#4. The authors log transformed the bleaching variable “… to generate linear 
#relationships with temperature.” In R, back transform LOGBLEACH to the original 
#percentage scale, and plot against SST (No need to re-run the model). Does a 
#log-transformation make ecological sense, given the type of process and the data?

#a.
plot(10^(bleach$LOGBLEACH) ~ bleach$SST)
#b.
hist(bleach$LOGBLEACH)
hist(10^(bleach$LOGBLEACH))
hist(bleach$SST)
hist(bleach$MASS)

#c.
#A log transformation does not make much sense to me in this scenario. If there 
#was a large range of values, say for 1% all the way to 90%, with only a few 
#values being extreme, I might then consider a log transformation. But, 
#considering there's not a wide range of values and there's not a few extreme 
#observations, I would not log-transform the data. It also make the results of 
#your model less interpretable/biologically meaningful and you have to go
#through extra steps to make it so (back-transforming). The log transformation 
#also appears to make the data trimodal, which seems less beneficial.

######################################################################################
#5. At what range of values does the linear regression model (still using the 
#LOGBLEACH response variable) predict 50% bleaching will occur? Provide an 
#estimate of uncertainty for this prediction (Hint: use the predict.lm() 
#function with options newdata and interval=”prediction”. See help(predict.lm) ). 

#What is the minimum LOGBLEACH value that corresponds to 50% bleaching on the 
#normal scale?
log10(50) #1.69897
#What is the maximum LOGBLEACH value that corresponds to 100% bleaching on the 
#normal scale?
log10(100) #2

#Make a prediction out of a broad range to then narrow down our range to a more 
#appropriate/relevant range
newdatam1=seq(0,5, by = 0.1)
Predictions_Init=predict.lm(Model1, data.frame(SST = newdatam1), interval="prediction")
range(newdatam1[which(Predictions_Init[,3]>=1.69897 & Predictions_Init[,2]<=2)])

#minimum SST is ~0.5(upper confidence interval), maximum (biologically possible
#since you can't have more than 100% bleaching) is ~1.4 
#(lower prediction interval), so let's narrow it down:
#Store our range of potential values
newdata2m1=seq(0.4, 1.5, by = 0.01)

#Make predictions of LOGBLEACH with SST = to our "newdata2" range
Predictions=predict.lm(Model1, data.frame(SST = newdata2m1), interval="prediction")

#See what range of SST values corresponds to >=50% bleaching, but does not 
#exceed the biologically possible 100% bleaching
range(newdata2m1[which(Predictions[,3]>=1.69897 & Predictions[,2]<=2)])
#50% bleaching is predicted (at the highest prediction interval) to begin at 
#SST = .46; 100% bleaching is predicted (at the lowest prediction interval) to 
#be reached at SST = 1.41.

#50% bleaching is predicted (at the highest prediction interval) to begin at SST 
#= .46; 100% bleaching is predicted (at the lowest prediction interval) to be
#reached at SST = 1.41. So the range of values is .46:1.41

######################################################################################
#6. Given the generalized linear model results, at what value of SST is there a 
#50% chance of a mass bleaching event occurring? Provide an estimate of 
#uncertainty for this prediction. (Hint: check out the differences between 
#?predict.lm() and ?predict.glm(). I suggest setting 'type = link', 
#'se.fit = TRUE', and transforming the resultant predictions into probabilities 
#using the plogis() function).

#Make a prediction out of a broad range to then narrow down our range to a more 
#appropriate/relevant range
newdatam2=seq(0,5, by = 0.1)

#Initial broad predictions
Predictions_Init=predict.glm(Model2, data.frame(SST = newdatam2),type="link", 
                             se.fit = TRUE)

#Upper estimates of predictions
Predictions_Init_Upper=Predictions_Init$fit + (1.96 * Predictions_Init$se.fit)
#Lower estimates of predictions
Predictions_Init_Lower=Predictions_Init$fit - (1.96 * Predictions_Init$se.fit)

#See where model predicts >50% chance of mass bleaching event.
min(newdatam2[which(plogis(Predictions_Init$fit)>.5)]) #.3

#See which SST values produce >50% chance of mass bleaching event in either the 
#upper end (conservative) of its estimated/predicted y value.
min(newdatam2[which(plogis(Predictions_Init_Upper)>.5)]) #minimum SST to reach 
#>50% probability of mass bleaching event with 95% confidence interval is .1

#Narrow it down
#Store our range of potential values
newdata2m2=seq(0, .35, by = 0.01)

#Make predictions of LOGBLEACH with SST = to our "newdata2" range
Predictions=predict.glm(Model2, data.frame(SST = newdata2m2),type="link",
                        se.fit = TRUE)

#Upper (conservative) estimates of predictions
Predictions_Upper=Predictions$fit + (1.96 * Predictions$se.fit)
#Lower estimates of predictions
Predictions_Lower=Predictions$fit - (1.96 * Predictions$se.fit)

#Where model predicts 50% chance of mass bleaching event
min(newdata2m2[which(plogis(Predictions$fit)>.5)]) #.3 SST

#See minimum SST value that produces >50% chance of mass bleaching event in the 
#upper end (conservative) confidence interval.
min(newdata2m2[which(plogis(Predictions_Upper)>.5)]) #minimum SST to reach >50% 
#probability of mass bleaching event with 95% confidence interval is .08.

#The model estimates that >50% bleaching occurs at .3 SST. The minimum SST value 
#that produces >50% chance of a mass bleaching event in the upper interval 
#(conservative) confidence interval is .08

######################################################################################
#7. Choose your favorite model and create a figure showing the relationship 
#between the response and predictor. Include confidence intervals. There are 
#multiple ways to create predicted effects plots (e.g., ggeffects R package, 
#use the base R package 'predict' function). **But** be careful that the 
#confidence intervals are correct (see hint in question #6)!

pred_df <- 
  data.frame(
    as.data.frame(predict(Model1, SST = newdata2m1, se.fit = TRUE))) %>%
  mutate(CI_0.975 = fit + (se.fit * 1.96),
         CI_0.025 = fit - (se.fit * 1.96))
pred_df$SST=bleach$SST
pred_df$prob=plogis(pred_df$fit)
# Plot the effects!
ggplot(pred_df, 
       aes(x = SST,
           y = prob,
           ymin = CI_0.025,
           ymax = CI_0.975)) + 
  geom_ribbon(alpha = 0.3,
              color = "red") + 
  geom_line() + 
  theme_bw() + 
  ylab("Probability of Mass Bleaching Event")
