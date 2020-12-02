


## Importing packages
library(tidyverse) # metapackage with lots of helpful functions
library(gridExtra) # to plot multiple ggplots aside
library(car) # package for regression diagnostics
library(broom)      # helps to tidy up model outputs
library(olsrr)     #For STEPWISE regression
library(modelr)
library(rms) #for VIF 


## Reading in files
insurance <- read.csv("https://raw.githubusercontent.com/hantswilliams/AHI_STATS_507/main/Week12_Regression/Dataset/insurance.csv")


## Lets check out the data: 
head(insurance) #First 5 
str(insurance) #The variable types   // insurance.dtypes
summary(insurance) #Stummary stats for each of the variables 

#Based on the abe summary, we can see that we have at least 3 categorical
#variables: sex, smoker, region 

summary(insurance$sex) 
table(as.numeric(insurance$sex))  #can see here that when recoded, female = 1, male = 2 

summary(insurance$smoker)
table(as.numeric(insurance$smoker)) #yes = 1, no = 2 

summary(insurance$region)
table(as.numeric(insurance$region)) #NE= 1, NW=2, SE=3, SW=4 

#Letsp plot: 

plot.age <- ggplot(insurance, aes(x = age, y = charges)) +
  geom_point()

plot.bmi <- ggplot(insurance, aes(x = bmi, y = charges)) +
  geom_point()

plot.region <- ggplot(insurance, aes(x = region, y = charges)) +
  geom_point()

grid.arrange(plot.age, plot.bmi, plot.region, ncol=3)

######## Now lets do some box plots: 

plot.sex <- ggplot(insurance, aes(x = sex, y = charges)) +
  geom_boxplot()

plot.smoker <- ggplot(insurance, aes(x = smoker, y = charges)) +
  geom_boxplot()

plot.child <- ggplot(insurance, aes(x = as.factor(children), y = charges)) +
  geom_boxplot()

plot.region <- ggplot(insurance, aes(x = region, y = charges)) +
  geom_boxplot()

gridExtra::grid.arrange(plot.sex, plot.smoker, plot.child, plot.region, ncol=4, nrow=1)

##shows us that females and males pay on avarage the same charges. 
##When looking at the second boxplot (right upper corner) we see that 
## smokers pay higher charges compared to non smokers. Also people with 
## more childres pay more charges and it seems that the region has not an 
## influence on the charges. In all instances the charges have a skewed 
## distribution


# Simple Linear Regression /// 
# Predict COST based on AGE / one IV and one DV 

# make plot 
ggplot(insurance, aes(x = age, y = charges)) +
  geom_point() +
  geom_hline(yintercept = mean(insurance$charges)) + #add line representing the mean charges
  geom_smooth(method='lm') # add regression line




#####SIMPLE LINEAR REGRESSION #####
#####SIMPLE LINEAR REGRESSION #####
#####SIMPLE LINEAR REGRESSION #####


# simple linear regression using age as the predictor variable:
mod1 = lm(charges ~ age, data = insurance)

# add model diagnostics to our training data
model1_results <- mod1
model1_results <- augment(mod1, insurance)

# show results:
summary(mod1)

#Here you see Multiple R-squared: 0.08941. This value represents the square R between age and charges and also tells us 
#that 8.94% of the variation in the outcome variable charges is explained by the predictor variable age. 
#When you take the square root of square-R, you also get the correlation coefficient between age and charges.
# to show the correlation between age and age.
sqrt(0.08941)


# https://drsimonj.svbtle.com/visualising-residuals
insurance$predicted <- predict(mod1)   # Save the predicted values
insurance$residuals <- residuals(mod1) # Save the residual values
#####
ggplot(insurance, aes(x = age, y = charges)) +  # Set up canvas with outcome variable on y-axis
  geom_point() #plot the actual values
#####
#add the predicted values 
ggplot(insurance, aes(x = age, y = charges)) +
  geom_point() +
  geom_point(aes(y = predicted), shape = 1)
#####
ggplot(insurance, aes(x = age, y = charges)) +
  geom_segment(aes(xend = age, yend = predicted)) +
  geom_point() +
  geom_point(aes(y = predicted), shape = 1)
#####
ggplot(insurance, aes(x = age, y = charges)) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Plot regression slope
  geom_segment(aes(xend = age, yend = predicted), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = predicted), shape = 1) +
  theme_bw()  # Add theme for cleaner look


#Another way: 
ggplot(insurance, aes(age, charges)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_smooth(se = FALSE, color = "red")




##### MULTIPLE LINEAR REGRESSION #####
##### MULTIPLE LINEAR REGRESSION #####
##### MULTIPLE LINEAR REGRESSION #####
##### MULTIPLE LINEAR REGRESSION #####


#
# Slides -> page 9/10 in slides
#
######

# multiple linear regression using age and bmi as predictor variables:
mod2 = lm(charges ~ age + bmi, data = insurance)

# show results:
summary(mod2)

broom::tidy(mod2)

confint(mod2, level=0.95)

# the "standard" 95% CI consists of a lower 2.5% limit and an upper 97.5% limit. 
# When you repeat your experiment many times, the true parameter value will be below the 
# CI in 2.5% of cases and above it in another 2.5% of cases - and the CI will cover it in 95% of cases.



#multiple R-squared of 0.1172, this means that our model explains 11.72% of the variation 
#in the outcome variable. Multiple R-squared can range from 1 to 0, were 1 means that the 
#model perfectly fits the observed data and explains 100% of the variation in the outcome 
#variable. In our model with only the predictor age, the R-squared was 0.08941. Thus adding 
#BMI to the model improves the fit. 

#But how do we know if this improvement in R-square is significant? Well we can compare both 
#models by calculating the F-ratio. Here is how to do is in R:

anova(mod1, mod2)

# with a p<0.001 we know that the change in explained variance is significant. 

broom::glance(mod1)
broom::glance(mod2)

#Comparing MODELS 
list(model1 = broom::glance(mod1), model2 = broom::glance(mod2))

# r-squared? 
# adj. r-squared? 
# sigma (RSE) ? 
# statistic ? 
# AIC/BIC 



#Now we want to know if sex, region and the number of children will improve the model. 
#First we make a model that includes the known predictors. Since we don't which of the new 
#predictors (sex, region or number of children is important, we will add them one by one in the
#new model.

mod2 = lm(charges ~ age + bmi, data = insurance)

mod3 <- lm(charges ~ age + bmi + children, data = insurance)
summary(mod3)
broom::glance(mod3)

mod4 <- lm(charges ~ smoker + age + bmi + sex + children + region, data = insurance)
summary(mod4)

list(model2 = broom::glance(mod2), model3 = broom::glance(mod3), model4=broom::glance(mod4))



mod5 <- lm(charges ~ smoker + age + bmi + children + region, data = insurance)
summary(mod5)

anova(mod3, mod5)




#### CHECK ASSUMPTIONS 
#### CHECK ASSUMPTIONS 
#### CHECK ASSUMPTIONS 


### OUTLIERS -> 
# obtain residuals, cooks distance for model 5 and add to dataframe
insurance$standardized.residuals <- rstandard(mod5)
insurance$cooks.distance <- cooks.distance(mod5) # to spot influential cases

# plot cooks distance visually 
plot(insurance$cooks.distanc, pch = 16, col = "blue") #Plot the Cooks Distances.

# another way of looking at cooks: 
par(mfrow=c(1, 2))
plot(mod5, which = 4, id.n = 5)
plot(mod5, which = 5, id.n = 5)

# if we want to then see who the top 5 are: 
model1_results %>%
  top_n(5, wt = .cooksd)

###Categorizing distance from normal with 3 different groupers: 
# indicate which standardized residuals are > 2 and add to dataframe
insurance$large.residual2 <- insurance$standardized.residuals > 2 | insurance$standardized.residuals < -2
# indicate which standardized residuals are > 2.5 and add to dataframe
insurance$large.residual2.5 <- insurance$standardized.residuals > 2.5 | insurance$standardized.residuals < -2.5
# indicate which cases are outliers (standardized residual > 3)
insurance$outlier.residual <- insurance$standardized.residuals > 3 | insurance$standardized.residuals < -3




# percentage of cases with residual > 2 (this should 5% or lower)
paste("The percentage of cases with a standardized residual > 2 is", 
      round(sum(insurance$large.residual2)/nrow(insurance)*100, 2), "percent.")

# percentage of cases with residual > 2.5 (this should be 1% or lower)
paste("The percentage of cases with a standardized residual > 2.5 is", 
      round(sum(insurance$large.residual2.5)/nrow(insurance)*100, 2), "percent.")

# potential outliers
nrow(insurance[which(insurance$outlier.residual==T),])

# get cases with standardized residuals larger than 2.5
insurance %>% filter(large.residual2.5==T)

# cases with cooks distance larger than 1
insurance %>% filter(cooks.distance>=1)
# There are no cases with a cooks distance larger than 1, meaning that there 
# are no influential cases.




#Checking the assumption of multicollinearity
#Checking the assumption of multicollinearity
#Checking the assumption of multicollinearity

#Variance Inflation Factor (VIF)
#When checking the assumption of multicollinearity you check if one or more variables 
#used in the model are strongly correlated amongst eachother.
# One way to check is this, is to make a correlation matrix with all 
# predictor variables. An easier way is to look at the variance inflation factor (VIF). 
# The VIF should not be higher than 10.

rms::vif(mod5)


# VIF
car::vif(mod5)
# tolerance
1/vif(mod5)
# mean VIF
mean(car::vif(mod5))

#A VIF larger than 10 indicates multicolinearity. We have no VIF larger than 10, so this 
#indicates no multicollinearity. A tolerance lower 0.2 indicates problems. 
#In our cases the tolerance levels are all higher than 0.2, thus no problems here. 

#Also the mean VIF is around 1, so no bias here.










#Checking the assumption of independence
#Checking the assumption of independence
#Checking the assumption of independence 

# This step is most important as we will learn for time series 
# This can be done with the Durbin Watsons test
dwt(mod5)
# The D-W statistic is very close to 2 and we see a p-value larger than 0.5. This means 
# that the assumption of indepence is met.






#Checking assumptions around RESIDUALS 
#Checking assumptions around RESIDUALS 
#Checking assumptions around RESIDUALS 

plot(mod5)

#The first graph shows the fitted values against the observed resididuals. In this plot the 
#dots should be randomly placed around the horizontal zero line, which is clearly not the 
#case. It seems that there are three groups present. Thus per group there is difference 
#in variance across the residuals.

#Second: There is increasing variance across the residuals (heteroscedasticity), and 
#there migth also be a non-linear relationship between the outcome and the predictor. 
#This non-linear relationship is also reflected by the second plot (Q-Q plot), since 
#the dots deviate from the dotted line.


#Comparing MODELS 
list(model3 = broom::glance(mod3), model5 = broom::glance(mod5))





####STEPWISE REGRESSION //// 
# install.packages("olsrr")
stepwise <- ols_step_all_possible(mod5)

stepwise_2 <-ols_step_both_p(mod5, details=TRUE)




