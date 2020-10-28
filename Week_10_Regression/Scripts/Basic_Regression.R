## ORIGINAL DATA RETRIEVED FROM: https://www.kaggle.com/mirichoi0218/insurance

## Setting up the environment and data import

library(ggplot2)
library(dplyr)
library(cowplot)
library(Hmisc)
library(WVPlots)
library(broom)


Data <- read.csv("https://raw.githubusercontent.com/hantswilliams/AHI_STATS_507/main/Week_10_Regression/Datasets/insurance.csv")

## Understanding the data
#* **Age**: insurance contractor age, years
#* **Sex**: insurance contractor gender, [female, male]
#* **BMI**: Body mass index, providing an understanding of body, weights that are relatively high or low relative to height, objective index of body weight (kg / m ^ 2) using the ratio of height to weight, ideally 18.5 to 24.9
#* **Children**: number of children covered by health insurance / Number of dependents
#* **Smoker**: smoking, [yes, no]
#* **Region**: the beneficiary's residential area in the US, [northeast, southeast, southwest, northwest]
#* **Charges**: Individual medical costs billed by health insurance, $ *#predicted value*


summary(Data)
Hmisc::describe(Data)


## Exploratory Data Analysis
x_age <- ggplot(Data, aes(age, charges)) +
  geom_jitter(color = "blue", alpha = 0.5) +
  theme_light()

y_bmi <- ggplot(Data, aes(bmi, charges)) +
  geom_jitter(color = "green", alpha = 0.5) +
  theme_light()

p <- plot_grid(x_age, y_bmi) 
title <- ggdraw() + draw_label("1. Correlation between Charges by Age + BMI", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))
#* **Plot 1**: As Age and BMI go up Charges for health insurance also trends up.



##############################
x <- ggplot(Data, aes(sex, charges)) +
  geom_jitter(aes(color = sex), alpha = 0.7) +
  theme_light()

y <- ggplot(Data, aes(children, charges)) +
  geom_jitter(aes(color = children), alpha = 0.7) +
  theme_light()

p <- plot_grid(x, y) 
title <- ggdraw() + draw_label("2. Correlation between Charges and Sex / Children covered by insurance", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))
# * **Plot 2**: No obvious connection between Charges and Age. Charges for insurance with 4-5 chilren covered seems to go down (doesn't make sense, does it?)

##############################
x <- ggplot(Data, aes(smoker, charges)) +
  geom_jitter(aes(color = smoker), alpha = 0.7) +
  theme_light()

y <- ggplot(Data, aes(region, charges)) +
  geom_jitter(aes(color = region), alpha = 0.7) +
  theme_light()

p <- plot_grid(x, y) 
title <- ggdraw() + draw_label("3. Correlation between Charges and Smoker / Region", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))

# * **Plot 3**: Charges for Smokers are higher for non-smokers (no surprise here). No obvious connection between Charges and Region.


## Linear Regression Model
## Linear Regression Model
## Linear Regression Model

## Lets focus on AGE + CHARGES 


ggplot(Data, aes(x = age, y = charges)) +
  geom_point() +
  stat_smooth()

cor(Data$age, Data$charges)

## Computation 
model <- lm(charges ~ age, data = Data)
model

# INTERPRETATION: 

# the estimated regression line equation can be written as follow: charges = 3165.9 + 257.7*age 
# the intercept: 3165.9 // predicted charge cost when age = 0 
# regression beta coefficient (257.7) => THE SLOPE 

ggplot(Data, aes(x = age, y = charges)) +
  geom_point() +
  stat_smooth(method = lm)

### Lets look at the residual errors => 
model.diag.metrics <- augment(model)

ggplot(model.diag.metrics, aes(age, charges)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE) +
  geom_segment(aes(xend = age, yend = .fitted), color = "red", size = 0.3)



summary(model) # Statistically significant, but not explaning a lot of the variance 

# The R-squared (R2) ranges from 0 to 1 and represents the proportion of information (i.e. variation) 
# in the data that can be explained by the model. 
# The adjusted R-squared adjusts for the degrees of freedom --> more important for when have MULTIPLE Y variables // 
# e.g., multiple independent variables / predictors -> penalizes the equation 

# An (adjusted) R2 that is close to 1 indicates that a large proportion of the variability 
# in the outcome has been explained by the regression model.

# A number near 0 indicates that the regression model did not explain 
# much of the variability in the outcome.

# R-Squared: Higher the better
# F-statistic: Higher the better

# For confidence intervals: 
confint(model)




##### OUTLIER REMOVAL: 
##### OUTLIER REMOVAL: 
#find Q1, Q3, and interquartile range for values in column A
Q1 <- quantile(Data$charges, .25)
Q3 <- quantile(Data$charges, .75)
IQR <- IQR(Data$charges)

#only keep rows in dataframe that have values within 1.5*IQR of Q1 and Q3
no_outliers <- subset(Data, Data$charges > (Q1 - 1.5*IQR) & Data$charges< (Q3 + 1.5*IQR))

model <- lm(charges ~ age, data = no_outliers)
model
summary(model) 
