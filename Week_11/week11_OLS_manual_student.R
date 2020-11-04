
library(car) 
library(boot) 
library(scatterplot3d) # Used to extract p-value from logistic model

options(scipen=999) #turn off scientific notation
 

y <- c(23663, 20659, 32277, 21595, 27227, 
       25023, 26504, 28741, 21735, 23366, 
       20871, 28370, 21105, 22706, 19527, 
       28321)

# X represents the fraction of residents in each county having attained a
# bachelor's degree or better
x   <- c(0.19, 0.16, 0.40, 0.24, 0.31, 0.24, 0.28, 
         0.31, 0.18, 0.23, 0.17, 0.31, 0.15, 0.25, 
         0.19, 0.28)

# Let's combine Income and Education into a single dataframe
dat <- data.frame(Income = y, Education = x)

# We will add county names to each row
row.names(dat) <- c("Androscoggin", "Aroostook", "Cumberland", "Franklin", "Hancock",
                    "Kennebec", "Knox", "Lincoln", "Oxford", "Penobscot", "Piscataquis",
                    "Sagadahoc", "Somerset", "Waldo", "Washington", "York")

plot(Income ~ Education , dat)
      




## manually calculating 
dat2 = dat
dat2$x = dat2$Education
dat2$y = dat2$Income

#need to claculate the mean of X and mean of Y 
meanX = mean(dat2$x)
meanY= mean(dat2$y)

dat2$numerator = (dat2$x - meanX) * (dat2$y - meanY)
dat2$denominator = ((dat2$x - meanX) * (dat2$x - meanX))

sum_numerator = sum(dat2$numerator)
sum_denominator = sum(dat2$denominator)

x_slope_via_ls = sum_numerator / sum_denominator

# now calculating y-sloe 
ourmeanx = meanX
ourmeany = meanY
yintercept = ourmeany - (x_slope_via_ls*meanX)

#y intercept = 12405 
#x slope = 49670
# equation = Y = 12405 + 49670(x) + error 
dat2$predicted = 12405 + (49670*dat2$x)

#Predicted 
plot(predicted ~ x , dat2)
#Original 
plot(y ~ x , dat2)

#Residuals 
#Now lets manually compute the delta (abs) between predicted and actual 
#This will also be called the least absolute error (LAE)
dat2$lae = abs(dat2$predicted - dat2$y)

#Then to get to the sum of squared errors (SSE)
dat2$sse = (abs(dat2$predicted - dat2$y))^2

lae_mean = mean(dat2$lae)
sse_mean = mean(dat2$sse)

#Sum of Standard Error 
sum_sse = sum(dat2$sse)

#Mean of Standard Error
n <- length(dat2$x)
mean_standard_error = sum_sse / (n - 2)











## Simple Way 
dat3 = dat
dat3$x = dat2$Education
dat3$y = dat2$Income
model <- lm( y ~ x, dat3)
model_summ <-summary(model)

#### Need to show manually how then based on the model, 
#### you could then manually show where / how the points 
#### would equally if you were to do this manually with the slope 
#### equation we just greated 

# https://drsimonj.svbtle.com/visualising-residuals
dat3$predicted <- predict(model)   # Save the predicted values
dat3$residuals <- residuals(model) # Save the residual values
#####
ggplot(dat3, aes(x = x, y = y)) +  # Set up canvas with outcome variable on y-axis
geom_point() #plot the actual values
#####
#add the predicted values 
ggplot(dat3, aes(x = x, y = y)) +
  geom_point() +
  geom_point(aes(y = predicted), shape = 1)
#####
ggplot(dat3, aes(x = x, y = y)) +
  geom_segment(aes(xend = x, yend = predicted)) +
  geom_point() +
  geom_point(aes(y = predicted), shape = 1)
#####
ggplot(dat3, aes(x = x, y = y)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
  geom_segment(aes(xend = x, yend = predicted), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = predicted), shape = 1) +
  theme_bw()  # Add theme for cleaner look






####ANOTHER WAY => https://rpubs.com/aaronsc32/simple-linear-regression
####ANOTHER WAY => https://rpubs.com/aaronsc32/simple-linear-regression
####ANOTHER WAY => https://rpubs.com/aaronsc32/simple-linear-regression
# Manual Calculation of then the ERROR 
n <- length(dat2$x)

# Calculate the error statistics Sxx, Syy, and Sxy
sxx <- sum(dat2$x^2) - sum(dat2$x)^2 / n
syy <- sum(dat2$y^2) - sum(dat2$y)^2 / n
sxy <- sum(x * y) - (sum(x) * sum(y)) / n

# Coefficients beta0 and beta1
b1 <- sxy / sxx
b0 <- mean(dat2$y) - b1 * mean(dat2$x)

# Sum of standard error and Mean Standard Error
sse <- syy - sxy^2 / sxx
mse <- sse / (n - 2)

# Coefficient of determination R-squared
# where 𝑆𝑆res is the residual sum of squares and 𝑆𝑆tot is the total sum of squares.
r2 <- (syy - sse) /syy