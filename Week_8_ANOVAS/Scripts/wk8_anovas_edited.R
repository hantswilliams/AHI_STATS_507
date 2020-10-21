library(DBI) #for database connection
library(dplyr) #for data maipulation 
library("ggpubr") #for visuzliation 
library(moments) #for assumption testing 
library(car) #for assumption test / CAR package for non-balanced designs
library(stringr) #for string manipulation 
library(psych) #for descriptives 
library(vcd) #for mosaic visuzliation 
library(summarytools) #for advanced stats / descriptives 
library(nortest) #equivalent of shapiro but for larger samples // Anderson-Darling // ad.test(data$variable)
library(CGPfunctions) #for plotting 2 factor anova nice and clean
library(pwr)



## Installation of ODBC drivers if doing locally: 
## https://cran.r-project.org/web/packages/odbc/readme/README.html

con <- DBI::dbConnect(odbc::odbc(),
                      Driver    = "MySQL", 
                      Server    = "3.84.158.190",
                      Database = "examples",
                      UID       = "dba",
                      PWD       = "ahi2020",
                      Port      = "3306")

df <- dbReadTable(con, "diabetes_hants")

##Lets pick at least 2-3 categorical variables (IVs - factors) and at least 1 DV (continuous)
df_sub = df %>% select ('num_medications', 'race', 'gender', 'age', 'medical_specialty')

#Now lets just rename the columns: 
df_sub <- df_sub %>% rename('medcount' = 'num_medications', 'category' = 'medical_specialty', 'sex' = 'gender')

# Looks like there is null/missing that are highlighted with ? -> lets replace 
# those with actual NULL / NA 
df_sub <- replace(df_sub, df_sub == '?', NA)

# Make sure our data varaible (continuous) is really continuous data / 
df_sub$medcount = as.numeric(df_sub$medcount)

# Do some data cleaning on the AGE 
df_sub$age <- str_replace(df_sub$age, "\\[", "")
df_sub$age <- str_replace(df_sub$age, "\\)", "")

## Lets find out the types 
df_sub_types = sapply(df_sub, class)

race = as.data.frame(table(df_sub$race))
sex = as.data.frame(table(df_sub$sex))
age = as.data.frame(table(df_sub$age))
category = as.data.frame(table(df_sub$category))


## Create some new cateogries for CATEGORIES that are simpler -> maybe 3-4 groups
## Create some new cateogries for CATEGORIES that are simpler -> maybe 3-4 groups
## When looking at the category preview table, sort by size, we can see 
## there are a few categories with >5k responses, lets just focus on those 
# InternalMedicine
# Emergency/Trauma
# Family/GeneralPractice
# Cardiology
# Surgery-General
# Nephrology
# Orthopedics
# Orthopedics-Reconstructive
# Radiologist

# generate the recode groups.
highresponse = c("InternalMedicine", "Emergency/Trauma", "Family/GeneralPractice",
                 "Cardiology", "Surgery-General")

mediumresponse = c("Nephrology", "Orthopedics", "Orthopedics-Reconstructive",
                   "Radiologist")

df_sub$category_coded = ifelse(df_sub$category %in% highresponse, "Highresponse", 
                               ifelse(df_sub$category %in% mediumresponse, "Mediumresponse", "Lowormissing"))


acutegroup = c("Emergency/Trauma", "Emergency/Trauma", "Surgery-General", "Surgery-Vascular",
               "Surgery-Neuro", "Surgery-Thoracic", "Pediatrics-CriticalCare")


df_sub$category_acuity = ifelse(df_sub$category %in% acutegroup, "HighAcuity", "LowAcuity")







#### Now lets do the same for age -> // Based on theoretical framework 
# Child 0-10 10-20
# Adult - everyone else 
# Senior 60-70 70-80 80-90 90-100

child = c("0-10", "10-20")
senior = c("60-70", "70-80", "80-90", "90-100")

df_sub$age_coded = ifelse(df_sub$age %in% child, "Child", 
                               ifelse(df_sub$age %in% senior, "Senior", "Adult"))

df_sub$age_coded_simple = ifelse(df_sub$age %in% child, "Child", "Adult")



#### Now lets do the same for race -> white or not white 
white = c("Caucasian")

df_sub$race_coded = ifelse(df_sub$race %in% white, "white", "not_white")





### VISUALIZATIONS - > Individual Categorical with Continuous 
### VISUALIZATIONS - > Individual Categorical with Continuous 
### VISUALIZATIONS - > Individual Categorical with Continuous 
boxplot(medcount ~ sex, data=df_sub, main="Sex by Med Count")

boxplot(medcount ~ race, data=df_sub, main="Race by Med Count")
boxplot(medcount ~ race_coded, data=df_sub, main="Race by Med Count")

boxplot(medcount ~ age, data=df_sub, main="Age by Med Count")
boxplot(medcount ~ age_coded, data=df_sub, main="Age Coded by Med Count")
boxplot(medcount ~ age_coded_simple, data=df_sub, main="Age Coded Simple by Med Count")

boxplot(medcount ~ category, data=df_sub, main="Category by Med Count")
boxplot(medcount ~ category_coded, data=df_sub, main="Category coded by Med Count")


### VISUALIZATIONS -> With 2 Categories 
### VISUALIZATIONS -> With 2 Categories 
### VISUALIZATIONS -> With 2 Categories 
ggboxplot(df_sub, x = "sex", y = "medcount", color = "race")
ggboxplot(df_sub, x = "race", y = "medcount", color = "sex")

ggboxplot(df_sub, x = "sex", y = "medcount", color = "age")
ggboxplot(df_sub, x = "age", y = "medcount", color = "sex")

ggboxplot(df_sub, x = "sex", y = "medcount", color = "category")
ggboxplot(df_sub, x = "category", y = "medcount", color = "sex")

ggboxplot(df_sub, x = "race", y = "medcount", color = "category")
ggboxplot(df_sub, x = "category", y = "medcount", color = "race")


mosaic(~ sex + race,
       data = df_sub)

mosaic(~ race + sex,
       data = df_sub)

mosaic(~ race + category_coded,
       data = df_sub)



ctable(x = df_sub$sex,
       y = df_sub$race)

ctable(x = df_sub$category,
       y = df_sub$race)

ctable(x = df_sub$category,
       y = df_sub$age)

ctable(x = df_sub$category,
       y = df_sub$age_coded)

ctable(x = df_sub$category,
       y = df_sub$age_coded_simple)



# Assess continuous variable 
# Assess continuous variable 
# Assess continuous variable 
# Assess continuous variable 
# Assess continuous variable 

summary(df_sub$medcount)

#psych package
describeBy(df_sub$medcount)

boxplot(df_sub$medcount,
        ylab = "medcount")

plot(density(df_sub$medcount))

# Draw points on the qq-plot
qqnorm(df_sub$medcount)

# Draw the reference line:
qqline(df_sub$medcount)
qqPlot(df_sub$medcount)
ggqqplot(df_sub$medcount)

# Drawing the reference line by groups:
qqPlot(df_sub$medcount, groups = df_sub$race)
qqPlot(df_sub$medcount, groups = df_sub$race_coded)
qqPlot(df_sub$medcount, groups = df_sub$sex)
qqPlot(df_sub$medcount, groups = df_sub$age)
qqPlot(df_sub$medcount, groups = df_sub$category)
qqPlot(df_sub$medcount, groups = df_sub$category_coded)
qqPlot(df_sub$medcount, groups = df_sub$category_acuity)


# Drawing multiple density plots by groups: 
ggplot(data=df_sub, aes(x=medcount, group=race, fill=race)) + geom_density(adjust=1.5) + facet_wrap(~race) 
ggplot(data=df_sub, aes(x=medcount, group=race_coded, fill=race_coded)) + geom_density(adjust=1.5) + facet_wrap(~race_coded) 
ggplot(data=df_sub, aes(x=medcount, group=sex, fill=sex)) + geom_density(adjust=1.5) + facet_wrap(~sex) 
ggplot(data=df_sub, aes(x=medcount, group=category_acuity, fill=category_acuity)) + geom_density(adjust=1.5) + facet_wrap(~category_acuity) 
ggplot(data=df_sub, aes(x=medcount, group=category_coded, fill=category_coded)) + geom_density(adjust=1.5) + facet_wrap(~category_coded) 


### Stacked multple density plots 
ggplot(data=df_sub, aes(x=medcount, group=race, fill=race)) +geom_density(adjust=1.5, position="fill")
ggplot(data=df_sub, aes(x=medcount, group=sex, fill=sex)) +geom_density(adjust=1.5, position="fill")


### SKEWNESS + KURTOSIS  
df_sub %>% 
  group_by(race) %>%
  summarise(Skew = skewness(medcount), Kurtosis = kurtosis(medcount))

df_sub %>% 
  group_by(sex) %>%
  summarise(Skew = skewness(medcount), Kurtosis = kurtosis(medcount))

df_sub %>% 
  group_by(category) %>%
  summarise(Skew = skewness(medcount), Kurtosis = kurtosis(medcount))



### SHAPIRO TESTS EQUIVALENTS // Anderson-Darling // if p < .05 = failure of normality check 
### SHAPIRO TESTS EQUIVALENTS // Anderson-Darling // if p < .05 = failure of normality check 
### SHAPIRO TESTS EQUIVALENTS // Anderson-Darling // if p < .05 = failure of normality check 
do.call("rbind", with(df_sub, tapply(medcount, race,
                                       function(x) unlist(ad.test(x)[c("statistic", "p.value")]))))

do.call("rbind", with(df_sub, tapply(medcount, age,
                                     function(x) unlist(ad.test(x)[c("statistic", "p.value")]))))

do.call("rbind", with(df_sub, tapply(medcount, category_coded,
                                     function(x) unlist(ad.test(x)[c("statistic", "p.value")]))))


# Bartlett Test of Homogeneity of Variances // PARAMETRIC 
# Bartlett Test of Homogeneity of Variances // PARAMETRIC 
# Bartlett Test of Homogeneity of Variances // PARAMETRIC 
bartlett.test(medcount~race, data=df_sub)
bartlett.test(medcount~age, data=df_sub)
bartlett.test(medcount~category_coded, data=df_sub)

# Figner-Killeen Test of Homogeneity of Variances // NON-PARAMETRIC 
# Figner-Killeen Test of Homogeneity of Variances // NON-PARAMETRIC 
# Figner-Killeen Test of Homogeneity of Variances // NON-PARAMETRIC 
fligner.test(medcount~race, data=df_sub)
fligner.test(medcount~age, data=df_sub)
fligner.test(medcount~category_coded, data=df_sub)


# ANOVA with NO-INTERACTION 
res.aov2 <- aov(medcount ~ race + age, data = df_sub)
summary(res.aov2)
### POST HOC -> 
TukeyHSD(res.aov2)
TukeyHSD(res.aov2, which="race")
TukeyHSD(res.aov2, which="age")

ggplot(data=df_sub, aes(x=medcount, y=age, fill=race)) +
  geom_bar(stat="summary", fun.y="mean") 

ggplot(aes(y = medcount, x = age, fill = race), data = df_sub) + geom_boxplot()

### Very Very Clean Output
Plot2WayANOVA(medcount ~ category_acuity * race, df_sub, plottype = "line")
Plot2WayANOVA(medcount ~ age * race, df_sub, plottype = "line")





# Two-way ANOVA with interaction effect
# These two calls are equivalent
res.aov3 <- aov(medcount ~ race * age, data = df_sub)
summary(res.aov3)
TukeyHSD(res.aov3, which="race")
TukeyHSD(res.aov3, which="race:age")

# Same thing below -->
res.aov3 <- aov(medcount ~ race + age + race:age, data = df_sub)
summary(res.aov3)


# Unbalanced design / groups (useage of the `car` package)
res.aov3.uneven <- aov(medcount ~ race * age, data = df_sub)
Anova(res.aov3.uneven, type = "III")













#Potentially drop NAs/nulls 
#Potentially drop NAs/nulls 
#Potentially drop NAs/nulls 

df_sub_nm = na.omit(df_sub)

