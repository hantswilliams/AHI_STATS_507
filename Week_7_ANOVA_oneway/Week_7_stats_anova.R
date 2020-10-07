library(DBI)
library(dplyr)
library("ggpubr")
library(e1071) 
library(moments)  
library(car)


## lets connect to a dataset that we were exploring on monday 
## the NHANES data / nice and big, interesting variables potentially 

con <- DBI::dbConnect(odbc::odbc(),
                      Driver    = "MySQL", 
                      Server    = "3.84.158.190",
                      Database = "us_population_2",
                      UID       = "dba",
                      PWD       = "ahi2020",
                      Port      = "3306")

## https://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx?Component=Demographics&CycleBeginYear=2017
## https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DEMO_J.htm#RIAGENDR


## SEQN - numerical value / patient ID 

## RIAGENDR // 1 = Male, 2 = Female  . = missing 


## RIDAGEYR - age in years at time of screening 

## RIDRETH1 - race 1 = mexican american 2 = other hispanic 3 = non-hispanic white 4= nonhispanic black 
## 5 = other race . = missing 

demographics <- dbReadTable(con, "demo_j")

demo_sample = demographics %>% select (SEQN, RIAGENDR, RIDAGEYR, RIDRETH1)

options("scipen"=100, "digits"=2)

demo_sample = demographics %>% select (SEQN, RIAGENDR, RIDAGEYR, RIDRETH1)

demo_sample$RIDAGEYR = round(demo_sample$RIDAGEYR, digits = 0)

# now lets just rename the columns: 

demo_sample <- demo_sample %>% rename(patientid = SEQN, sex = RIAGENDR, 
                                      age = RIDAGEYR, race = RIDRETH1 )



## the prescriptions dataset is really interseting, can use this for the API we are creating 
## looks at all the different interesting fields 
## https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/RXQ_RX_J.htm

## SEQN

## RXDCOUNT Number of prescription medicines taken
## continuous variable 

prescriptions <- dbReadTable(con, "rxq_rx_j")

rx_sample = prescriptions %>% select (SEQN, RXDCOUNT )


## lets do some data cleaning here - it appears as though it is stacked 
## dataframe, and we just care about the total number of unique medications 
## per patient // 

rx_sample_distinct = distinct(rx_sample, SEQN, RXDCOUNT)

## lets now rename the category as well 

rx_sample_distinct = rx_sample_distinct %>% rename (patientid = SEQN, rx_total = RXDCOUNT )

## now lets group these two data frames together: 

clean_df <- merge(demo_sample, rx_sample_distinct,by="patientid")



### one thing we can probably do is transform male = 1 and female = 2 
### to make this a little bit more interpretable 

clean_df = clean_df %>% mutate(sex=recode(sex, 
                                          `1`="Male",
                                          `2`="Female"))

clean_df = clean_df %>% mutate(race=recode(race, 
                                           `1`="Mexican_American",
                                           `2`="Other_Hispanic",
                                           `3`="Non_Hisp_White",
                                           `4`="Non_Hisp_Black",
                                           `5`="Other_Race"))



clean_df <- clean_df %>% mutate(agegroup = case_when(
  age >= 80 ~ 'Very Senior',
  age >= 65  & age <= 79 ~ 'Senior',
  age >= 30  & age <= 64 ~ 'Old',
  age >= 18  & age <= 29 ~ 'Young Adult',
  age <= 17 ~ 'Child')) 







## what should we do with missing values? lets see how many first 
summary(clean_df$rx_total)

## so we are missing 5,209 
## 5,209/9254 = roughly 56% 
## dont care - 4k responses is still good enough 
## we can either assume missing = 0 which might skew things, make assumptions that we shouldn't be making
## or we can just drop them and come back to this later 
## OR....we could impute missing values, giving each the mean of means

clean_nomiss = na.omit(clean_df)


## FIRST QUESTION --> independent t-test // 
## does the mean value of total medications prescribed differ between age 
## we have a single grouper (factor) that has two levels: male and female 
## and we have one dependent (continuous) variable = medication count 
## so we will do a T-test, independent (e.g., male vs female are not part of the same group)

## Assumptions for medication count 

## list of meds for males 
male_medscount = clean_nomiss %>% filter(sex == "Male") 
male_medscount = male_medscount$rx_total

female_medscount = clean_nomiss %>% filter(sex == "Female")
female_medscount = female_medscount$rx_total

# MALE PLOTS 
hist(male_medscount)
ggdensity(male_medscount)
ggqqplot(male_medscount)

# FEMALE PLOTS 
hist(female_medscount)
ggdensity(female_medscount)
ggqqplot(female_medscount)

# Lets do the statistical test for normal distrubtion / want p > .05
shapiro.test(male_medscount)
shapiro.test(female_medscount)


# Lets get the numbers for kurtosis / skewness 
kurtosis(male_medscount) 
kurtosis(female_medscount) 
skewness(male_medscount) 
skewness(female_medscount) 

# Homogenity of variance // F-test // variance equal between M and F ?? 
# no need to do the F-test for homogenity of variance, because we know 
# that the data is HEAVILY skewed by on our shpairo wilk / lets move on 
# to then conducting a non-parametric t-test between M and F for mean 
# medication count 


# if we wanted to though / but not report, this it: 
## interpretation: p > .05 == homogenity of variance assumption is good 
res <- var.test(rx_total ~ sex, data = clean_nomiss)
res


### lets look at the means between groups , general counts 

group_by(clean_nomiss, sex) %>%
  summarise(
    count = n(),
    mean = mean(rx_total, na.rm = TRUE),
    sd = sd(rx_total, na.rm = TRUE)
  )

pivot_sex_medcount = group_by(clean_nomiss, sex) %>%
  summarise(
    count = n(),
    mean = mean(rx_total, na.rm = TRUE),
    sd = sd(rx_total, na.rm = TRUE)
  )



# Plot rx_count by group and color by group
ggpubr::ggboxplot(clean_nomiss, x = "sex", y = "rx_total", 
                  color = "sex", palette = c("#00AFBB", "#E7B800"),
                  order = c("Male", "Female"),
                  ylab = "rx_total", xlab = "Groups")


############
##
## Preleminary test to check paired t-test assumptions
##
## Assumption 1: Are the two samples paired?
## No 
##
## Assumption 2: Is this a large sample?
## Yes - 
##
## Assumption 3: How to check the normality?
## Shapiro-Wilk 
## NO 
##
##
## Conclusion - non-parmetric indepedent sample t-test 
# independent 2-group Mann-Whitney U Test
## wilcox.test(y~A)
# where y is numeric and A is A binary factor
############

wilcox.test(rx_total~sex, data = clean_nomiss)

## not significantly different /// 




#### Now lets look at say RACE and AGE 
## lets first look at AGE 


age_senior = clean_nomiss %>% filter(agegroup == "Senior") 
age_senior = age_senior$rx_total

age_old = clean_nomiss %>% filter(agegroup == "Old") 
age_old = age_old$rx_total

age_ya = clean_nomiss %>% filter(agegroup == "Young Adult") 
age_ya = age_ya$rx_total

age_child = clean_nomiss %>% filter(agegroup == "Child") 
age_child = age_child$rx_total


# SENIOR 
hist(age_senior)
ggdensity(age_senior)
ggqqplot(age_senior)

# OLD 
hist(age_old)
ggdensity(age_old)
ggqqplot(age_old)

# YOUNG ADULTS  
hist(age_ya)
ggdensity(age_ya)
ggqqplot(age_ya)

# CHILDREN  
hist(age_child)
ggdensity(age_child)
ggqqplot(age_child)


# Lets do the statistical test for normal distrubtion / want p > .05
shapiro.test(age_senior)
shapiro.test(age_old)
shapiro.test(age_ya)
shapiro.test(age_child)

# Lets get the numbers for kurtosis / skewness 
kurtosis(age_senior) 
kurtosis(age_old) 
kurtosis(age_ya) 
kurtosis(age_child) 

skewness(age_senior) 
skewness(age_old) 
skewness(age_ya) 
skewness(age_child) 


### Homogenity check -> we have more than 3 groups, and non-normal data
### if was normal, could use bartlett: 
bartlett.test(rx_total ~ agegroup, data = clean_nomiss) #homogenity not met 

### # Levene’s test: A robust alternative to the Bartlett’s test that is less sensitive to departures from normality. 
leveneTest(rx_total ~ agegroup, data = clean_nomiss).  #homogenity not met, p < .05 
### this tells us there there is unequal variance - differences that exist between distribution of scores
### within each group 

### 
## so, not normally distributed 
## homogeneity is not met 
## we focus on non-parametric one sample ANOVA 
###



group_by(clean_df, agegroup) %>%
  summarise(
    count = n(),
    mean = mean(rx_total, na.rm = TRUE),
    sd = sd(rx_total, na.rm = TRUE),
    median = median(rx_total, na.rm = TRUE),
    IQR = IQR(rx_total, na.rm = TRUE)
  )

# Box plots
# ++++++++++++++++++++
# Plot weight by group and color by group
# Plot rx_count by group and color by group
ggpubr::ggboxplot(clean_nomiss, x = "agegroup", y = "rx_total", 
                  color = "agegroup", palette = c("#00AFBB", "#E7B800", "#FF69B4", "#5F9EA0"),
                  order = c("Senior", "Old", "Young Adult", "Child"),
                  ylab = "rx_total", xlab = "agegroup")

# Mean plots
# ++++++++++++++++++++
# Plot weight by group
# Add error bars: mean_se
# (other values include: mean_sd, mean_ci, median_iqr, ....)
ggline(clean_nomiss, x = "agegroup", y = "rx_total", 
       add = c("mean_se", "jitter"), 
       order = c("Senior", "Old", "You Adult", "Child"),
       ylab = "rx_total", xlab = "agegroup")


### non-parametric one-way anova // independent samples // 
### is there a difference in means between our single factor (age), that contains
### four different leves 
kruskal.test(rx_total ~ agegroup, data = clean_nomiss)

## interpret: 
## As the p-value is less than the significance level 0.05, 
## we can conclude that there are significant differences between the treatment groups.

## NOW LETS CONDUCT POST_HOC test (postpriori vs apriori)
# From the output of the Kruskal-Wallis test, we know that there is a 
# significant difference between groups, but we don’t know which pairs of groups are different.
# It’s possible to use the function pairwise.wilcox.test() to 
# calculate pairwise comparisons between group levels with corrections for multiple testing.

pairwise.wilcox.test(clean_nomiss$rx_total, clean_nomiss$agegroup,
                     p.adjust.method = "BH")







