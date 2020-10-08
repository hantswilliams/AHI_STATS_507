
library(DBI)
library(dplyr)
library("ggpubr")
library(moments) 
library(car)


con <- DBI::dbConnect(odbc::odbc(),
                      Driver    = "MySQL", 
                      Server    = "3.84.158.190",
                      Database = "us_population_2",
                      UID       = "dba",
                      PWD       = "ahi2020",
                      Port      = "3306")

demographics <- dbReadTable(con, "demo_j")

demo_sample = demographics %>% select (SEQN, RIAGENDR, RIDAGEYR, RIDRETH1)

options("scipen"=100, "digits"=2)

demo_sample = demographics %>% select (SEQN, RIAGENDR, RIDAGEYR, RIDRETH1)

demo_sample$RIDAGEYR = round(demo_sample$RIDAGEYR, digits = 0)

# now lets just rename the columns: 

demo_sample <- demo_sample %>% rename(patientid = SEQN, sex = RIAGENDR, 
                                      age = RIDAGEYR, race = RIDRETH1 )

demo_sample = na.omit(demo_sample)



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


clean_nomiss = na.omit(clean_df)



### one thing we can probably do is transform male = 1 and female = 2 
### to make this a little bit more interpretable 

clean_nomiss$sex = toString(clean_nomiss$sex)


clean_nomiss = clean_df %>% 
  mutate(sex = recode_factor(sex, `1` = "Male", `2` = "Female"))

clean_nomiss = clean_nomiss %>% mutate(race=recode_factor(race, 
                                                          `1`="Mexican_American",
                                                          `2`="Other_Hispanic",
                                                          `3`="Non_Hisp_White",
                                                          `4`="Non_Hisp_Black",
                                                          `5`="Other_Race"))








## FIRST QUESTION --> independent t-test // 
## does the mean value of total medications prescribed differ between age 
## we have a single grouper (factor) that has two levels: male and female 
## and we have one dependent (continuous) variable = medication count 
## so we will do a T-test, independent (e.g., male vs female are not part of the same group)

## Assumptions for medication count 


clean_nomiss = na.omit(clean_nomiss)


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










# Lets get the numbers for kurtosis / skewness 
kurtosis(male_medscount) 
kurtosis(female_medscount) 

skewness(male_medscount) 
skewness(female_medscount) 


# if we wanted to though / but not report, this it: 
## interpretation: p > .05 == homogenity of variance assumption is good 
res <- var.test(rx_total ~ sex, data = clean_nomiss)
res


# Plot rx_count by group and color by group
ggpubr::ggboxplot(clean_nomiss, x = "sex", y = "rx_total", 
                  color = "sex", palette = c("#00AFBB", "#E7B800"),
                  order = c("Male", "Female"),
                  ylab = "rx_total", xlab = "Groups")


wilcox.test(rx_total~sex, data = clean_nomiss)




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



ggpubr::ggboxplot(clean_nomiss, x = "agegroup", y = "rx_total", 
                  color = "agegroup", palette = c("#00AFBB", "#E7B800", "#FF69B4", "#5F9EA0"),
                  order = c("Senior", "Old", "Young Adult", "Child"),
                  ylab = "rx_total", xlab = "agegroup")




group_by(clean_nomiss, agegroup) %>%
  summarise(
    count = n(),
    mean = mean(rx_total, na.rm = TRUE),
    sd = sd(rx_total, na.rm = TRUE),
    median = median(rx_total, na.rm = TRUE),
    IQR = IQR(rx_total, na.rm = TRUE)
  )


kurtosis(age_senior) 
kurtosis(age_old) 
kurtosis(age_ya) 
kurtosis(age_child) 

skewness(age_senior) 
skewness(age_old) 
skewness(age_ya) 
skewness(age_child) 

# Lets do the statistical test for normal distrubtion / want p > .05
shapiro.test(age_senior)
shapiro.test(age_old)
shapiro.test(age_ya)
shapiro.test(age_child)

leveneTest(rx_total ~ agegroup, data = clean_nomiss)

kruskal.test(rx_total ~ agegroup, data = clean_nomiss)


pairwise.t.test(clean_nomiss$rx_total, clean_nomiss$agegroup, p.adjust.method = "BH", pool.sd = FALSE)


pairwise.wilcox.test(clean_nomiss$rx_total, clean_nomiss$agegroup,
                     p.adjust.method = "BH")




