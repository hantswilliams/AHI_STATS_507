
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

clean_nomiss$race = toString(clean_nomiss$race)

clean_df = clean_df %>% mutate(race=recode_factor(race, 
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



