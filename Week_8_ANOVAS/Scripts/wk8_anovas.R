library(DBI)
library(dplyr)
library("ggpubr")
library(moments) 
library(car)

## Installation of ODBC drivers if doing locally: 
## https://cran.r-project.org/web/packages/odbc/readme/README.html

con <- DBI::dbConnect(odbc::odbc(),
                      Driver    = "MySQL", 
                      Server    = "3.84.158.190",
                      Database = "us_population_2",
                      UID       = "dba",
                      PWD       = "ahi2020",
                      Port      = "3306")

df <- dbReadTable(con, "scratch")

##Lets pick at least 2-3 categorical variables (IVs - factors) and at least 1 DV (continuous)
df_sub = demographics %>% select (VAR1, VAR2, VAR3, VAR4, VAR5)

#Now lets just rename the columns: 
df_sub <- df_sub %>% rename(NEW = OLD, NEW = OLD, 
                                      NEW = OLD, NEW = OLD )

#Potentially drop NAs/nulls 
df_sub_nm = na.omit(df_sub)


# Detect Outliers 
library(mvoutlier)
outliers <-
  aq.plot(dataframename[c("VAR1","VAR2","VAR3","VAR4")])
outliers # show list of outliers


# Recode Example Again / using recode_factor (str) VS recode (int)
df_sub_nm$VAR = toString(df_sub_nm$VAR)
df_sub_nm = df_sub_nm %>% 
  mutate(VAR_RECODE = recode_factor(VAR, `OriginalValue` = "NewValue", `OriginalValue` = "NewValue"))


# Check Assumptions of all ouf our IVs with the DV 
# IV1 -> DV 
  # IV1-level1 -> DV 
  # IV1-level2 -> DV 
  # IV1-level3 -> DV
# IV2 -> DV 
  # IV2-level1 -> DV 
  # IV2-level2 -> DV 


# Prior way: 
iv1_level1 = df_sub_nm %>% filter(IV1 == "LEVEL1") 
iv1_level1 = iv1_level1$DV 

iv1_level2 = df_sub_nm %>% filter(IV1 == "LEVEL2") 
iv1_level2 = iv1_level1$DV 

iv2_level1 = df_sub_nm %>% filter(IV2 == "LEVEL1") 
iv2_level1 = iv2_level1$DV 

iv2_level2 = df_sub_nm %>% filter(IV2 == "LEVEL2") 
iv2_level2 = iv2_level2$DV 


# Why dont we create a function 
var_exctractor = function(dataframe, variablename, variablelevel) {
  temp = dataframe %>% filter (variablename == variablelevel)
  temp = temp$variablename
}

iv1_level1 = var_exctractor(df_sub_nm, "variablename", "variablelevel")
iv1_level2 = var_exctractor(df_sub_nm, "variablename", "variablelevel")
iv2_level1 = var_exctractor(df_sub_nm, "variablename", "variablelevel")
iv2_level2 = var_exctractor(df_sub_nm, "variablename", "variablelevel")


# Now lets do the plots: / Univariate NORMALITY 
hist(iv1_level1)
ggdensity(iv1_level1)
ggqqplot(iv1_level1)
qqnorm(iv1_level1)
qqline(iv1_level1)


# Lets get the numbers for kurtosis / skewness 
kurtosis(iv1_level1) 
skewness(iv1_level1) 
# Lets do the statistical test for normal distrubtion / want p > .05
shapiro.test(iv1_level1)


# Homogeniety 
# Bartlett Test of Homogeneity of Variances // PARAMETRIC 
bartlett.test(DV~IV, data=mydata)

# Figner-Killeen Test of Homogeneity of Variances // NON-PARAMETRIC 
fligner.test(DV~IV, data=mydata)


# Box plot with multiple groups
# +++++++++++++++++++++
# Plot tooth length ("len") by groups ("dose")
# Color box plot by a second group: "supp"
library("ggpubr")
ggboxplot(DATAFRAME, x = "IV1", y = "DV", color = "IV2",
          palette = c("#00AFBB", "#E7B800"))


# Line plots with multiple groups
# +++++++++++++++++++++++
# Plot tooth length ("len") by groups ("dose")
# Color box plot by a second group: "supp"
# Add error bars: mean_se
# (other values include: mean_sd, mean_ci, median_iqr, ....)
library("ggpubr")
ggline(DATAFRAME, x = "IV1", y = "DV", color = "IV2",
       add = c("mean_se", "dotplot"),
       palette = c("#00AFBB", "#E7B800"))



require("dplyr")
group_by(DATAFRAME, IV1, IV2) %>%
  summarise(
    count = n(),
    mean = mean(len, na.rm = TRUE),
    sd = sd(len, na.rm = TRUE)
  )


# ANOVA with NO-INTERACTION 
res.aov2 <- aov(DV ~ IV1 + IV2, data = DATAFRAME)
summary(res.aov2)


# Two-way ANOVA with interaction effect
# These two calls are equivalent
res.aov3 <- aov(DV ~ IV1 * IV2, data = DATAFRAME)
res.aov3 <- aov(DV ~ IV1 + IV2 + IV1:IV2, data = DATAFRAME)
summary(res.aov3)


# Unbalanced design / groups 
library(car)
my_anova <- aov(DV ~ IV * IV2, data = DATAFRAME)
Anova(my_anova, type = "III")



