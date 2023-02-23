
library(lmtest)
library(sandwich)
library(stargazer)
library(tidyverse)
library("Hmisc")
library(corrplot)
library(ggplot2)

# LOADING DATA
data_covid <- read.csv("~/Downloads/original data_final.csv")
data_covid_df <- data.frame(data_covid)
head(data_covid_df)


#DATA VISUALIZATION 

mydata1 <- data_covid_df[c(3,6,8,13,19)]
mydata1.cor = cor(mydata1)
mydata1.cor
corrplot(mydata1.cor)

##Creating Normality plots for relevant variables

qqnorm(mydata1$death,pch = 1,ylab= "Deaths",frame = FALSE)
qqline(data_covid_df$death, col = "steelblue", lwd = 2)

qqnorm(mydata1$inIcuCurrently,pch = 1,ylab = "In the ICU currently",frame = FALSE)
qqline(data_covid_df$inIcuCurrently, col = "steelblue", lwd = 2)

qqnorm(mydata1$hospitalizedCurrently,pch = 1,ylab ="Hospitalized currently",frame = FALSE)
qqline(data_covid_df$hospitalizedCurrently,col = "steelblue", lwd = 2)

qqnorm(mydata1$hospitalizedCurrently,pch = 1,ylab ="On Ventilator Currently",frame = FALSE)
qqline(data_covid_df$hospitalizedCurrently,col = "steelblue", lwd = 2)

##CREATING A TIME SERIES PLOT FOR DATE AND DEATH INCREASE

my_data2 <- data_covid_df[c(1,4)]

##converting factor type to date type for ggplot
my_data2$date <-as.Date(my_data2$date,format = "%Y-%m-%d")
ggplot( data = my_data2, aes( date, deathIncrease )) + geom_line() +
   scale_x_date(date_labels = "%Y-%m-%d")


##Creating Normality plots for relevant variables

qqnorm(mydata1$death,pch = 1,ylab= "Deaths",frame = FALSE)
qqline(data_covid_df$death, col = "steelblue", lwd = 2)

qqnorm(mydata1$inIcuCurrently,pch = 1,ylab = "In the ICU currently",frame = FALSE)
qqline(data_covid_df$inIcuCurrently, col = "steelblue", lwd = 2)

qqnorm(mydata1$hospitalizedCurrently,pch = 1,ylab ="Hospitalized currently",frame = FALSE)
qqline(data_covid_df$hospitalizedCurrently,col = "steelblue", lwd = 2)

qqnorm(mydata1$hospitalizedCurrently,pch = 1,ylab ="On Ventilator Currently",frame = FALSE)
qqline(data_covid_df$hospitalizedCurrently,col = "steelblue", lwd = 2)

##CREATING A TIME SERIES PLOT FOR DATE AND DEATH

my_data2 <- mydata[c(1,4)]

##converting factor type to date type for ts
my_data2$date <-as.Date(my_data2$date,format = "%Y-%m-%d")
ggplot( data = my_data2, aes( date, deathIncrease )) + geom_line() +
scale_x_date(date_labels = "%Y-%m-%d")

cov2cor(mydata1.cor)

# SUBSETTING DATA AS PER YEAR
data_covid_df_2021 <- subset(data_covid_df, year == "2021")
head(data_covid_df_2021)

data_covid_df_2020 <- subset(data_covid_df, year == "2020")
head(data_covid_df_2020) 

# REGRESSION ANALYSIS

# HYPOTHESIS 1 (onVentilatorCurrently V/S Death):

# Null Hypothesis H0: Effect of onVentilatorCurrently is insignificant on Death for year 2021
# Alternate Hypothesis H1: Effect of onVentilatorCurrently is significant on Death for year 2021

# Linear Regression
linear_model_1 <- lm(death ~ onVentilatorCurrently, data_covid_df_2021)
summary(linear_model_1)

# Robust Standard Error
var_cov <- vcovHC(linear_model_1, type = "HC1")
coeftest(linear_model_1, var_cov)

# Plotting variables
plot(data_covid_df_2021$onVentilatorCurrently, data_covid_df_2021$death,
     col = "steelblue",
     pch = 20,
     xlab = "Ventilator", 
     ylab = "Number of deaths",
     cex.main = 0.9,
     main = "Number of deaths vs. Patients on Ventilator (2021)")
# add the regression line to the plot
abline(linear_model_1, 
       col = "red", 
       lwd = 2)

# As the p-value for the predictor "onVentilatorCurrently" is less than 0.05, we reject the null hypothesis.
# Therefore, the effect of "onVentilatorCurrently" on Death is significant.

# onVentilatorCurrently V/S Death (2020)

# HYPOTHESIS 2 (onVentilatorCurrently V/S Death):

# Null Hypothesis H0: Effect of onVentilatorCurrently is insignificant on Death for year 2020
# Alternate Hypothesis H1: Effect of onVentilatorCurrently is significant on Death for year 2020

# Linear Regression
linear_model_2 <- lm(death ~ onVentilatorCurrently, data_covid_df_2020)
summary(linear_model_2)

# Robust Standard Error
var_cov <- vcovHC(linear_model_2, type = "HC1")
coeftest(linear_model_2, var_cov)

# Plotting variables
plot(data_covid_df_2020$onVentilatorCurrently, data_covid_df_2020$death,
     col = "steelblue",
     pch = 20,
     xlab = "Ventilator", 
     ylab = "Number of deaths",
     cex.main = 0.9,
     main = "Number of deaths vs. Patients on Ventilator (2020)")
# add the regression line to the plot
abline(linear_model_2, 
       col = "red", 
       lwd = 2)

# As the p-value for the predictor "onVentilatorCurrently" is less than 0.05, we reject the null hypothesis.
# Therefore, the effect of "onVentilatorCurrently" on Death is significant.

# P/N Ratio V/S Death (2021)

# HYPOTHESIS 3 (P/N Ratio V/S Death):

# Null Hypothesis H0: Effect of P/N Ratio is insignificant on Death for year 2021
# Alternate Hypothesis H1: Effect of P/N Ratio is significant on Death for year 2021

# Quadratic Model
quadratic_model_1 <- lm(death ~ P.Nratio + I(P.Nratio^2), data = data_covid_df_2021)
summary(quadratic_model_1)

# Robust Standard Error
var_cov <- vcovHC(quadratic_model_1, type = "HC1")
coeftest(quadratic_model_1, var_cov)

# Plotting variables
plot(data_covid_df_2021$P.Nratio, data_covid_df_2021$death,
     col = "steelblue",
     pch = 20,
     xlab = "P/Nratio", 
     ylab = "Number of deaths",
     cex.main = 0.9,
     main = "Number of deaths vs. Positive/Negative Ratio (2021)")
# add the regression line to the plot
order_id <- order(data_covid_df_2021$P.Nratio)
lines(x = data_covid_df_2021$P.Nratio[order_id], 
      y = fitted(quadratic_model_1)[order_id],
      col = "red", 
      lwd = 2)

# As the p-value for the predictor "P/N Ratio" is less than 0.05, we reject the null hypothesis.
# Therefore, the effect of "P/N Ratio" on Death is significant.

# P/N Ratio V/S Death (2020)

# HYPOTHESIS 4 (P/N Ratio V/S Death):

# Null Hypothesis H0: Effect of P/N Ratio is insignificant on Death for year 2020
# Alternate Hypothesis H1: Effect of P/N Ratio is significant on Death for year 2020

# Non-Linear (Quadratic)
quadratic_model_2 <- lm(death ~ P.Nratio + I(P.Nratio^2), data = data_covid_df_2020)
summary(quadratic_model_2)

# Robust Standard Error
var_cov <- vcovHC(quadratic_model_2, type = "HC1")
coeftest(quadratic_model_2, var_cov)

# Plotting variables FOR 2020
plot(data_covid_df_2020$P.Nratio, data_covid_df_2020$death,
     col = "steelblue",
     pch = 20,
     xlab = "P/Nratio", 
     ylab = "Number of deaths",
     cex.main = 0.9,
     main = "Number of deaths vs. Positive/Negative Ratio (2020)")
# add the regression line to the plot
order_id <- order(data_covid_df_2020$P.Nratio)
lines(x = data_covid_df_2020$P.Nratio[order_id], 
      y = fitted(quadratic_model_2)[order_id],
      col = "red", 
      lwd = 2)

# As the p-value for the predictor "P/N Ratio" is less than 0.05, we reject the null hypothesis.
# Therefore, the effect of "P/N Ratio" on Death is significant.

# HospitalizedCurrently V/S Death (2021)

# HYPOTHESIS 5 (HospitalizedCurrently V/S Death):

# Null Hypothesis H0: Effect of HospitalizedCurrently is insignificant on Death for year 2021
# Alternate Hypothesis H1: Effect of HospitalizedCurrently is significant on Death for year 2021

# Linear Regression
linear_model_3 <- lm(death ~ hospitalizedCurrently, data_covid_df_2021)
summary(linear_model_3)

# Robust Standard Error
var_cov <- vcovHC(linear_model_3, type = "HC1")
coeftest(linear_model_3, var_cov)

# Plotting variables
plot(data_covid_df_2021$hospitalizedCurrently, data_covid_df_2021$death,
     col = "steelblue",
     pch = 20,
     xlab = "hospitalizedCurrently", 
     ylab = "Number of deaths",
     cex.main = 0.9,
     main = "Number of deaths vs. hospitalizedCurrently (2021)")
# add the regression line to the plot
abline(linear_model_3, 
       col = "red", 
       lwd = 2)

# As the p-value for the predictor "hospitalizedCurrently" is less than 0.05, we reject the null hypothesis.
# Therefore, the effect of "hospitalizedCurrently" on Death is significant.

# hospitalizedcurrently V/S Death (2020)

# HYPOTHESIS 6 (HospitalizedCurrently V/S Death):

# Null Hypothesis H0: Effect of HospitalizedCurrently is insignificant on Death for year 2020
# Alternate Hypothesis H1: Effect of HospitalizedCurrently is significant on Death for year 2020

# Linear Regression
linear_model_4 <- lm(death ~ hospitalizedCurrently, data_covid_df_2020)
summary(linear_model_4)

# Robust Standard Error
var_cov <- vcovHC(linear_model_4, type = "HC1")
coeftest(linear_model_4, var_cov)

# Plotting variables
plot(data_covid_df_2020$hospitalizedCurrently, data_covid_df_2020$death,
     col = "steelblue",
     pch = 20,
     xlab = "hospitalizedCurrently", 
     ylab = "Number of deaths",
     cex.main = 0.9,
     main = "Number of deaths vs. hospitalizedCurrently (2020)")
# add the regression line to the plot
abline(linear_model_4, 
       col = "red", 
       lwd = 2)

# As the p-value for the predictor "hospitalizedCurrently" is less than 0.05, we reject the null hypothesis.
# Therefore, the effect of "hospitalizedCurrently" on Death is significant.

# inIcuCurrently V/S Death (2021)

# HYPOTHESIS 7 (inIcuCurrently V/S Death):

# Null Hypothesis H0: Effect of inIcuCurrently is insignificant on Death for year 2021
# Alternate Hypothesis H1: Effect of inIcuCurrently is significant on Death for year 2021

# Linear Regression
linear_model_5<- lm(death ~ inIcuCurrently, data_covid_df_2021)
summary(linear_model_5)

# Robust Standard Error
var_cov <- vcovHC(linear_model_5, type = "HC1")
coeftest(linear_model_5, var_cov)

# Plotting variables
plot(data_covid_df_2021$inIcuCurrently, data_covid_df_2021$death,
     col = "steelblue",
     pch = 20,
     xlab = "inIcuCurrently", 
     ylab = "Number of deaths",
     cex.main = 0.9,
     main = "Number of deaths vs. inIcuCurrently (2021)")
# add the regression line to the plot
abline(linear_model_5, 
       col = "red", 
       lwd = 2)

# As the p-value for the predictor "inIcuCurrently" is less than 0.05, we reject the null hypothesis.
# Therefore, the effect of "inIcuCurrently" on Death is significant.

# inIcuCurrently V/S Death (2020)

# HYPOTHESIS 8 (inIcuCurrently V/S Death):

# Null Hypothesis H0: Effect of inIcuCurrently is insignificant on Death for year 2020
# Alternate Hypothesis H1: Effect of inIcuCurrently is significant on Death for year 2020

# Linear Regression
linear_model_6 <- lm(death ~ inIcuCurrently, data_covid_df_2020)
summary(linear_model_6)

# Robust Standard Error
var_cov <- vcovHC(linear_model_6, type = "HC1")
coeftest(linear_model_6, var_cov)

# Plotting variables
plot(data_covid_df_2020$inIcuCurrently, data_covid_df_2020$death,
     col = "steelblue",
     pch = 20,
     xlab = "inIcuCurrently", 
     ylab = "Number of deaths",
     cex.main = 0.9,
     main = "Number of deaths vs. inIcuCurrently (2020)")
# add the regression line to the plot
abline(linear_model_6, 
       col = "red", 
       lwd = 2)

# As the p-value for the predictor "inIcuCurrently" is less than 0.05, we reject the null hypothesis.
# Therefore, the effect of "inIcuCurrently" on Death is significant.

robust_list <- list(sqrt(diag(vcovHC(linear_model_1, type = "HC1"))),
                    sqrt(diag(vcovHC(linear_model_2, type = "HC1"))),
                    sqrt(diag(vcovHC(linear_model_3, type = "HC1"))),
                    sqrt(diag(vcovHC(linear_model_4, type = "HC1"))),
                    sqrt(diag(vcovHC(linear_model_5, type = "HC1"))),
                    sqrt(diag(vcovHC(linear_model_6, type = "HC1"))),
                    sqrt(diag(vcovHC(quadratic_model_1, type = "HC1"))),
                    sqrt(diag(vcovHC(quadratic_model_2, type = "HC1"))))
library(stargazer)
mymodel<-list(linear_model_1,linear_model_2,linear_model_3,linear_model_4,linear_model_5,linear_model_6,quadratic_model_1,quadratic_model_2)
stargazer(mymodel,
          type="html",
          digits = 3,
          se = robust_list,
          title = "Regression Models",
          out="reg_model_final.htm")

#####################################################################
