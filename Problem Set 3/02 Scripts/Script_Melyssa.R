### Problem Set 3

## Install packages
# install.packages("wooldridge")
library(tidyverse)
library(kableExtra)
library(wooldridge)
knitr::opts_chunk$set(echo = TRUE, message=FALSE, warning = FALSE)



# Exercise 1 --------------------------------------------------------------

### Deniz Güvercin (2019): Women in Politics and Child Labor: an Instrumental Variable Approach

# Research question = How increasing the participation of women in government-level decisions decreases child labor?

# Instrument variable (z) = quotas * gender index (i.e., gender electoral quota interacted with an index displaying the intensity of 
# gender marking in language)

# Treatment variable (d) = female seat share (i.e., share of parliamentary seats held by women)

# Outcome variable (y) = child labor (i.e., percentage of children aged 5 to 14 years involved in child labor activities)

# The instrumental variable approach is used to avoid endogeneity issues because of omitted variable bias that would generate biased 
# coefficients, leading to nonidentification of the correct impact of the main variable of interest (female seat share) on the dependent 
# variable (child labor). Therefore, using an instrumental variable approach enables to overcome any potential biased coefficients 
# produced by OLS regression (to leverage partially exogenous treatment variation in observational studies).

# CRITIC: the IV approach in this study does not satisfy one of the three requirements: Z is random or "as if" random 
# [a.k.a. independence assumption].



### Muntasir Murshed (2019): Are Trade Liberalization policies aligned with Renewable Energy Transition in low and middle 
# income countries? An Instrumental Variable approach

# Instrument variables (z) = Real Exchange Rate, Terms of Trade, Tariffs levied on imports

# Treatment variable (d) = Trade Liberalization (i.e., trade openness: total volume of imports and exports)

# Outcome variable (y) = RET (i.e., four indicators of RET: renewable energy consumption, renewable energy share, 
# intensity of renewable energy use, access to clean fuel and technology for cooking)

# The instrumental variable approach is used to modify the problematic endogenous regressor/s being correlated with the error term.

# CRITIC: the IV approach in this study does not satisfy one of the three requirements: Z is random or "as if" random 
# [a.k.a. independence assumption].



### Daniel Kim, Christopher F Baum, Michael Ganz, S V Subramanian, and Ichiro Kawachi (2011): The contextual effects 
# of social capital on health: a cross national instrumental variable analysis

# Instrument variables (z) = Corruption, Higher population densities, Religious fractionalization 

# Treatment variable (d) = Social trust

# Outcome variable (y) = Individual self-rated health

# The instrumental variable approach is used to modify the problematic endogenous regressor/s being correlated with the error term.



# Exercise 2 --------------------------------------------------------------

# Pregnant women would be randomly assigned to smoking (-> create treatment and control groups). Afterwards we could compare the 
# group means to get the causal effect of smoking during pregnancy on the weight of the babies.
# However we could not run such an experiment for ethical reasons in this case! But we could use the instrumental varaible(s) approach
# instead.

# Instrumental variable: Anti-Smoking Campaign 

# Three requirements:
# 1) Z has a causal effect on the endogenous treatment D: Kenneth Warner (1977) found that anti-smoking campaign has a 
# causal effect on cigarette consumption. It could be that anti-smoking campaigns have an effect on mothers’ cigarette consumption.

# 2) Z is random because we can randomly expose one group to the treatment “anti-smoking campaign” and the other group 
# does not get the treatment. The question is however, how can we ensure that only one group is exposed to advertising 
# over a longer period of time...

# 3) Z affects Y only though D: it should be the case because it should not be possible that one “anti-smoking campaign” 
# affects the weight of babies.



# Exercise 3 --------------------------------------------------------------

### 3.1

#first linear model:
data <- import(file = here::here("Problem Set 3", "01 Data", "BWGHT.DTA"))
fit1 <- lm(bwght ~ cigs, data = data)
summary(fit1)
# findings: the number of cigarettes smoked per day has a negative effect on birth weight in ounces. So on average: if the number of 
# cigarettes smoked by the mother during pregnancy increases by 1, the birth weight decreases by ~0.5 ounces. The result is statistically
# significant (p-value = 1.662e-08)
# This is not an estimate for the causal effect of smoking because endogeneity issues can still occur because of 
# omitted variables bias could generate biased coefficients (treatment:smoking during pregnancy is not randomly assigned -> selection bias!)

#potential omitted variables (second model):
fit2 <- lm(bwght ~ cigs + motheduc + cigprice, data = data)
summary(fit2)
# findings: the estimate is changing slightly. The "causal" effect is less strong. 



### 3.2

#instrumental variable: mother years of education
#satisfy the first requirement: Z has a causal effect on the endogenous treatment D (see model fit4)
#CRITIC: Z is not random
#satisfy requirement 3: Z affects Y only through D: the mother years  of education cannot directly affect the weight of the baby
fit3 <- lm(cigs ~ motheduc, data = data)
summary(fit3)

### alternative approach (because the mother's years of education is not radnomly assinged...)
# to find a viable instrument, the 3 requirements should be met.
# 1) Z has a causal effect on the endogenous treatment D
# 2) Z is random or "as if" random 
# 3) Z affects Y only through D

# which variables in the data set met these 3 requirements?
# => cigtax, cigprice -> both random or as if random and affect the babies weight only through D. Now we need to see if there is
# is a strong causal effect between Z and D.
summary(lm(cigs ~ cigtax, data = data))
summary(lm(cigs ~ cigprice, data = data))
# the assumed causal effect is not strong (~ 0.022 & ~ 0.006)
# but in my opinion all the other variables are not viable instruments because they violate other requirements...
# so let's take cigtax as instrument


### 3.3
model_first_stage <- lm(cigs ~ motheduc, data = data)
model_iv <- lm(bwght ~ cigs | motheduc, data = data)


### alternative approach
# first stage
model_first_stage_alt <- lm(cigs ~ cigtax, data = data)
# compute D-hat (predict) and add it to data set
data <-  data %>%
  mutate(D_hat = predict(model_first_stage_alt))


# second stage
model_iv_alt <- lm(bwght ~ D_hat, data = data)
summary(model_iv_alt)

# on average: if the number of cigarettes smoked by the mother during pregnancy increases by 1, the birth weight increases by 5.55 ounces.
# => strong positive causal effect. But take a look at the p-value (0.075) -> not statistically significant


### 3.4
# => https://www.rdocumentation.org/packages/AER/versions/1.2-9/topics/ivreg
library(AER)
model_iv_2SLS <- ivreg(bwght ~ cigs | cigtax, data = data)
summary(model_iv_2SLS)

# the causal effect is the same as in the manual model. But the p-value is even bigger (0.3821) -> not statistically significant!
# I'm a bit confused now. Does this suggest, that smoking does not have an causal effect on the babies weight?


### 3.5
library(stargazer)
stargazer(fit1, model_iv_alt, model_iv_2SLS,
          type = "html",
          out = "PS3_table.html")



