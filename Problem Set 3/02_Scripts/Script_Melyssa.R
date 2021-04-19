###Problem Set 3

##Install packages
install.packages("wooldridge")
library(tidyverse)
library(kableExtra)
library(wooldridge)
knitr::opts_chunk$set(echo = TRUE, message=FALSE, warning = FALSE)

##Exercise 1
#Deniz Güvercin (2019): Women in Politics and Child Labor: an Instrumental Variable Approach
#Research question = How increasing the participation of women in government-level decisions decreases child labor?
#Instrument variable (z) = quotas * gender index (i.e., gender electoral quota interacted with an index displaying the intensity of gender marking in language)
#Treatment variable (d) = female seat share (i.e., share of parliamentary seats held by women)
#Outcome variable (y) = child labor (i.e., percentage of children aged 5 to 14 years involved in child labor activities)
#The instrumental variable approach is used to avoid endogeneity issues because of omitted variable bias that would generate biased coefficients, leading to nonidentification of the correct impact of the main variable of interest (female seat share) on the dependent variable (child labor). Therefore, using an instrumental variable approach enables to overcome any potential biased coefficients produced by OLS regression (to leverage partially exogenous treatment variation in observational studies).
#CRITIC: the IV approach in this study does not satisfy one of the three requirements: Z is random or "as if" random [a.k.a. independence assumption.
                                                                                                                     