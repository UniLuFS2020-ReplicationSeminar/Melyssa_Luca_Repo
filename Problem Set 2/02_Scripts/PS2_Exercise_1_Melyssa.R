### Exercise 1 Melyssa

## install packages
library(tidyverse)
library(kableExtra)
library(DiagrammeR)
knitr::opts_chunk$set(echo = TRUE, message=FALSE, warning = FALSE)

### exercise 1
## exercise 1.1

# reproducibility
set.seed(24032021)

#generate three exogenous variables
df <- tibble(
  D = rnorm(n = 5000, mean = 0, sd = 1),
  d = rbinom(5000, 1, 0.5),
  e = rnorm(n = 5000, mean = 0, sd = 1/5)
)

#generate endogenous variable

y1 <- 1 # with insurance
y0 <- 0 # with no insurance 

df <- df %>% mutate(y1 = ifelse(test = d == 0, yes = y0 + e, no = y0 + 0.5 + e))

##exercise 1.2
#estimate a linear regression model

fit <- lm(y1 ~ d, data = df)

summary(lm(y1 ~ d, data = df))

#exercise 1.3
#generate y

df <- df %>% mutate(y = ifelse(test = d == 1, yes = y1, no = y0))
    
#the data generating process is visible because we are simulating it. In standard data analysis, the goal is to understand what causes y.
#so we would observe which variable causes y but not the potential outcomes variables in a standard data analysis.
#errors are never observed in standard data analysis. We can observe residuals from a model.

#exercise 1.4
#subset data frame by group 

treatmentgroup <- df %>% 
  filter(d == 1)

controlgroup <- df %>% 
  filter(d != 1)

#compute the observed average difference between the treatment and control group
mean(treatmentgroup$y) - mean(controlgroup$y)

#results 0.5013113 --> close to the average treatment effect ("0.5") and also close to the linear regression model (i.e., 0.503219).


#exercise 1.5
#compute the average treatment effect on the treated and the selection bias
mean(treatmentgroup$y1) - mean(treatmentgroup$y0)
