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
  x = rnorm(n = 5000, mean = 0, sd = 1),
  D = rbinom(5000, 1, 0.5),
  e = rnorm(n = 5000, mean = 0, sd = 1/5)
)

#generate endogenous variable
df <- df %>% 
  mutate(y1 = 0 + 0.5 * x + D + e) %>% 
  relocate(y1, D, x, e)  

D = 0 # with no insurance 
D = 1 # with insurance

if(D == 0){print("y1 = y0 + e")} else{print("y1 = y0 + 0.5 + e")}

##exercise 1.2
#estimate a linear regression model

fit <- lm(y1 ~ D, data = df)
fit2 <- lm(y1 ~ x, data = df)

summary(lm(y1 ~ D, data = df))
summary(lm(y1 ~ x, data = df))

#exercise 1.3

predict(fit)

#the data generating process is visible because we are simulating it. In standard data analysis, the goal is to understand what causes y.
#so we would observe which variable causes y but not the potential outcomes variables in a standard data analysis.
#errors are never observed in standard data analysis. We can observe residuals from a model.

#exercise 1.4

y <- c(0, 1)
mean(y, na.rm = TRUE)

#exercise 1.5

