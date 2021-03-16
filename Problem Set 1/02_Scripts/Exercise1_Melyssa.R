### Melyssa - Exercise 1

## Install needed packages
install.packages("kableExtra")
install.packages("tidyverse")
install.packages("DiagrammeR")

## Exercise 1.1 Generate a simulated data set:
# Step 1: install and upload "DiagrammeR"
install.packages("DiagrammeR")
library("DiagrammeR", quietly = TRUE)

# Step 2: using the function grViz
grViz("digraph boxes_and_circles {
node [shape = circle]
x1; x2; e
node [shape = square]
y
x1 -> y; x2 -> y; e -> y
}
")

## Exercise 1.2 Estimate three regression models 
# Step 1: reproducibility
set.seed(15032021)

# Step 2: create a data frame
df <- tibble(
  x1 = rnorm(n = 5000, mean = 0, sd = 15),
  x2 = rnorm(n = 5000, mean = -10, sd = 1/2),
  e = rnorm(n = 5000, mean = 0, sd = 1)
)
  
#set population parameters
a <- -10
b1 <- 0.1
b2 <- 2

#create new variables within a data frame and reorder variables
df <- df %>% 
  mutate(y = a + b1 * x1 + b2 * x2 + e) %>% 
  relocate(y, x1, x2, e)  
  
#visualize the simulated data
View(df)

# Step 3: fit three linear regression models
fit1 <- lm(y ~ x1, data = df)
coef(fit1)

fit2 <- lm(y ~ x2, data = df)
coef(fit2)

fit3 <- lm(y ~ x1 + x2, data = df)
coef(fit3)

# Step 4: create a regression table with all three models with stargazer::stargazer()
install.packages("stargazer")
install.packages("sjPlot")
library(modelr)
library(sjPlot)
library(stargazer)

stargazer::stargazer(fit1, fit2, fit3, type = "html", style = "apsr", out = "table1.html")

# Step 5: predict values with sjPlot::plot_model()
plot1 <- sjPlot::plot_model(fit3,
                         type = "pred",
                         terms = "x1")
plot2 <- sjPlot::plot_model(fit3,
                         type = "pred",
                         terms = "x2")
#combine the 2 plots with patchwork
install.packages("patchwork")
library(patchwork)
plot1 / plot2

## Exercise 1.3 comment the estimated coefficients from the three models
##1 how do they compare with the assumed parameters?

#fit1
coef(fit1)
#assumed: y = -10 + 0.1 * x1
#estimated: y= -29.98 + 0.098 * x1 + e

#fit2
coef(fit2)
#assumed: y = -10 + 2 * x2
#estimated: y= -10.37 + 1.96 * x2 + e

coef(fit3)
#assumed: y = -10 + 0.1 * x1 + 2 * x2
#estimated: y= -10.066 + 0.098 * x1 + 1.99 * x2 + e

##2 How does the interpretation of `b1` change between model 1 and model 3?
coef(fit1)
coef(fit3)
#model 1: b1 = 0.09823371 
#model 3: b1 = 0.09891204 the effect of "b1" is stronger in model 3

y = a + b1 * x1 + b2 * x2 + e

##3 Finally, compare the effect of `x1` and the effect of `x2`: which one is stronger?
coef(fit1)
coef(fit2)
# the effect on x2 is stronger / b1 = 0.09823371 VS b2 = 1.960218 

##4 What is the predicted value of `y` when `x1 = 20` and `x2 = 0`?
# y = -10.066 + 0.098 * 20 + 1.991 * 0 + e





 

