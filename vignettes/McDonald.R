## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(dplyr)

## ----setup--------------------------------------------------------------------
library(McDonald)

## ----message = FALSE, warning=FALSE-------------------------------------------
library(McDonald)

x <- 180 #size in centimeters
y <- 75 #weight in kilos
bmi <- McDonald::bmi(x,y)
bmi %>% kableExtra::kable(col.names = "BMI", align = "l")

## ----message = FALSE, warning=FALSE-------------------------------------------
y <- 75 #weight in kilos
coefficient <- 1 #coefficient corresponding to the gender (1 for male and 0.9 for female)
d <- 24 #fix value
leanfactor <- 0.9 #lean factor that correspond to the gender and its respective BMI (see table), here 0.9
bmr <- McDonald::bmr(y, coefficient, d, leanfactor)
bmr %>% kableExtra::kable(col.names = "BMR", align = "l")

## ----message = FALSE, warning=FALSE-------------------------------------------
multiplier <- 1.65 #depends on your weekly phisical activites, here moderate activity = 1.65
needs <- McDonald::needs(bmr, multiplier)
needs %>% kableExtra::kable(col.names = "Daily calorie need", align = "l")

## -----------------------------------------------------------------------------
y <- 75
MET <- 3 #normal walking = 3, fast walking = 4.5
e <- 3.5
f <- 200
kcal <- 850 #calories absorbed
walking_time <- McDonald::walking_time(y, MET, e, f, kcal) %>% round(digits = 0)
walking_time %>% kableExtra::kable(col.names = "Walking time in minutes at a normal pace", align = "l")


## ----eval=FALSE---------------------------------------------------------------
#  McDonald::run()

