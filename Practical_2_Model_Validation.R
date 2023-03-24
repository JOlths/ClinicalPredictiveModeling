## Install packages ####
library(readxl)
library(dplyr)
library(magrittr)
library(ggplot2)
library(rms)
library(rmda)

##load dataset ####
covidpredict <- readxl::read_excel('covidpredict-1.xlsx')
View(covidpredict)

##code with data manipulation from last session ####
# select only patients recruited between 6 Feb - 20 May (indicated as 'dev')
# and remove 'set' column
covidpredict.training <- covidpredict %>%
  dplyr::filter(set == 'dev') %>%
  as.data.frame()

covidpredict.training %<>% select(., -set)



#check for missing values
sapply(covidpredict.training, function(x) sum(is.na(x)))

#check for length of each column
sapply(covidpredict.training, function(x) length(x))

# model training
lrm.covidpredict <- lrm(data = covidpredict.training,
                        formula = mortality ~ female + comorbidity + rr +
                          oxygen_sat + urea + crp + gcs + age + age*comorbidity, x=T, y=T)

## Exercise 1 ####
rms::validate(lrm.covidpredict, method = 'boot', B=40)

#apparent calibration slope: 1.00
# optimism correced calibration slope: 0.9831
# most reliable performance with new patients: optimism corrected slope

## Exercise 2 ####
C_stat <- function(Dxy) {return(0.5*(Dxy+1))}
C_stat(0.5458) #original C-statistic
C_stat(0.5399) #optimism corrected C-statistic. This one gives the best performance with new patient inputs

## Exercise 3 ####
res <- rms::calibrate(lrm.covidpredict, B=200)
plot(res)
 #decent prediction, some under and overprediction but in general quite decent

## Assignment 2 ####
covidpredict.test <- covidpredict %>%
  dplyr::filter(set == 'val') %>%
  as.data.frame()

head(covidpredict.test)


predictions <- stats::predict(lrm.covidpredict, covidpredict.test, type='fitted')
head(predictions)

validation <- rms::val.prob(predictions, covidpredict.test$mortality, g=10)

## Assignment 3 ####
covidpredict.test$prediction <- predictions
head(covidpredict.test$prediction)

dca <- rmda::decision_curve(data = covidpredict.test,
                            formula = mortality ~ prediction,
                            fitted.risk = T)
rmda::plot_decision_curve(dca)

s <- summary(dca, measure="TPR")
s$
summary(dca, measure="FPR")
