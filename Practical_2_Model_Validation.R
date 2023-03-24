## Install packages ####
library(readxl)
library(dplyr)
library(magrittr)
library(ggplot2)

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

#change binary/categorical variables to their respective datatype
covidpredict.training$female %<>% as.factor()
covidpredict.training$comorbidity %<>% as.factor()
covidpredict.training$mortality %<>% as.factor()
covidpredict.training$date %<>% as.Date()

#check for missing values
sapply(covidpredict.training, function(x) sum(is.na(x)))

#check for length of each column
sapply(covidpredict.training, function(x) length(x))

# model training
glm.covidpredict <- glm(data = covidpredict.training,
                        formula = mortality ~ female + comorbidity + rr +
                          oxygen_sat + urea + crp + gcs + age,
                        family = binomial)
summary(glm.covidpredict)

## Assignment 1 ####
