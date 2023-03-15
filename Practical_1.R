## Install packages ####
library(readxl)
library(dplyr)
library(magrittr)
library(ggplot2)


## Load dataset ####
covidpredict <- readxl::read_excel('covidpredict-1.xlsx')
View(covidpredict)

# select only patients recruited between 6 Feb - 20 May (indicated as 'dev')
# and remove 'set' column
covidpredict.training <- covidpredict %>%
  dplyr::filter(set == 'dev') %>%
  as.data.frame()

covidpredict.training %<>% select(., -set)

## descriptive statistics ####
summary(covidpredict.training)

#change binary/categorical variables to their respective datatype
covidpredict.training$female %<>% as.factor()
covidpredict.training$comorbidity %<>% as.factor()
covidpredict.training$mortality %<>% as.factor()
covidpredict.training$date %<>% as.Date()

#check for missing values
sapply(covidpredict.training, function(x) sum(is.na(x)))

#check for length of each column
sapply(covidpredict.training, function(x) length(x))

#graph boxplots for scaled data
covidpredict.training %>%
  select(., -date, -female, -mortality, -comorbidity) %>%
  scale()  %>%
  data.frame() %>%
  stack() %>%
  ggplot(aes(x=ind, y=values)) +
  geom_boxplot() +
  xlab('Variables') +
  ylab('Values')

## Generalized Linear Model ####
glm.covidpredict <- glm(data = covidpredict.training,
                        formula = mortality ~ female + comorbidity + rr +
                          oxygen_sat + urea + crp + gcs + age,
                        family = binomial)
summary(glm.covidpredict)

## Assignment 2: personal risk score ####
new_data <- data.frame(female=as.factor(0), comorbidity=as.factor(1), rr=22, oxygen_sat=94,
                       gcs=15, urea=7, crp=84.9, age=27)
new_data

stats::predict(glm.covidpredict, new_data, type='response')

## Assignment 3: 47 year old personal risk score ####
new_data.47 <- data.frame(female=as.factor(0), comorbidity=as.factor(1), rr=22, oxygen_sat=94,
                       gcs=15, urea=7, crp=84.9, age=47)
new_data.47

stats::predict(glm.covidpredict, new_data.47, type='response')

## Assignment 4: 70 year old personal risk score ####
new_data.77 <- data.frame(female=as.factor(0), comorbidity=as.factor(1), rr=22, oxygen_sat=94,
                          gcs=15, urea=7, crp=84.9, age=77)
new_data.77

stats::predict(glm.covidpredict, new_data.77, type='response')

## Assignment 5 ####
new_data.76 <- data.frame(female=as.factor(0), comorbidity=as.factor(2), rr=35, oxygen_sat=76,
                          gcs=15, urea=34, crp=200, age=77)

new_data.76

stats::predict.glm(glm.covidpredict, new_data.76, type = 'response')
