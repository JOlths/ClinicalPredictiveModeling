#import packages
list.of.packages <- c('magrittr', 'ggplot2', 'dplyr', 'caret', 'rms', 'mice')
lapply(list.of.packages, require, character.only=T)

#load data
data <- read.delim("ovarian.txt")

#check for missing variables
mice::md.pattern(data)

summary(data)

#change to appropriate datatype
data$Ascites %<>% as.factor()
data$papflow %<>% as.factor()
data$wallreg %<>% as.factor()
data$Shadows %<>% as.factor()
data$pershistovca %<>% as.factor()
data$famhistovca <- as.factor(ifelse(data$famhistovca == 0, 0, ifelse(data$famhistovca == 1, 1, 2)))
data$hormtherapy %<>% as.factor()
data$pain %<>% as.factor()
data$colscore %<>% as.factor()
data$nrloculescat %<>% as.factor()
data$papnr %<>% as.factor()
data$oncocenter %<>% as.factor()
data$mal %<>% as.factor()

## Model building ####
lg_model.1 <- rms::lrm(
  data = data,
  formula = mal ~ .,
  x = T, y = T
)

lg_model.1

#validate
tab <- rms::validate(lg_model.1, method = 'boot', B=40)
C_stat <- function(Dxy) {return(0.5*(Dxy+1))}
sapply(tab[1,], C_stat)
