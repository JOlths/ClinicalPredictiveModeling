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
data$famhistovca %<>% as.factor()

