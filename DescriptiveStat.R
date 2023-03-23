#libraries
list.to.install <- c('caret', 'ggplot2', 'dplyr', 'magrittr')
lapply(list.to.install, require, character.only=T)

#load package
data <- read.delim("ovarian.txt")
View(data)

#convert to dataframe
data.datafr <- as.data.frame(data)
head(data.datafr)

#look for % of malignancies (outcome variable)
data.datafr %>%
  count(mal) %>%
  mutate(percentage = n / sum(n)) %>%
  select(., -n)
