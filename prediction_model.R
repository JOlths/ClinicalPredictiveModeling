setwd("C:/Users/cschulat/OneDrive - JNJ/Master/Maastricht/09_Prediction Models/Assignment")
data <- read.delim("ovarian.txt")

library(mice)
library(pROC)
library(dplyr)
library(yardstick)

summary(data)

### check for missing data
### result: no missings
md.pattern(data)

### some variables are coded as numerical but represent categories
### save as factors to make sure that dummy variables are created in the regression model
data$Ascites <- as.factor(data$Ascites)
data$papflow <- as.factor(data$papflow)
data$wallreg <- as.factor(data$wallreg)
data$Shadows <- as.factor(data$Shadows)
data$pershistovca <- as.factor(data$pershistovca)
### family history has more categories but due to low numbers it makes sense to re-categorize into 0, 1 and 2 or more
data$famhistovca <- as.factor(ifelse(data$famhistovca == 0, 0, ifelse(data$famhistovca == 1, 1, 2)))
data$hormtherapy <- as.factor(data$hormtherapy)
data$pain <- as.factor(data$pain)
data$colscore <- as.factor(data$colscore)
data$nrloculescat <- as.factor(data$nrloculescat)
data$papnr <- as.factor(data$papnr)
data$oncocenter <- as.factor(data$oncocenter)
data$mal <- as.factor(data$mal)

summary(data)

### full model including all candidate variables
model <- glm(mal ~ Age + Ascites + papflow + lesdmax + soldmaxorig + wallreg 
             + Shadows + famhistovca + pershistovca + hormtherapy + pain + colscore 
             + CA125 + nrloculescat + papnr + oncocenter, data = data, family = binomial)
summary(model)

### alpha level of 0.01 is chosen for variables selection
### backward selection is applied

### 1st step: omit hormtherapy
model <- glm(mal ~ Age + Ascites + papflow + lesdmax + soldmaxorig + wallreg 
             + Shadows + famhistovca + pershistovca  + pain + colscore 
             + CA125 + nrloculescat + papnr + oncocenter, data = data, family = binomial)
summary(model)

### 2nd step: omit lesdmax
model <- glm(mal ~ Age + Ascites + papflow  + soldmaxorig + wallreg 
             + Shadows + famhistovca + pershistovca  + pain + colscore 
             + CA125 + nrloculescat + papnr + oncocenter, data = data, family = binomial)
summary(model)

### 3rd step: omit papnr
model <- glm(mal ~ Age + Ascites + papflow  + soldmaxorig + wallreg 
             + Shadows + famhistovca + pershistovca  + pain + colscore 
             + CA125 + nrloculescat + oncocenter, data = data, family = binomial)
summary(model)

### selection process complete
### for all other categorical variables at least 1 dummy is below significance threshold
### --> keep all in

### perform prediction
data$prediction <- predict(model, data, type = "response")
roc <- roc(data$mal, data$prediction)
auc(roc)
plot.roc(roc, print.auc = TRUE)

### obtain optimal threshold from ROC curve
threshold <- coords(roc, "best", ret = "threshold")

### check sensitivity, specificity & predictive values
for (i in 1:nrow(data)){
data$predicted_class[i]<-ifelse(data$prediction[i]>threshold,1,0)
}

matrix<-table(data$predicted_class, data$mal)
matrix
diagnostics <- bind_rows(sens(matrix), spec(matrix), ppv(matrix), npv(matrix))
diagnostics
