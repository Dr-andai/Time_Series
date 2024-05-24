## EEG Sleep State

## Importance
# It is quite important in the applications of medicine; for example, 
# the disabled people are able to interact better by utilizing this way, 
# and this is achieved by founding a method that acts like a bridge between 
# computer and human brain. 
library("rio")
library("rstatix")
library("tidyverse")
library("dplyr")
library(FactoMineR)
library(factoextra)

eeg <- import("EEG/EEG Eye State.arff")

names(eeg)

# AF3: Frontal lobe, left hemisphere
# F7: Frontal lobe, left hemisphere, near the temple
# F3: Frontal lobe, left hemisphere
# FC5: Frontal lobe, left hemisphere, near the center
# T7: Temporal lobe, left hemisphere
# P7: Parietal lobe, left hemisphere
# O1: Occipital lobe, left hemisphere
# O2: Occipital lobe, right hemisphere
# P8: Parietal lobe, right hemisphere
# T8: Temporal lobe, right hemisphere
# FC6: Frontal lobe, right hemisphere, near the center
# F4: Frontal lobe, right hemisphere
# F8: Frontal lobe, right hemisphere, near the temple
# AF4: Frontal lobe, right hemisphere


# DATA ANALYSIS

# Eye Closed
eye_closed <- eeg[eeg$eyeDetection == 1, ]
# Eye Open
eye_open <- eeg[eeg$eyeDetection == 0, ]

# dropping the "eye detection" column
eye_closed <- eye_closed[, -15]
eye_open <- eye_open[, -15]

# STATS
eeg_closed_stats <- get_summary_stats(eye_closed)
eye_open_stats <- get_summary_stats(eye_open)

# dropping the "eye detection" column
eeg_variables <- eeg[, -15]


# STATS
eeg_stats <- get_summary_stats(eeg_variables)

## -visualize on the summary stats
# visualize max col
eeg_stats %>% 
  ggplot(aes(max,variable))+
  geom_point()
  
# correlation matrix
corr_eeg <- cor(eeg_variables)
corr_eeg <- round(corr_eeg, 2)

library("corrplot")
corrplot(corr_eeg)

pairs(corr_eeg)
## review eeg theory

## PCA plotting
# - Dimensionality reduction
# - Data visualization
# - Feature engineering

eeg_prcomp <- prcomp(eeg_variables, scale=TRUE)
eeg_prcomp <- as.data.frame(eeg_prcomp$x)
eeg_prcomp_X$label <- eeg$eyeDetection


plot(eeg_prcomp_x$PC1,
     eeg_prcomp_x$AF3,
     col = eeg_prcomp_x$label,
     pch = 19,
     main="Figure 1: 2D Scatterplot",
     log = "y",
     xlab="PC1",
     ylab="AF3")
legend("bottomright",
       legend = levels(eeg_prcomp_x$label),
       col = seq_along(levels(eeg_prcomp_x$label)),
       pch = 19)

fviz_eig (eeg_prcomp, addlabels = TRUE)


# scree plot of variance
fviz_eig (eeg_prcomp, addlabels = TRUE)
# Biplot
fviz_pca_biplot(eeg_prcomp)
eeg_prcomp_x$label <- eeg$eyeDetection
#Biplot with colored groups
fviz_pca_biplot(eeg_prcomp, label = "var", habillage = eeg_prcomp_x$label)
install.packages("caret")
View(eeg)
library(caret)
id <- createDataPartition(eeg$eyeDetection, p = 0.8, list = FALSE)
train = eeg[id, ]
test = eeg[-id, ]
View(train)
cb = class::lvqinit(train[1:4], train$eyeDetection)
# training set in a codebook.
build.cb = class::olvq1(train[1:4], train$eyeDetection, cb)
# classify test set from LVQ Codebook for test data
predict = class::lvqtest(build.cb, test[1:4])
# confusion matrix.
caret::confusionMatrix(test$eyeDetection, predict)
install.packages("randomForest")
library(randomForest)
rf_model <- randomForest(eyeDetection ~ ., data=train, ntree=500, mtry=3, importance = TRUE)
print(rf_model)
predictions <- predict(rf_model, test)
cm <- confusionMatrix(predictions, test$eyeDetection)
cm






















