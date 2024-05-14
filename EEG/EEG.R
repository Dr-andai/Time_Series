## EEG Sleep State

library("rio")
library("rstatix")
library("tidyverse")
library("dplyr")

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
# dropping the "eye detection" column
eeg_variables <- eeg[, -15]

# STATS
eeg_stats <- get_summary_stats(eeg_variables)

## -visualize on the summary stats
# visualize max col
eeg_stats %>% 
  ggplot(aes(variable,max))+
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

explained_variance_ratio <- summary(eeg_prcomp)

eeg_prcomp$label <- eeg$eyeDetection

plot(eeg_prcomp$PC1,
     eeg_prcomp$AF3,
     pch = 19,
     main="Figure 1: 2D Scatterplot",
     xlab="PC1",
     ylab="AF3")




library("PerformanceAnalytics")
chart.Correlation(corr_eeg, histogram=TRUE, pch=19)
# In the above plot:
#   
# The distribution of each variable is shown on the diagonal.
# On the bottom of the diagonal : the bivariate scatter plots with a fitted line are displayed
# On the top of the diagonal : the value of the correlation plus the significance level as stars

















