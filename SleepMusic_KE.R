# Code to process features data from Spotify, create a model to predict sleep-inducing propensity using a 
# subset of labelled tracks and apply the model to a larger data set of Spotify tracks and create a playlist.

library(tidyverse)
library(reshape2)
library(RColorBrewer)


# Load data ---------------------------------------------------------------

# Set wd.
# setwd()

# Load sample Spotify features data set (N = 16,814 tracks) and the labelled data.
feats <- read.csv('Spotify_feats.csv')
ratings <- read.csv('ratings_data.csv')


# Inspect data ------------------------------------------------------------

# Visually inspect the distribution of the features data.
feats_long <- reshape2::melt(feats[,c(6:30)]) # Long form to assist plotting. 

ggplot(feats_long, aes(x=value)) +
  geom_histogram() +
  facet_wrap(~ variable, scales = 'free') +
  theme_classic()

# Inspect correlations matrix. 
feats_cormat <- round(cor(feats[,c(6:30)], use = 'complete.obs', method = "spearman"), 3)
feats_cormat[abs(feats_cormat) < 0.3] <- NA # for easier inspection. 


# PCA ---------------------------------------------------------------------

# Use PCA to reduce features into underlying components to assist the modelling, carrying over only the 
# continuous data that passed our correlations assessment.
pca_data <- feats[,c(6:8,10,12:13,15:17,19:30)]

# Run PCA.
pca <- prcomp(pca_data, center = TRUE, scale = TRUE)
summary(pca)

# Retaining six components with Eigenvalues > 1 - calculate Varimax rotation.
pca_rotation <- varimax(pca$rotation[,1:6])
pca_rotation[1]

# Gather component scores for the first 6 components and add these to our data.
pca_scores <- pca$x[,1:6]
feats <- cbind(feats, pca_scores)


# Run regression models and make predictions ------------------------------

# Pull out the PCA scores for the labelled tracks and add them to the ratings data. 
mdl_data <- merge(ratings, feats, by = c('artist','album','track_name', 'track_id'))

# Create model.
mdl <- lm(Sleep.Preventing ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6, data = mdl_data)
summary(mdl)

# Predict SI ratings for the greater Spotify data set.
mdl_preds <- as.data.frame(predict(mdl, newdata = feats, interval = 'confidence'))

# Add predictions to the features data set.
feats <- cbind(feats, mdl_preds)

# Calculate the confidence interval difference to create another metric with which we can assess the outcomes.
feats$int <- feats$fit - feats$lwr


# Explore outcomes --------------------------------------------------------

# Take a look at the model performance against the real ratings in the labelled data set.
mdl_test <- merge(ratings, feats, by = c('artist','album','track_name', 'track_id'))

ggplot(mdl_test, aes(x=Sleep.Preventing, y=fit)) +
  geom_point() +
  stat_smooth(method=lm, se=TRUE, fullrange=TRUE, show.legend = FALSE) +
  stat_cor(show.legend = FALSE) +
  geom_errorbar(aes(ymin=lwr,ymax=upr, width=.2), alpha = .4) +
  labs(x = 'True', 
       y = 'Predicted') +
  theme_classic()


# Take a look at how the predictions compare between playlist categories to see if they line up as expected.
# Create a pairs list to run pairwise statistical tests.
pairs <- list( c("Energising","Relax"), c("Relax","Sleep"), c("Energising","Sleep"))

ggplot(feats, aes(x = Category, y = fit)) + 
  stat_boxplot(geom ='errorbar', width = 0.08, aes(group = Category)) + 
  geom_boxplot(width = 0.08, show.legend = F, notch = FALSE, outlier.shape = NA) +
  stat_halfeye(aes(thickness = after_stat(pdf*n)), justification = -0.08, .width = 0, point_colour = NA) +
  stat_compare_means(comparisons = pairs, aes(label = ..p.signif..), method = 'wilcox.test') +
  labs(x = 'Category', y = 'Sleep Prediction') +
  theme_classic() 


# Plot the fit against the error interval.
ggplot(feats, aes(int, fit)) +
  geom_point(size = 0.5) +
  theme_classic() 


# Sampling the data set ---------------------------------------------------

# We can use the predicted values to sample sleep inducing tracks. We can also include a threshold for the int
# values, selecting only tracks for which we have a greater confidence in our prediction.

# This will randomly sample ten tracks with a sleep inducing prediction greater than 2 and a confidence 
# interval less than 1. 
sample <- feats[sample(which(feats$fit < -2 & feats$int < 1), 10),]


