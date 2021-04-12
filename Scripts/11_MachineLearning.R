#######################################
##AEDA Machine Learning assignment  ##
##Based on scripts from Lucas 2020 ##
####################################

## If you don't have these loaded yet, here are the libraries you will need
## Load general data manipulation / plotting libraries
library(dplyr)
library(ggplot2)

# Load modeling libraries
library(caret)
library(ranger)
library(pdp)
library(traitdata)
library(kernlab)

## Load helper scripts from Lucas 2020 - these are on the Github site as ML_helpers.R
## You can download and then source from this file (change file path to the location on your computer)
source('ML_helpers.R')

set.seed(100)

#caret_Setup

folds <- createFolds(p$y, k = 5, returnTrain = TRUE)
trcntrl <- trainControl(index = folds, savePredictions = TRUE, search = 'random')

## Now let's use similar methods to those we used in the exercise to evaluate covariates of litter/clutch size in reptiles!
## Load a larger dataset for amniotes

data(amniota)

amniote_data<-amniota

names(amniote_data)
dim(amniote_data)

sum(!is.na(amniote_data$litter_or_clutch_size_n))

#The trait names should be pretty self-explanatory. "svl" = snout-vent length 

#Q1: Write some code to clean the data.
#Rename the variable of interest to "y", log transform variable of interest and 
#remove any taxa with missing litter/clutch size data.
#Then, retain only taxa in the class Reptilia and remove any variables with no data (all NA).

reptilia <- amniote_data %>% 
    filter(!is.na(litter_or_clutch_size_n)) %>% 
    mutate(y = log1p(litter_or_clutch_size_n)) %>%
    filter(Class=='Reptilia') %>%
    select(where(~!all(is.na(.x))))

summary(reptilia)

##Q2: Plot the distribution of log-transformed litter/clutch size in reptiles.
##Histogram or boxplot (or both if you want) are fine.
##Visualizing by order may be useful.

ggplot(reptilia, aes(y)) + geom_histogram()

ggplot(reptilia, aes(x = Order, y = y)) + geom_boxplot()


##Q3: Write a little more data-cleaning code!
##Impute missing data and remove taxonomic data, common name, and scientific name.

preprocesses <- preProcess(reptilia, method = 'medianImpute')
reptilia_impute <- predict(preprocesses, reptilia)

cols=c(7:30, 32)

reptilia_impute_data=reptilia_impute[,cols]

dim(reptilia_impute_data)
names(reptilia_impute_data)

##Q4: Visualize the distributions for the predictor variables.
##Identify which variables look like they have a highly non-normal distribution.
##Log-transform these variables and visualize again.
##Which of the four models we will fit need the input variables to be log-transformed?

par(mfrow = c(2, 2))

for(i in 0:25){
    for( j in 1:4){
        
        if(j + 4 * i <= ncol(reptilia_impute_data)){
            hist(reptilia_impute_data[, j + 4 * i], breaks = 100, ylim = c(0, 80), main = j + 4 * i)
        }
        
    }
    print(i)
    par(mfrow = c(2, 2))
}

#Most of the predictor variables have non-normal distributions
log_cols <- c(1, 2, 3, 4, 6, 7, 8, 9, 10, 12, 13, 14, 15, 17, 18, 20, 21, 22, 23)

reptilia_impute_data[, log_cols] <- log1p(reptilia_impute_data[, log_cols])

par(mfrow = c(2, 2))

for(i in 0:25){
    for( j in 1:4){
        
        if(j + 4 * i <= ncol(reptilia_impute_data)){
            hist(reptilia_impute_data[, j + 4 * i], breaks = 100, ylim = c(0, 80), main = j + 4 * i)
        }
        
    }
    print(i)
    par(mfrow = c(2, 2))
}

#The parametric models require the log transformation of our predictor variables. For us this
# is the linear model and the elastic net model.

##Q5: Fit a linear model relating your response variable to some potential predictors.
##To make this similar to our model for mammals, use adult body mass, age to maturity for females, incubation length, 
#litters/clutches per year, and maximum longevity.
##Visualize model fit and get R2.
##How does this model compare to the mammal model?

reptilia_apriori_formula <- y ~ adult_body_mass_g + female_maturity_d + incubation_d + 
     litters_or_clutches_per_y + maximum_longevity_y
reptilia_lm <- train(reptilia_apriori_formula, data = reptilia_impute_data, method = 'lm', trControl = trcntrl, na.action = na.omit)

plotCV(reptilia_lm)

reptilia_lm

summary(reptilia_lm$finalModel)

#R-squared is 0.31, which is very similar to the mammal linear model's R-squared of 0.33. The linear model does not appear to be 
#the best model for predicting litter/clutch size in reptiles or mammals.

##Q6: Fit an elastic net to the data. Use the same hyperparameters used for the mammal dataset.
##Visualize model fit and get maximum R2.
##Plot R2 vs lasso/ridge fraction and strength of regularization (lambda).
##Does using the elastic net improve prediction relative to the linear model for this dataset?

enet_gr <- expand.grid(lambda = 10 ^ seq(0, -4, length.out = 20), fraction = c(seq(0.01, 1, length.out = 25)))
reptilia_enet <- train(y ~ ., data = reptilia_impute_data, method = 'enet', tuneGrid = enet_gr, trControl = trcntrl, na.action = na.omit)

#Cross-validation: plot observed vs predicted
plotCV(reptilia_enet)

#elastic_net_summary - best model fit
reptilia_enet$results$Rsquared %>% max
#Max R-squared is 0.97

# Plot R2 vs regularization strength (recreate figure 1a)
#1 is more Lasso, more Lasso than ridge means it's applying a stronger penalty (not weighting the coefficient as high)

reptilia_enet$results %>%
    ggplot(aes(fraction, Rsquared, colour = lambda, group = factor(lambda))) +
    geom_line() +
    geom_point() + scale_color_viridis_c(trans = 'log10') + xlab('Lasso/Ridge fraction')

#Yes, the elastic net improves prediction relative to the linear model, as the R-squared value is higher for the elastic net.

##Q7: Fit a Gaussian process model to the data. Use the same range of sigma values used for the mammal dataset. 
##Visualize model fit and get R2.
##Plot R2 vs sigma. How does this plot compare to the plot from the mammal dataset?
##Overall, does the Gaussian process model perform better than the linear model?

gp_gr <- data.frame(sigma = c(0.01, 0.02, 0.04, 0.08, 0.16))
reptilia_gp <- train(y ~ ., data = reptilia_impute_data, method = 'gaussprRadial', tuneGrid = gp_gr, trControl = trcntrl, na.action = na.omit)

## Plot R2 vs sigma (recreate figure 1b)

reptilia_gp$results %>% ggplot(aes(sigma, Rsquared)) +
    geom_line() + geom_point() + xlab('Sigma')

#The reptile R2 vs. sigma plot is slightly different than the mammal plot. The reptile plot starts at a higher R2 value and has 
# a faster decline/steeper slope than the mammal plot does. 

#Cross-validation: plot observed vs predicted
plotCV(reptilia_gp)

#gaussian process summary
reptilia_gp
reptilia_gp$results$Rsquared %>% max

#The max R-squared is 0.82, which shows the Gaussian process model performs better than the linear model.

##Q7: Train a random forest on the data. Note - use a lower maximum number of random predictors by setting mtry = c(2, 5, 10, 20).
##Visualize model fit and get R2.
##Plot R2 vs node size and number of random predictors.
##What does the node size selected indicate about the amount of noise in the model?
##What does the number of random predictors selected indicate about interaction depth?

rf_gr_reptilia <- expand.grid(mtry = c(2, 5, 10, 20), splitrule = 'variance', min.node.size = c(5, 10, 20, 50))
reptilia_rf <- train(y ~ ., data = reptilia_impute_data, method = 'ranger', tuneGrid = rf_gr_reptilia, 
                     trControl = trcntrl, na.action = na.omit, importance = 'impurity', num.trees = 1000)

##Plot # of random predictors and minimum node size vs R2 (recreate figure 1c)

reptilia_rf$results %>%
    ggplot(aes(mtry, Rsquared, colour = factor(min.node.size), group = factor(min.node.size))) +
    geom_line() +
    geom_point() +
    labs(colour = 'min.node.size')

#Cross-validation: plot observed vs predicted
plotCV(reptilia_rf)

# Random forest summary
reptilia_rf
reptilia_rf$results$Rsquared %>% max

#The max R-squared for the random forests model is 0.98.
#A node size of 5 yielded the highest R-squared value. This indicates there is not too much noise in the model.
#20 random predictors yielded the highest R-squared value. As this is the highest number of predictors specified in 
#creating the model, I assume there is high interaction depth.


##Q8: Overall, which model(s) perform best at predicting litter/clutch size, and which perform the worst?
##Compare this to the mammal analysis. What does this say about the universality of these methods?

compare_models(reptilia_gp, reptilia_rf)
compare_models(reptilia_enet, reptilia_rf)
compare_models(reptilia_lm, reptilia_rf)

#The random forest model performs the best, and it is closely followed by the elastic net model. The R-squared for the random
#forest and elastic net model were both very high and only differed by about 0.01. The linear model was by far the worst predictor.
#The Gaussian process model performed okay, but was not the best option. The overall results of the random forest model being the best
#predictor, and the linear model being the worst predictor followed the mammal dataset. However, the gaussian process model performed
#better than the elastic net model in the mammal dataset, showing the methods are not completely universal across datasets.

##Q9: Evaluate variable importance for the elastic net, gaussian process, and random forest.
##Which variable is most important across models? 

varImp(reptilia_enet)
varImp(reptilia_gp)
varImp(reptilia_rf)

#Adult body mass is important across all three models. No sex svl is most important for the random forest model, but not as important
#in the elastic net and gaussian process models.

##Q10: Plot functional forms for the relationship between litter/clutch size and the most important variable for the 
#Gaussian Process and the random forest models.
##How do they differ?
##What does this say about the likely relation ship between litter/clutch size and the best predictor variable?

# partial dependence plot for adult body mass (gaussian process most important variable)
partial(reptilia_enet,pred.var = c('adult_body_mass_g'), plot = TRUE)
partial(reptilia_gp, pred.var = c('adult_body_mass_g'), plot = TRUE)
partial(reptilia_rf, pred.var = c('adult_body_mass_g'), plot = TRUE)

# partial dependence plot for no sex svl (random forest most important variable)
partial(reptilia_enet,pred.var = c('no_sex_svl_cm'), plot = TRUE)
partial(reptilia_gp, pred.var = c('no_sex_svl_cm'), plot = TRUE)
partial(reptilia_rf, pred.var = c('no_sex_svl_cm'), plot = TRUE)

#Snout-vent length shows a positive relationship with the litter/clutch size in the random forest model when the length is 1.2 cm or above. 
#Adult body mass shows a positive relationship with the litter/clutch size in the random forest model until body mass is above 2.0 g. 
#In the gaussian process model, adult body mass shows an overall positive trend with litter/clutch size as body mass increases. 
#Overall, it seems like higher snout vent length and adult body mass are positively correlated with litter/clutch size, 
#but the random forest model is able to pick out more of the intricacies (such as predictor values that lead to lower litter/clutch sizes
#than predicted by a linear relationship).

