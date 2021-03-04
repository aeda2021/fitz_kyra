#AEDA Midterm- Kyra Fitz

### QUESTION 1
# use the datasets 'pinelands_bees.csv' and 'land_cover.csv' to answer the question:
# Does natural land cover in the surrounding landscape predict the number of individual bees collected at a site?
# follow the analysis workflow that we learned in class, for which there are prompts below
# make sure to complete all 6 prompts
# use comment lines to briefly explain your reasoning at each step (ie, why you are doing what you are doing)
# you will turn in your code (ie, this R file) to your github repo at the end of class

## brief metadata
# the datasets you will use are 'pinelands_bees.csv'  and 'land_cover.csv'
# these are data I collected on wild bees of 117 ish species (ish because the taxonomy of some specimens is not fully sorted out) 
# data were collected at 27 study sites in the New Jersey pinelands
# all sites were in forest habitat, but sites differed in how much of the surrounding landscape was forested
# bees were collected on four days at each site between April and September 2003, with collection events organized into rounds, 
#such that each site was sampled once in round 1 before round 2 was begun, etc
# 'pinelands_bees.csv'contains data on the bees collected, where each row is one individual bee
# 'land_cover.csv' column 'Nat1600' is the percentage of the natural land cover surrounding the site within a 1600m radius (roughly an 800 ha area), 
#where the natural habitat is predominantly forest, with a small amount of other natural habitats such as open wetlands


library(tidyverse)

##  1 Get and format the data
# you will need to create a new dataframe that has the variables you need: site, land cover, number of bees collected
# you may want to use group_by, summarize, n(), left_join

#Read in the data
bees <- read_csv("pinelands_bees.csv")
landcover <- read_csv("land_cover.csv")


#Need a dataframe with site, land cover, and number of bees collected at each site

#Start by calculating the number of bees collected at each site
bees_site <- group_by(bees_landcover, site_name)
bees_df <- data.frame(table(bees_site$site_name))  #This has the number of bees found at each site in Freq column
names(bees_df)[1] <- "site_name"  #rename column header
names(bees_df)[2] <- "NBees"     #rename column header

#Join landcover to this data frame
bees_data <- left_join(bees_df, landcover, key=site_name)
#Join looks good; we now have the site name in column "site_name", the number of bees at each site in column
#"NBees" and the landcover value in "Nat1600"

## 2 Data picture
# plot the data and figure out what type of model might be best

ggplot(data=bees_data, aes(x=Nat1600, y=NBees)) +
    geom_point()

#Nat 1600 is a bounded variable, so a Poisson regression using GLM may be appropriate here

## 3 Test model assumptions
# you can test the assumptions of more than one type of model here if you aren't sure
# explain your reasoning for the model you choose

#Trying glm with poisson family
bees_poismod <- glm(NBees ~ Nat1600, data = bees_data, family = poisson)

#Look at diagnostic plots to check assumptions
plot(bees_poismod)
#Fitted values vs. residuals looks okay and is fairly equally distributed, but 2 points have high residuals
#Normal QQ plot looks okay except for the same 2 points with high residuals
#Scale location plot shows residuals fairly equally distributed, so seems okay
#Residuals vs. leverage plot shows most plots have low leverage

#Will compare this to a simple linear model
bees_simplemod <- lm(NBees ~ Nat1600, data=bees_data)
plot(bees_simplemod)
#Fitted values vs. residuals looks okay and is fairly equally distributed, but 2 points have high residuals
#Normal QQ plot looks okay except for the same 2 points with high residuals
#Scale location plot shows residuals fairly equally distributed, so seems okay
#Residuals vs. leverage plot shows most plots have low leverage
#Diagnostic plots look fairly similar to what I got for the poisson GLM, still seeing 2 points with high residuals

#As diagnostic plots did not look terribly different between the poisson GLM and the simple linear model, I will compare
#the AIC from each model

AIC(bees_poismod) #456.38
AIC(bees_simplemod) #261.59

#The simple linear model has a far lower AIC. Based on the lower AIC value, and the assumptions checking out for the 
#simple linear model, I choose a simple linear model as the best model for this data.


## 4 Report and interpret results
# make sure you interpret the following things: coefficients, p value, explanatory value of model
# state your conclusions using numbers and units
# discuss the magnitude and biological significance of your result, not just its significance
# how confident are you in this analysis and why

summary(bees_simplemod)

#Report of results:
#The coefficient for landcover (Nat1600) is estimated to be -0.4354. The coefficient for the intercept is estimated to 
#be 72.5977. The p-value is 0.0297 with 26 degrees of freedom. The multiple R-squared value is 0.1699, and the adjusted 
#R-squared value is 0.138. The linear model equation estimated is y = -0.4354x + 72.5977 where y is the number of bees 
# and x is the Nat1600 landcover.

#Interpretation and conclusion:
#This model tells us that there is a negative relationship between the number of bees found at a site and the Nat1600 landcover
#index. As the percentage of natural landcover increases, the number of bees found at a site decreases by a factor of -0.4354.
#An adjusted R-squared value of 0.138 indicates that this is weak linear relationship, and the p-value of 0.0297 indicates the 
#linear relationship is statistically significant. With such a weak negative linear relationship, I am not confident in landcover being
#a good predictor for the number of bees that will be at a site. There were two outliers in the dataset (sites ButtersworthBogsRd
#and OlsenSchool) that had over 100 bees, while the rest of the sites had less than 80 bees. It may be worth looking at additional 
#factors other than landcover to see what may be influencing the high bee numbers at these two sites and the rest of the sites overall.


## 5 Plot model results back onto the data picture
# geom_smooth is the easy way to do this, but you can alternatively do it manually using the model output (coefficients) if you want

augmented_bees_simplemod <- augment(bees_simplemod, interval = 'confidence') 
glimpse(augmented_bees_simplemod)

ggplot(augmented_bees_simplemod, aes(x = Nat1600, y = NBees)) + 
    geom_point() + 
    geom_line(aes(y = .fitted)) +  
    geom_ribbon(aes(ymin =.lower, ymax = .upper),alpha=0.3)


## 6  If you were to add a random effect to this model, what would it be, what would adding it accomplish?
# please answer this question in comment lines
# you do not need to format any data here or code or run anything
# explain your reasoning for using this random effect
# you will need to go back to the original data file and metadata to answer this question
# extra credit: write out the model code for your model now including the random effect

#I would add species as a random effect. We are not concerned about the individual species in our overall question here, as we are focusing
#on the number of bees at each site, so species could work well as a random effect. Adding species as a random effect would be useful 
#because it would use less degrees of freedom than adding it as a fixed effect, and it would help control for repeated measures, and
#and any errors correlated among the same species. Adding species as a random effect would incorporate any variation
#we may be missing in our current model without including it, giving us a more accurate idea of what is happening in
#the system without needing to lose too many degrees of freedom (thus reducing statistical power) if we added it as 
#a fixed effect. We do have two outliers in our current dataset, and it's possible that species as a random effect
#could help explain some of this variance.

fspecies <- factor(bees$species)

bees_randomeffectmod <-lme(NBees ~ Nat1600, random= ~ 1|fspecies, data=bees)


### QUESTION 2
# The file "modSel.csv" contains simulated dataset of observations of a focal species at a series of sites.
# For each site, you have observed abundance, and measurements of environmental variables you hypothesize
# to affect the distribution of this species.
# Specifically, you hypothesize the species is more abundant in warmer, wetter regions,
# and that it prefers core over edge habitat.
# You have measured each of these in a couple ways, as mean annual and summer temperature,
# cumulative annual and summer precipitation, and distance to nearest edge and total edge within 500 m.
# Your goal here is to find the best model you can, given your hypotheses,
# to describe the distribution of this species.
# In doing so, you will also assess the more relevant measure of each environmental condition,
# and weigh their relative importance (or at least predictive power).
# For simplicity, do not consider interactions between variables.
# Please give your models interpretable names.


# Step 1. Find the best error structure/error distribution for these data.
# State your conclusion in comment lines
# (Hints: you want to assess model-error distributions, not the data distribution; these are count data.)

modSel <- read_csv("modSel.csv")

#Abundance is count data, so try a poisson GLM first
mod_pois_full <- glm(observedAbundance ~ meanAnnualTemp + meanSummerTemp + annualPrecipitation + summerPrecipitation + 
                        distance2edge + totalEdge, data = modSel, family = poisson)

plot(mod_pois_full)
hist(mod_pois_full$residuals)
#Diagnostic plots look to approximately follow assumptions, but a histogram of residuals show many more residuals between 0 and -1 
#than between 0 and 1 

#Try a logistic GLM (family binomial) next

mod_log_full <- glm(observedAbundance ~ meanAnnualTemp + meanSummerTemp + annualPrecipitation + summerPrecipitation + 
                         distance2edge + totalEdge, data = modSel, family = binomial)

#This doesn't work because our y-values are not between 0 and 1- binomial is suited for binary data or proportions, which 
#we don't have here

#The Poisson GLM seems the most appropriate here. Poisson is frequently used with count data, which makes sense
#given that our y value of observed abundance is count data. I would have liked the histogram of the residuals
#to be a little more equally distributed around 0, but this still seems to be the best option given the data.


# Step 2: Having determined the best error structure, determine the more effective method of measuring each variable.
# For each variable, compare methods as a pair of single-variable models (e.g., summer temp vs annual temp).
# State your conclusion in comment lines

#Summer temp vs. annual temp.

mod_summertemp <- glm(observedAbundance ~ meanSummerTemp, data=modSel, family=poisson)
mod_annualtemp <- glm(observedAbundance ~ meanAnnualTemp, data=modSel, family=poisson)
AIC(mod_summertemp) #293.81
AIC(mod_annualtemp) #297.14

#Mean summer temperature appears to be the better method of measuring temperature, as it has a lower AIC compared
#to mean annual temperature.

#Annual precip vs. summer precip.

mod_summerprecip <- glm(observedAbundance ~ summerPrecipitation, data=modSel, family=poisson)
mod_annualprecip <- glm(observedAbundance ~ annualPrecipitation, data=modSel, family=poisson)
AIC(mod_summerprecip) #351.57
AIC(mod_annualprecip) #355.26

#Summer precipitation appears to be the better method of measuring precipitation, as it has a lower AIC compared
#to annual precipitation.

#Distance to edge vs. total edge

mod_distance2edge <- glm(observedAbundance ~ distance2edge, data=modSel, family=poisson)
mod_totaledge <- glm(observedAbundance ~ totalEdge, data=modSel, family=poisson)
AIC(mod_distance2edge) #355.199
AIC(mod_totaledge) #309.719

#Total edge appears to be the better method of measuring edge habitat, as it has a lower AIC compared
#to distance to edge.

# Step 3: Having determined which method of measurement for each variable is best,
# determine the most effective combination of predictors;
# run a set of competing models and create a table comparing these models to each other and to a null.
# state your conclusion in comment lines

#Naming guidelines- full for model with all 3 variables, first letter of the variable (t for temp, p for 
#precip, e for edge for the other combinations)
#Individual variables mods have already been run above- are mod_summerprecip, mod_totaledge, and mod_summertemp

mod_full <- glm(observedAbundance ~ meanSummerTemp + summerPrecipitation + totalEdge, data = modSel, family = poisson)

mod_tp <- glm(observedAbundance ~ meanSummerTemp + summerPrecipitation, data = modSel, family = poisson)

mod_te <- glm(observedAbundance ~ meanSummerTemp + totalEdge, data = modSel, family = poisson)
    
mod_pe <- glm(observedAbundance ~ summerPrecipitation + totalEdge, data = modSel, family = poisson)

mod_null <- glm(observedAbundance ~ 1, data = modSel, family = poisson)

#Organizing AIC values

AIC <- c(AIC(mod_full), AIC(mod_tp), AIC(mod_te), AIC(mod_pe), AIC(mod_summertemp), AIC(mod_summerprecip),
         AIC(mod_totaledge), AIC(mod_null))

modnames <- c("Temp, Precip and Total Edge", "Temp and Precip", "Temp and Total Edge", "Precip and Total Edge", 
              "Temp", "Precip", "Total Edge", "Null Model")

#Create data table to compare AIC across models
AIC_table <- tibble(modnames, AIC)

#The best model is the model with temperature and total edge (mod_te). This model has the lowest AIC out of all models.
#The full model with temperature, precipitation, and total edge was a close second, an AIC of 2 greater than the 
#temperature and total edge model's AIC. 

# Step 4: Interpret these results.
# Were your hypotheses supported? What is the relative importance of each predictor?
# What is your general conclusion?

summary(mod_te)
#A GLM with temperature and total edge had an intercept estimate of 0.29, a temperature coefficient estimate of 0.53, 
#and a total edge coefficient estimate of -0.54. This indicates that when total edge is held constant, the observed
#abundance will increase by a factor of 0.53 the temperature increases. When temperature is held constant, the observed
#abundance will decrease by a factor of 0.54 as the total edge increases. Overall this supports our hypothesis 
#because abundance is increasing as temperature increases and abundance is decreasing when the amount of edge habitat 
#is decreasing, indicating abundance is higher in core habitat versus the edge of habitats. As the best model 
#does not include precipitation, it does not appear that the precipitation part of our hypothesis is supported. As the 
#coefficients are similar in magnitude for each predictor, it seems temperature and total edge are equally important
#in predicting abundance. 
#My general conclusion is that abundance of this species increases when temperatures are warmer and when total edge
#habitat is lower. 


#Side note- I would have liked to make a more cohesive naming scheme for my models but was running out of time.

