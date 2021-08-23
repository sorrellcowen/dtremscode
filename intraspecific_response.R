setwd("C:/Users/solco/OneDrive/TFE/FINAL PROJECT WORK")


#### Libraries required
library(plyr) #for data manipulation
library(lme4) #for mixed models
library(reshape2) #for generating Table S2
library(piecewiseSEM) # for conditional rsquared for mixed models
library(boot) #for logit and invlogit transformations
library(MASS)
library(effects)
library(remotes)

## load in data
randf <- read.csv("Hansen_Range_Edge_Forest_Cover_Incidence.csv")
inonlyrandf <- read.csv("In_Species_Only_Full_Table.csv")

## Question 1 - Is intraspecific variation in sensitivity explained by the distance to range edge?

# Create a dataset containing only sites where species are present
data1 <- subset(randf, Incidence==1)

# Create a dataframe containing the average proportion of forest cover that a species is found in each study
# This is the weighted averages method #See Gauch 1982 - Multivariate analysis in community ecology. 
# and the  average distance to range edge per species per study
alldata <- aggregate(cbind(forest_cover_prop, sqrt_dist_edge) ~ PID + Species , data=data1, mean)

#Create a dataframe containing only the species that occur in all studies
counts <- table(alldata$Species)
common5x <- names(counts)[counts == 5]
data5x <- subset(alldata, Species %in% common5x)
incdata <- subset(randf, Species %in% common5x)

unique(data5x$Species)

data5x <- data5x[data5x$Species!="sturnira.lilium",]
## data5x[data5x == 0] <- 0.01

#Test the overall effect of distance to edge on intraspecific sensitivity 
# 0) null model, while allowing each species a different intercept
# 1) distance model, while allowing each species a different intercept
# 2) distance model, while allowing each species a different intercept and to have a different response to distance to edge
# 3) distance model, while allowing each species a different intercept, to have a different response to distance to edge, and to control for variation of forest cover acorss studies
# 4) as 3 but with no random effects
model0 <- lmer(logit(forest_cover_prop) ~ 1 + (1|Species), data=data5x)
model1 <- lmer(logit(forest_cover_prop) ~ sqrt_dist_edge + (1|Species), data=data5x)
model2 <- lmer(logit(forest_cover_prop) ~ sqrt_dist_edge + (sqrt_dist_edge|Species), data=data5x)
model3 <- lmer(logit(forest_cover_prop) ~ sqrt_dist_edge + (1|Species) + (1|PID), data=data5x)

summary(model0)
summary(model1)
summary(model2)
summary(model3)
AIC(model0, model1, model2, model3)

# Model 3 is best: no real associated with RE but doesn't affect parameter estimates
model4 <- lmer(logit(forest_cover_prop) ~ sqrt_dist_edge + (1|PID), data=data5x)
AIC(model3, model4)
# Model 4 not statistically better fit, AIC only 1.2 lower, so use Model 3 or 4

rsquared(model3)
rsquared(model4)

summary(model4)


#Model 4 reported in Main Text. Lowest AIC but results from model 1 and 2 and 3 are very similar.
# Model 2 has most degrees of freedom

#Run a linear regression using as response "average forest cover proportion" and 
# explanatory variable "distance to range edge" for each Species.
edge.results <- lmList(logit(forest_cover_prop) ~ sqrt_dist_edge | Species, data=data5x)
summary(edge.results)


unique(data5x$Species)
# demo binomial models for selected Species and sites
sp <- rep(c("artibeus.lituratus","carollia.perspicillata","chiroderma.villosum","desmodus.rotundus","glossophaga.soricina"), each=2)
studies <- rep(c("PID0144","PID0143"), times=5)
sp
studies

#Perform a binomial glm for the specific Species in specific studies in vectors above
list.5sp <- list()
for(i in 1:10){
  list.5sp[[i]] <- glm(Incidence ~ forest_cover_prop, data=randf, 
                       subset=Species == sp[i] & PID == studies[i], family=binomial)
}

setwd("C:/Users/solco/OneDrive/TFE/FINAL PROJECT WORK/Code/Modelling")
save(edge.results, list.5sp, sp, studies, common5x, alldata, incdata, file='hansen_intraspecific_models.Rdata')


list.5sp[[1]]
list.5sp[[5]]


