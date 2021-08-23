setwd("C:/Users/solco/OneDrive/TFE/FINAL PROJECT WORK")


#### Libraries required
library(plyr) #for data manipulation
library(lme4) #for mixed models
library(reshape2) #for generating Table S2
library(piecewiseSEM) # for conditional rsquared for mixed models
library(boot) #for logit and invlogit transformations
library(MASS)
library(effects)
####Question 1 - Does distance to edge modulates the sensitivity to habitat loss across all species?

## load in data
randf <- read.csv("Hansen_Range_Edge_Forest_Cover_Incidence.csv")
inonlyrandf <- read.csv("In_Species_Only_Full_Table.csv")

## ratio of present to absent
variable<-factor(posrandf$Incidence,c(0,1),labels=c("Absent", "Present"))
variable <- table(variable)
v <- prop.table(variable)
v <- round(prop.table(v), digits=2)
v

length(unique(randf$Species))

# Function to test for overdispersion

overdisp_fun <- function(model) {
  ## number of variance parameters in 
  ##   an n-by-n variance-covariance matrix
  vpars <- function(m) {
    nrow(m)*(nrow(m)+1)/2
  }
  model.df <- sum(sapply(VarCorr(model),vpars))+length(fixef(model))
  rdf <- nrow(model.frame(model))-model.df
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}

# ME model to test question about distance to edge and the influence of forest cover
summary(randf)
glmer_mod1 <- glmer(Incidence ~ sqrt_dist_edge * forest_cover_prop +
                      (forest_cover_prop | PID) + (sqrt_dist_edge * forest_cover_prop | Species),
                    data=randf, family='binomial',
                    control=glmerControl(optimizer="bobyqa",  optCtrl=list(maxfun=2e5)))

summary(glmer_mod1)
# The approximated dispersion parameter is the ratio of Pearson Chi-square to residual d.f. Should be close 
#  to 1, less than around 2. In this case 0.82, so all good
overdisp_fun(glmer_mod1)
summary(glmer_mod1)


## plotting model?
library(effects)
plot(allEffects(glmer_mod1))


## remove species causing damage
subrandf <- subset(randf, Species!="sturnira.lilium")
subrandf <- subset(subrandf, Species!="tonatia.bidens")
subrandf <- subset(subrandf, Species!="vampyressa.pusilla")
subrandf <- subset(subrandf, Species!="vampyressa.brocki")
subrandf <- subset(subrandf, Species!="neacomys.guianae")
subrandf <- subset(subrandf, Species!="anoura.geoffroyi")
subrandf <- subset(subrandf, Species!="micronycteris.microtis")

length(unique(subrandf$Species))
#### species causing skew / messing around to find rogue values
between <- randf[randf$Edge_Dist_in_km > -500 & randf$Edge_Dist_in_km < 0,]
unique(between$Species)  

#### without species skewing range edge distance, run model again using subrandf data 
glmer_mod2 <- glmer(Incidence ~ sqrt_dist_edge * forest_cover_prop +
                      (forest_cover_prop | PID) + (sqrt_dist_edge * forest_cover_prop | Species),
                    data=subrandf, family='binomial',
                    control=glmerControl(optimizer="bobyqa",  optCtrl=list(maxfun=2e5)))

plot(allEffects(glmer_mod2))
summary(glmer_mod2)
overdisp_fun(glmer_mod2)
hist(subrandf$Edge_Dist_in_km)

## remove negative range edge measurements? ## 300 is cutoff chosen, rogue measurements after this
posrandf <- randf[!randf$Edge_Dist_in_km<(-300),]

#### without negative range edge distance, run model again using posrandf data 
glmer_mod3 <- glmer(Incidence ~ sqrt_dist_edge * forest_cover_prop +
                      (forest_cover_prop | PID) + (sqrt_dist_edge * forest_cover_prop | Species),
                    data=posrandf, family='binomial',
                    control=glmerControl(optimizer="bobyqa",  optCtrl=list(maxfun=2e5)))

plot(allEffects(glmer_mod3))
summary(glmer_mod3)
overdisp_fun(glmer_mod3)


## try with even more cutoff
chunkrandf <- randf[!randf$Edge_Dist_in_km<(0),]
length(unique(chunkrandf$Species))

glmer_mod4 <- glmer(Incidence ~ sqrt_dist_edge * forest_cover_prop +
                      (forest_cover_prop | PID) + (sqrt_dist_edge * forest_cover_prop | Species),
                    data=chunkrandf, family='binomial',
                    control=glmerControl(optimizer="bobyqa",  optCtrl=list(maxfun=2e5)))

plot(allEffects(glmer_mod4))
summary(glmer_mod4)


# Calculate the distance threshold where all forest covers have equal probability
# Given y = a + bx + cz + dxz, solve dy/dx = 0, which is b + dz = 0 
# which makes sense: find how many times d equals the slope b. Hence z = b /-d

params <- fixef(glmer_mod1)
th <- params['forest_cover_prop'] / - params['sqrt_dist_edge:forest_cover_prop']

# bootstrap some confidence intervals on the threshold
sigma <- vcov(glmer_mod1)
sim <- mvrnorm(1000000, mu=params, Sigma=sigma)

# calculate the threshold
th <- sim[,'forest_cover_prop'] / - sim[,'sqrt_dist_edge:forest_cover_prop']
quantile(th, c(0.025, 0.975)) ^ 2
summary(th)


### for glmer_mod2
params2 <- fixef(glmer_mod2)
th2 <- params2['forest_cover_prop'] / - params2['sqrt_dist_edge:forest_cover_prop']

# bootstrap some confidence intervals on the threshold
sigma2 <- vcov(glmer_mod2)
sim2 <- mvrnorm(1000000, mu=params2, Sigma=sigma2)

# calculate the threshold
th2 <- sim2[,'forest_cover_prop'] / - sim2[,'sqrt_dist_edge:forest_cover_prop']
quantile(th2, c(0.025, 0.975)) ^ 2
summary(th2)


### for glmer_mod3
params3 <- fixef(glmer_mod3)
th3 <- params3['forest_cover_prop'] / - params3['sqrt_dist_edge:forest_cover_prop']

# bootstrap some confidence intervals on the threshold
sigma3 <- vcov(glmer_mod3)
sim3 <- mvrnorm(1000000, mu=params3, Sigma=sigma3)

# calculate the threshold
th3 <- sim3[,'forest_cover_prop'] / - sim3[,'sqrt_dist_edge:forest_cover_prop']
quantile(th3, c(0.025, 0.975)) ^ 2
summary(th3)


### for glmer_mod4
params4 <- fixef(glmer_mod4)
th4 <- params4['forest_cover_prop'] / - params4['sqrt_dist_edge:forest_cover_prop']

# bootstrap some confidence intervals on the threshold
sigma4 <- vcov(glmer_mod4)
sim4 <- mvrnorm(1000000, mu=params4, Sigma=sigma4)

# calculate the threshold
th4 <- sim4[,'forest_cover_prop'] / - sim4[,'sqrt_dist_edge:forest_cover_prop']
quantile(th4, c(0.025, 0.975)) ^ 2
summary(th4)


## save models 1 and 2 and 3 and 4
setwd("C:/Users/solco/OneDrive/TFE/FINAL PROJECT WORK/Code/Modelling")
save(glmer_mod1, th, glmer_mod2, th2, glmer_mod3, th3, glmer_mod4, th4, file='Hansen_glmer_models.Rdata')
params_out <- t(t(c(params, threshold=th)))
write.table(data.frame(params_out), 'glmer_params.csv', col.names=FALSE, quote=FALSE, sep=',')


########## INTRASPECIFIC MODELS ISH?????? ##########

### use n_occur_repeated3 from similar_species.R
allbats <- read.csv("bat_species_in_all_studies.csv")

### change variable names to match other data sets
allbats$dfl <- gsub(" ", "\\.", allbats$dfl)
allbats$dfl <- tolower(allbats$dfl)

intrasp <- randf[randf$Species %in% allbats$dfl,]
intrasp <- na.omit(intrasp)

unique(intrasp$Species)
unique(intrasp$PID)


# Create a dataframe containing the average proportion of forest cover that a species is found in each study
# This is the weighted averages method #See Gauch 1982 - Multivariate analysis in community ecology. 
# and the  average distance to range edge per species per study
intradata <- aggregate(cbind(forest_cover_prop, sqrt_dist_edge) ~ PID + Species , data=intrasp, mean)

### Uroderma bilobatum now only appears in two studies due to removal of most negatvie distances, so remove
intradata <- intradata[intradata$Species!="uroderma.bilobatum",]
### Remove Sturnira lilium as well as insanely negative (range is just wrong)
intradata <- intradata[intradata$Species!="sturnira.lilium",]


#Test the overall effect of distance to edge on intraspecific sensitivity 
# 0) null model, while allowing each species a different intercept
# 1) distance model, while allowing each species a different intercept
# 2) distance model, while allowing each species a different intercept and to have a different response to distance to edge
# 3) distance model, while allowing each species a different intercept, to have a different response to distance to edge, and to control for variation of forest cover acorss studies
# 4) as 3 but with no random effects
model0 <- lmer(logit(forest_cover_prop) ~ 1 + (1|Species), data=intradata)
model1 <- lmer(logit(forest_cover_prop) ~ sqrt_dist_edge + (1|Species), data=intradata)
model2 <- lmer(logit(forest_cover_prop) ~ sqrt_dist_edge + (sqrt_dist_edge|Species), data=intradata)
model3 <- lmer(logit(forest_cover_prop) ~ sqrt_dist_edge + (1|Species) + (1|PID), data=intradata)


summary(model0)
summary(model1)
summary(model2)
summary(model3)
AIC(model0, model1, model2, model3)
### null model is best fit, AIC value is lowest by >2


#Run a linear regression using as response "average forest cover proportion" and 
# explanatory variable "distance to range edge" for each species.
edge.results <- lmList(logit(forest_cover_prop) ~ sqrt_dist_edge | Species, data=intradata)



unique(intradata$Species)
# demo binomial models for selected species and sites
sp <- c("artibeus.lituratus","carollia.perspicillata","chiroderma.villosum","desmodus.rotundus","glossophaga.soricina","lophostoma.brasiliense","mimon.crenulatum","platyrrhinus.helleri","trachops.cirrhosus")
studies <- c("PID0058", "PID0093", "PID0143", "PID0144")

#Perform a binomial glm for the specific species in specific studies in vectors above
list.sp <- list()
for(i in 1:9){
  list.sp[[i]] <- glm(Incidence ~ forest_cover_prop, data=randf, 
                       subset=Species == sp[i], family="binomial")
}

summary(list.sp[[9]])



commonx <- c("artibeus.lituratus","carollia.perspicillata","chiroderma.villosum","desmodus.rotundus","glossophaga.soricina","lophostoma.brasiliense","mimon.crenulatum","platyrrhinus.helleri","trachops.cirrhosus")

### save models and character lists
setwd("C:/Users/solco/OneDrive/TFE/FINAL PROJECT WORK/Code/Modelling")
save(edge.results, list.sp, sp, studies, commonx, intradata, file='intraspecific_models.Rdata')

### save all data points as well ######
write.csv(intrasp, "Nonaggregated_data_intraspecific_species.csv", row.names = F)
##### make graph with these points as much larger dataset to work with?

