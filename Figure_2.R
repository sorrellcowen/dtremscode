library(hexbin)
library(grid)
library(gridBase)
library(fields) # for tim.colors
library(mgcv)
library(lme4)
library(ggplot2)
library(tidyverse) #for all data wrangling
library(cowplot) #for manuscript ready figures
library(sjPlot) #for plotting lmer and glmer mods
library(sjmisc) 
library(effects)
library(sjstats) #use for r2 functions
library(gridExtra)
library(piecewiseSEM)

# plotting data
setwd("C:/Users/solco/OneDrive/TFE/FINAL PROJECT WORK/Code/Modelling")
load("Hansen_glmer_models.RData")

## plotting model
plot(glmer_mod3)
qqnorm(resid(glmer_mod3))
qqline(resid(glmer_mod3))

summary(glmer_mod3)
## density plot
coefs = coef(glmer_mod3)[[1]] %>% as.data.frame()
coefs = coefs %>% rename("interaction_term" = "sqrt_dist_edge:forest_cover_prop")
## Just calculating the % of species with coefs above or below 0
prop_Pos_Dist = nrow(coefs[coefs$sqrt_dist_edge > 0,])/nrow(coefs)
prop_Pos_Forest = nrow(coefs[coefs$forest_cover_prop > 0,])/nrow(coefs)
prop_Neg_Int = nrow(coefs[coefs$interaction_term < 0,])/nrow(coefs)




## shade dark grey
dens <- density(coefs$sqrt_dist_edge)
data <- tibble(x = dens$x, y=dens$y) %>% 
  mutate(variable = case_when(
    (x >= 0) ~ "On",
    (x < 0) ~ "Off",
    TRUE ~ NA_character_))

## Plot for distance to range edge
g = ggplot(data, aes(x,y)) + theme_bw() + geom_line() + geom_area(data = filter(data, variable == 'On'), fill = 'grey') +
  geom_vline(aes(xintercept = 0.06233), linetype = "dashed") + 
  annotate("text", x = 0.12, y = 30, label = "99.3% > 0", size = 5) + 
  ggtitle("Square Root Distance to Range Edge") +
  theme(axis.text=element_text(size=24),
        axis.title=element_text(size=14,face="bold")) + xlim(-0.05,0.15) + ylim(0,40) + labs(x="Effect size",y="Density") 
g + theme_classic()

## shade dark grey
dens2 <- density(coefs$forest_cover_prop)
data2 <- tibble(x = dens2$x, y=dens2$y) %>% 
  mutate(variable = case_when(
    (x >= 0) ~ "On",
    (x < 0) ~ "Off",
    TRUE ~ NA_character_))


## Plot for forest cover
f = ggplot(data2, aes(x,y)) + geom_line() + geom_area(data = filter(data2, variable == 'On'), fill = 'grey') + theme_bw() + 
  geom_vline(aes(xintercept = 1.38162), linetype = "dashed") +
  annotate("text", x = 5, y = 0.4, label = "89.5% > 0", size = 5) + 
  ggtitle("Forest Cover Proportion") +
  theme(axis.text=element_text(size=24),
        axis.title=element_text(size=14,face="bold")) + xlim(-3,6.5) + ylim(0,0.6) + labs(x="Effect size",y="Density")
f + theme_classic()


## shade dark grey
dens3 <- density(coefs$interaction_term)
data3 <- tibble(x = dens3$x, y=dens3$y) %>% 
  mutate(variable = case_when(
    (x <= 0) ~ "On",
    (x > 0) ~ "Off",
    TRUE ~ NA_character_))

## Plot for interaction
e = ggplot(data3, aes(x,y)) + geom_line() + geom_area(data = filter(data3, variable == 'On'), fill = 'grey') + theme_bw() + 
  geom_vline(aes(xintercept = -0.03226), linetype = "dashed") +
  annotate("text", x = 0.15, y = 10, label = "90.2% < 0", size = 5) + 
  ggtitle("Interactive Effect") +
  theme(axis.text=element_text(size=24),
        axis.title=element_text(size=14,face="bold")) + xlim(-0.25,0.25) + ylim(0,25) + labs(x="Effect size",y="Density")
e + theme_classic()


## setting up figure
full_width <- 183 / 25.4
fh <- 183/25.4

pdf("foo.pdf", height=fh, width=full_width, pointsize=7, family='Helvetica')
grid.arrange(g, f, e)
dev.off()

##Looking at PID
coefs_pid = coef(glmer_mod3)[[2]] %>% as.data.frame()

## Just calculating the % of species with coefs above or below 0
prop_Forest_pid = nrow(coefs_pid[coefs_pid$forest_cover_prop > 0,])/nrow(coefs_pid)

## shade dark grey
dens4 <- density(coefs_pid$forest_cover_prop)
data4 <- tibble(x = dens4$x, y=dens4$y) %>% 
  mutate(variable = case_when(
    (x >= 0) ~ "On",
    (x < 0) ~ "Off",
    TRUE ~ NA_character_))


## Plot for forest cover
b = ggplot(data4, aes(x,y)) + geom_line() + geom_area(data = filter(data4, variable == 'On'), fill = 'grey') + theme_bw() + 
  geom_vline(aes(xintercept = 1.38162), linetype = "dashed") +
  annotate("text", x = 5, y = 0.4, label = "100% > 0", size = 5) + 
  ggtitle("Forest Cover Proportion") +
  theme(axis.text=element_text(size=24),
        axis.title=element_text(size=14,face="bold")) + xlim(-3,6.5) + ylim(0,0.6) + labs(x="Effect size",y="Density")
b + theme_classic()



