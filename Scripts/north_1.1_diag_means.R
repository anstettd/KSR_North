#################
# 1.1 Diagnostics 
#################

library(MASS)
library(AER)
library(performance)
library(ggplot2)
library(lme4)

########
# Test for normality of the response variable Seeds
########

ksr_m <- read.csv("Data/ksr_m.csv", header=T) # Imports individual dataset

#Assess normality of seeds
qqnorm(ksr_m$Seeds) # ~ extreme left skew, negative binomial or poisson
ggplot(data=ksr_m,aes(x=Seeds)) + geom_histogram(bins=100)+theme_classic()
qqnorm(log(1+ksr_m$Seeds)) # ~ Extreme zero inflation
ggplot(data=ksr_m,aes(x=log(1+Seeds))) + geom_histogram(bins = 100)+theme_classic()

#Test for overdispersion
over.ksr <- glm(Seeds ~ ., data = ksr_m, family = poisson)
dispersiontest(over.ksr) #Data over dispersed so use negative binomal


