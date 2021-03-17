#################
# 1.1 Diagnostics 
#################

library(MASS)
library(AER)
library(performance)
library(ggplot2)
library(lme4)

########
#Individual Data
########

ksr_i <- read.csv("Data/ksr_means.csv", header=T) # Imports individual dataset

#Assess normality of seeds
qqnorm(ksr_i$Seeds) # ~ Extreme left skew, zero inflated.
ggplot(data=ksr_i,aes(x=Seeds)) + geom_histogram(bins=50)+theme_classic()
qqnorm(log(1+ksr_i$Seeds)) # ~ Extreme zero inflation
ggplot(data=ksr_i,aes(x=log(1+Seeds))) + geom_histogram()+theme_classic()

#Test for overdispersion
over.ksr <- glm(Seeds ~ ., data = ksr_i, family = poisson)
dispersiontest(over.ksr) #Data over dispersed so use negative binomal

#Test for Zero inflation
#zero.ksr <- glmer.nb(Seeds ~ (1|Pop) + (1|Block), data = ksr_i)
#check_zeroinflation(zero.ksr) # So us zero inflated, negative bionmial mixed model
#check_zeroinflation(over.ksr) # test seems broken, look at distribution


########
#Chemistry Data
########

chm <- read.csv("Data/chm.csv", header=T) # Imports individual dataset

#Assess distribution of seeds
qqnorm(ksr_i$Seeds) # ~ left skewed data
ggplot(data=chm,aes(x=Seeds)) + geom_histogram()+theme_classic()
qqnorm(log(1+chm$Seeds)) # some zero inflation
ggplot(data=chm,aes(x=log(1+Seeds))) + geom_histogram()+theme_classic()

#Test for overdispersion
over <- glm(Seeds ~ ., data = chm, family = poisson)
dispersiontest(over) #Data over dispersed so use negative binomal

#Test for Zero inflation
#zero.ksr <- glm.nb(Seeds ~ ., data = chm)
#check_zeroinflation(over) # test seems broken, look at distribution





