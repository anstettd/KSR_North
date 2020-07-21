#################
# Set up of full data frames for individual plant and plant means analysis. 
# Seed calculations
# Test of normality with transformations
#################

library(tidyverse)

#Individual dataset
ksr_i <- read.csv("Data/KSR_individual.csv", header=T) # Imports main dataset
ksr_i <- ksr_i %>% drop_na("Fruit") # Remove rows where fruit information is NA
ksr_length <- ksr_i %>% select(l1,l2,l3,l4,l5) ; ksr_Ml<-rowMeans(ksr_length,na.rm=T) # Average length
ksr_width <- ksr_i %>% select(d1,d2,d3,d4,d5) ; ksr_Mw<-rowMeans(ksr_width,na.rm=T) # Average width
ksr_i<- ksr_i %>% mutate (F_length=ksr_Ml) %>% mutate (F_width=ksr_Mw) # Add averages to ksr_i
ksr_i<- ksr_i %>% mutate (Seeds=2.33*F_length*F_width - 102.47)
ksr_i <- ksr_i %>% select(-l1,-l2,-l3,-l4,-l5,-d1,-d2,-d3,-d4,-d5,-Fruit,-F_length,-F_width, -Flower_Number)

#Assess normality of seeds
qqnorm(ksr_i$Seeds) # ~ normal
ggplot(data=ksr_i,aes(x=Seeds)) + geom_histogram()+theme_classic()

#Lat, distance, climate
pop_lat <- read.csv("Data/pop_lat.csv", header=T) # Imports main dataset


