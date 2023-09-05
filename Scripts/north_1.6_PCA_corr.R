##################################################################################
# Analyses lat, climate, and mean trait data predicting seed number
## Daniel Anstett
## Last updated Sept 4, 2023
##################################################################################

# Clear environment
rm(list = ls())

#import libraries
library(MASS)
library(lmtest)
library(visreg)
library(tidyverse)
library(Hmisc)
library(car)
library(nlme)
library(cowplot)
library(quantreg)

ksr_coor <- read.csv("Data/ksr_coor.csv", header=T) # Imports individual dataset


##################################################################################
#11. Does PC1 and PC2 predict success?

plot(ksr_coor$PC1,ksr_coor$Seeds) # Unlikely 2nd order
plot(ksr_coor$PC2,ksr_coor$Seeds) # No evidence of 2nd order

qu_PC1 <- glm.nb(Seeds ~ PC1 ,data=ksr_coor)
Anova(qu_PC1,type = 3)

qu_PC2 <- glm.nb(Seeds ~ PC2 ,data=ksr_coor)
Anova(qu_PC2,type = 3)


# Nothing significant

plot11a<-visreg(qu_PC1, "PC1", scale="response", partial=TRUE, gg=TRUE, line=list(col="black")) +
  geom_point(size=1)+ scale_y_continuous(name="Seed Number", limits=c(0,100000),
                                         breaks=c(25000,50000,75000,100000))+
  scale_x_continuous(name="PC1")+ theme_classic()
plot11a <- plot11a +   theme(axis.text.x = element_text(size=13, face="bold"),
                           axis.text.y = element_text(size=13,face="bold"),
                           axis.title.x = element_text(color="black", size=12, vjust = 0, face="bold"),
                           axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
plot11a
ggsave("Single_fig/11a_PC1_seeds.pdf",width=7,height=6,units="in")


plot11b<-visreg(qu_PC2, "PC2", scale="response", partial=TRUE, gg=TRUE, line=list(col="black")) +
  geom_point(size=1)+ scale_y_continuous(name="Seed Number", limits=c(0,100000),
                                         breaks=c(25000,50000,75000,100000))+
  scale_x_continuous(name="PC2")+ theme_classic()
plot11b <- plot11b +   theme(axis.text.x = element_text(size=13, face="bold"),
                             axis.text.y = element_text(size=13,face="bold"),
                             axis.title.x = element_text(color="black", size=12, vjust = 0, face="bold"),
                             axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
plot11b
ggsave("Single_fig/11a_PC2_seeds.pdf",width=7,height=6,units="in")

## Cowplot Fig S3 export at 4 X 8 inches
plot_grid(plot11a,plot11b)

##################################################################################
#12. Are PC1 and PC2 correlated with Latitude?

plot(ksr_coor$Latitude,ksr_coor$PC1) # Unlikely 2nd order
plot(ksr_coor$Latitude,ksr_coor$PC2) # No evidence of 2nd order

qu_PC1_lat <- lm(PC1~Latitude,data=ksr_coor)
summary(qu_PC1_lat)
Anova(qu_PC1_lat,type = 3)

qu_PC2_lat <- lm(PC2~Latitude ,data=ksr_coor)
summary(qu_PC2_lat)
Anova(qu_PC2,type = 3)

plot12a <- ggplot(ksr_coor, aes(x=Latitude, y=PC1)) + 
  geom_point()+
  stat_smooth(method =lm,color="black")+
  theme_classic() + theme(axis.text.x = element_text(size=13, face="bold"),
                         axis.text.y = element_text(size=13,face="bold"),
                         axis.title.x = element_text(color="black", size=12, vjust = 0, face="bold"),
                         axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
plot12a
  

plot12b <- ggplot(ksr_coor, aes(x=Latitude, y=PC2)) + 
  geom_point()+
  stat_smooth(method =lm,color="black")+
  theme_classic() + theme(axis.text.x = element_text(size=13, face="bold"),
                          axis.text.y = element_text(size=13,face="bold"),
                          axis.title.x = element_text(color="black", size=12, vjust = 0, face="bold"),
                          axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
plot12b








