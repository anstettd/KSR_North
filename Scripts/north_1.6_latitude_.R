##################################################################################
# Latitude vs traits while showing seeds as color on points
## Daniel Anstett
## Last updated Sept 12, 2023
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
library(RColorBrewer)

#import data
ksr_m <- read.csv("Data/ksr_m.csv", header=T) # Imports individual dataset


#################################################################################
#Make plots

####Phenology
lat_flower <- ggplot(na.omit(ksr_m), aes(x=Latitude, y=Flowering_Date),fill = Seeds,) + 
  geom_point(aes(fill = Seeds), shape=21,size=2.5)+
  ylab("Flowering Date")+
  scale_fill_gradientn(colours = rev(brewer.pal(11,"Spectral"))) +
  #scale_fill_gradientn(colours = rev(brewer.pal(11,"RdBu"))) +
  #stat_smooth(method =lm,color="black")+
  geom_vline(xintercept= 44.026349,linetype="dashed")+
  theme_classic() + theme(axis.text.x = element_text(size=13, face="bold"),
                          axis.text.y = element_text(size=13,face="bold"),
                          axis.title.x = element_text(color="black", size=15, vjust = 0, face="bold"),
                          axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
#legend.position = "none")
lat_flower

lat_bolt <- ggplot(na.omit(ksr_m), aes(x=Latitude, y=Bolt_Date),fill = Seeds,) + 
  geom_point(aes(fill = Seeds), shape=21,size=2.5)+
  ylab("Bolt Date")+
  scale_fill_gradientn(colours = rev(brewer.pal(11,"Spectral"))) +
  #stat_smooth(method =lm,color="black")+
  geom_vline(xintercept= 44.026349,linetype="dashed")+
  theme_classic() + theme(axis.text.x = element_text(size=13, face="bold"),
                          axis.text.y = element_text(size=13,face="bold"),
                          axis.title.x = element_text(color="black", size=15, vjust = 0, face="bold"),
                          axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
#legend.position = "none")
lat_bolt 

lat_growth <- ggplot(na.omit(ksr_m), aes(x=Latitude, y=Growth_Rate),fill = Seeds,) + 
  geom_point(aes(fill = Seeds), shape=21,size=2.5)+
  scale_fill_gradientn(colours = rev(brewer.pal(11,"Spectral"))) +
  #stat_smooth(method =lm,color="black")+
  geom_vline(xintercept= 44.026349,linetype="dashed")+
  theme_classic() + theme(axis.text.x = element_text(size=13, face="bold"),
                          axis.text.y = element_text(size=13,face="bold"),
                          axis.title.x = element_text(color="black", size=15, vjust = 0, face="bold"),
                          axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
#legend.position = "none")
lat_growth 

####Morphology
lat_tri <- ggplot(na.omit(ksr_m), aes(x=Latitude, y=Num_Trichomes),fill = Seeds,) + 
  geom_point(aes(fill = Seeds), shape=21,size=2.5)+
  scale_fill_gradientn(colours = rev(brewer.pal(11,"Spectral"))) +
  stat_smooth(method =lm,color="black")+
  ylab("Trichom Number")+
  geom_vline(xintercept= 44.026349,linetype="dashed")+
  theme_classic() + theme(axis.text.x = element_text(size=13, face="bold"),
                          axis.text.y = element_text(size=13,face="bold"),
                          axis.title.x = element_text(color="black", size=15, vjust = 0, face="bold"),
                          axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
#legend.position = "none")
lat_tri 


lat_water <- ggplot(na.omit(ksr_m), aes(x=Latitude, y=Water_Content),fill = Seeds,) + 
  geom_point(aes(fill = Seeds), shape=21,size=2.5)+
  scale_fill_gradientn(colours = rev(brewer.pal(11,"Spectral"))) +
  #stat_smooth(method =lm,color="black")+
  geom_vline(xintercept= 44.026349,linetype="dashed")+
  theme_classic() + theme(axis.text.x = element_text(size=13, face="bold"),
                          axis.text.y = element_text(size=13,face="bold"),
                          axis.title.x = element_text(color="black", size=15, vjust = 0, face="bold"),
                          axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
#legend.position = "none")
lat_water


lat_SLA <- ggplot(na.omit(ksr_m), aes(x=Latitude, y=SLA),fill = Seeds,) + 
  geom_point(aes(fill = Seeds), shape=21,size=2.5)+
  scale_fill_gradientn(colours = rev(brewer.pal(11,"Spectral"))) +
  stat_smooth(method =lm,color="black")+
  #geom_vline(xintercept= 44.026349,linetype="dashed")+
  theme_classic() + theme(axis.text.x = element_text(size=13, face="bold"),
                          axis.text.y = element_text(size=13,face="bold"),
                          axis.title.x = element_text(color="black", size=15, vjust = 0, face="bold"),
                          axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
#legend.position = "none")
lat_SLA 


#Chemistry
lat_TotpheFlower <- ggplot(na.omit(ksr_m), aes(x=Latitude, y=Flower_Totphe),fill = Seeds,) + 
  geom_point(aes(fill = Seeds), shape=21,size=2.5)+ ylab("Flower Total Phenolics")+
  scale_fill_gradientn(colours = rev(brewer.pal(11,"Spectral"))) +
  #stat_smooth(method =lm,color="black")+
  geom_vline(xintercept= 44.026349,linetype="dashed")+
  theme_classic() + theme(axis.text.x = element_text(size=13, face="bold"),
                          axis.text.y = element_text(size=13,face="bold"),
                          axis.title.x = element_text(color="black", size=15, vjust = 0, face="bold"),
                          axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
#legend.position = "none")
lat_TotpheFlower 


lat_TotpheFruit <- ggplot(na.omit(ksr_m), aes(x=Latitude, y=Fruit_Totphe),fill = Seeds,) + 
  geom_point(aes(fill = Seeds), shape=21,size=2.5)+ ylab("Fruit Total Phenolics")+
  scale_fill_gradientn(colours = rev(brewer.pal(11,"Spectral"))) +
  #stat_smooth(method =lm,color="black")+
  geom_vline(xintercept= 44.026349,linetype="dashed")+
  theme_classic() + theme(axis.text.x = element_text(size=13, face="bold"),
                          axis.text.y = element_text(size=13,face="bold"),
                          axis.title.x = element_text(color="black", size=15, vjust = 0, face="bold"),
                          axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
#legend.position = "none")
lat_TotpheFruit



lat_oeAFlower <- ggplot(na.omit(ksr_m), aes(x=Latitude, y=Flower_Oenothein_A),fill = Seeds,) + 
  geom_point(aes(fill = Seeds), shape=21,size=2.5)+ ylab("Flower Oenothein A")+
  scale_fill_gradientn(colours = rev(brewer.pal(11,"Spectral"))) +
  #stat_smooth(method =lm,color="black")+
  geom_vline(xintercept= 44.026349,linetype="dashed")+
  theme_classic() + theme(axis.text.x = element_text(size=13, face="bold"),
                          axis.text.y = element_text(size=13,face="bold"),
                          axis.title.x = element_text(color="black", size=15, vjust = 0, face="bold"),
                          axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
#legend.position = "none")
lat_oeAFlower 

lat_oeAFruit <- ggplot(na.omit(ksr_m), aes(x=Latitude, y=Fruit_Oenothein_A),fill = Seeds,) + 
  geom_point(aes(fill = Seeds), shape=21,size=2.5)+ ylab("Fruit Oenothein")+
  scale_fill_gradientn(colours = rev(brewer.pal(11,"Spectral"))) +
  #stat_smooth(method =lm,color="black")+
  geom_vline(xintercept= 44.026349,linetype="dashed")+
  theme_classic() + theme(axis.text.x = element_text(size=13, face="bold"),
                          axis.text.y = element_text(size=13,face="bold"),
                          axis.title.x = element_text(color="black", size=15, vjust = 0, face="bold"),
                          axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
#legend.position = "none")
lat_oeAFruit


##### Make Cowplot

#Flower and bolt #Export portrait at 6 X 8
plot_grid(lat_flower,lat_bolt, ncol=1) #Fig S3
plot_grid(lat_oeAFruit,lat_TotpheFruit, ncol=1) #Fig S4
plot_grid(lat_oeAFlower,lat_TotpheFlower, ncol=1) #Fig S5

##Include herbivores and traits all in Fig 3. Export at 10 X 5 inches
plot_grid(lat_flower,lat_bolt,lat_growth,lat_tri,lat_water,lat_SLA,ncol = 3)

#Plot chemistry export at 7 X 8 inches
plot_grid(lat_TotpheFlower,lat_TotpheFruit,lat_oeAFlower,lat_oeAFruit, ncol = 2)  





