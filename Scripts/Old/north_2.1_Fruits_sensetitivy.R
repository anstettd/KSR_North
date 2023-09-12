##################################################################################
# Analyses lat, climate, and mean trait data predicting fruits not damaged "Fruit_no_damage"
## Daniel Anstett
## Last updated Jan 31, 2022
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

#import data
ksr_m <- read.csv("Data/ksr_m.csv", header=T) # Imports individual dataset
attach(ksr_m)

pref_theme <- theme_classic() + theme(axis.text.x = element_text(size=13, face="bold"),
                                      axis.text.y = element_text(size=13,face="bold"),
                                      axis.title.x = element_text(color="black", size=15, vjust = 0, face="bold"),
                                      axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6),
                                      legend.position = "none")

##################################################################################
# What geographic/environmental traits predict plant fitness?
##################################################################################


#1. Does geography predict success?
  plot(Latitude,Fruit_no_damage) #possible quadratic pattern
  plot(Longitude,Fruit_no_damage) #possible quadratic pattern
  qu_geo <- glm.nb (Fruit_no_damage ~ Latitude + Longitude + I(Latitude^2) + I(Longitude^2), data=ksr_m)
  stepAIC(qu_geo,direction="both") # lowest AIC is Lat and Lat^2
  qu_lat <- lm (Fruit_no_damage ~ Latitude + I(Latitude^2), data=ksr_m)
  Anova(qu_lat,type=3) # both highly significant
  
  #Get peak of ggplot regression line
  plot1_peak <- visreg(qu_lat, "Latitude", scale="response", partial=TRUE)
  maxseed <- max(plot1_peak$fit$visregFit)
  max_all <- plot1_peak$fit %>% filter(visregFit==maxseed)
  max_lat <- max_all[1,1]
  
  #Make Plot
  plot1<-visreg(qu_lat, "Latitude", scale="response", partial=TRUE, gg=TRUE, line=list(col="black")) +
    geom_point(size=1)+ scale_y_continuous(name="Seed Number")+ theme_classic()
  plot1 <- plot1 + theme(axis.text.x = element_text(size=13, face="bold"),
      axis.text.y = element_text(size=13,face="bold"),
      axis.title.x = element_text(color="black", size=15, vjust = 0, face="bold"),
      axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))+
#    geom_hline(yintercept=19003.09930)+
    geom_vline(xintercept= max_lat)+
    geom_vline(xintercept= 44.026349,linetype="dashed")
  plot1
#ggsave("Single_fig/1.Lat.pdf", width = 7, height = 6, units = "in")




##################################################################################
#2. Does distance to common garden predict Fruit_no_damage?
  plot(Distance,Fruit_no_damage)
  qu_dist <- glm.nb (Fruit_no_damage ~ Distance)
  qu_dist
  Anova(qu_dist,type=3)
  
  #Make Plot
  plot2<- visreg(qu_dist, "Distance", scale="response", partial=TRUE, gg=TRUE, line=list(col="black")) +
    geom_point(size=1)+ scale_y_continuous(name="Seed Number")+ theme_classic()
  plot2 <- plot2 + theme(axis.text.x = element_text(size=13, face="bold"),
                         axis.text.y = element_text(size=13,face="bold"),
                         axis.title.x = element_text(color="black", size=15, vjust = 0, face="bold"),
                         axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
  plot2
  #ggsave("Single_fig/2.Diatance.pdf", width = 7, height = 6, units = "in")

##################################################################################  
#3. What environmental variables at each location best predicts success?
  plot(MAT,Fruit_no_damage) # all could have quadratic components
  plot(MSP,Fruit_no_damage)
  plot(CMD,Fruit_no_damage)
  plot(RH,Fruit_no_damage)
  qu_env <- glm.nb(Fruit_no_damage ~ MAT + MSP + CMD + RH + I(MAT^2) + I(MSP^2) + I(CMD^2) + I(RH^2), data=ksr_m)
  stepAIC(qu_env,direction="both") # Keep MAT and RH
  qu_MAT_RH <- glm.nb(Fruit_no_damage ~ MAT + RH + MSP + I(MAT^2), data=ksr_m)
  Anova(qu_MAT_RH,type=3) #MAT significant, RH marginally significant
  
  #Get peak of MAT ggplot regression line
  plot3_peak <- visreg(qu_MAT_RH, "MAT", scale="response", partial=TRUE)
  maxseed <- max(plot3_peak$fit$visregFit)
  max_all <- plot3_peak$fit %>% filter(visregFit==maxseed)
  max_MAT <- max_all[1,1]
  max_MAT
  
  #MAT
  plot3A <-visreg(qu_MAT_RH, "MAT", scale="response", partial=TRUE, gg=TRUE, line=list(col="black")) +
    geom_point(size=1)+ scale_x_continuous(name="Mean Annual Temperature (°C)")+
    scale_y_continuous(name="Seed Number")+ theme_classic()
  plot3A <- plot3A + theme(axis.text.x = element_text(size=13, face="bold"),
                         axis.text.y = element_text(size=13,face="bold"),
                         axis.title.x = element_text(color="black", size=15, vjust = 0, face="bold"),
                         axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))+
    geom_vline(xintercept= max_MAT)+
    geom_vline(xintercept= 7.9,linetype="dashed")
  plot3A
  #ggsave("Single_fig/3A.MAT.pdf", width = 7, height = 6, units = "in")
  
  #Get peak of RH ggplot regression line
  plot3b_peak <- visreg(qu_MAT_RH, "RH", scale="response", partial=TRUE)
  maxseed <- max(plot3b_peak$fit$visregFit)
  max_all <- plot3b_peak$fit %>% filter(visregFit==maxseed)
  max_RH <- max_all[1,2]
  
  #RH 
  visreg(qu_MAT_RH, "RH", scale="response", partial=TRUE, gg=TRUE)
  plot3B <-visreg(qu_MAT_RH, "RH", scale="response", partial=TRUE, gg=TRUE, line=list(col="black")) +
    geom_point(size=1)+ scale_x_continuous(name="Relative Huminidty (%)")+
    scale_y_continuous(name="Seed Number")+ theme_classic()
  plot3B <- plot3B + theme(axis.text.x = element_text(size=13, face="bold"),
                           axis.text.y = element_text(size=13,face="bold"),
                           axis.title.x = element_text(color="black", size=15, vjust = 0, face="bold"),
                           axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))+
    geom_vline(xintercept= max_RH)+
    geom_vline(xintercept= 69,linetype="dashed")
  plot3B
  #ggsave("Single_fig/3B.RH.pdf", width = 7, height = 6, units = "in")
  
  #Get peak of RH ggplot regression line
  plot3b_peak <- visreg(qu_MAT_RH, "RH", scale="response", partial=TRUE)
  maxseed <- max(plot3b_peak$fit$visregFit)
  max_all <- plot3b_peak$fit %>% filter(visregFit==maxseed)
  max_MSP <- max_all[1,2]
  
  #RH 
  visreg(qu_MAT_MSP, "MSP", scale="response", partial=TRUE, gg=TRUE)
  plot3C <-visreg(qu_MAT_RH, "MSP", scale="response", partial=TRUE, gg=TRUE, line=list(col="black")) +
    geom_point(size=1)+ scale_x_continuous(name="Mean Summer Precipitation")+
    scale_y_continuous(name="Seed Number")+ theme_classic()
  plot3C <- plot3C + theme(axis.text.x = element_text(size=13, face="bold"),
                           axis.text.y = element_text(size=13,face="bold"),
                           axis.title.x = element_text(color="black", size=15, vjust = 0, face="bold"),
                           axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))+
    geom_vline(xintercept= max_MPS)+
    geom_vline(xintercept= 69,linetype="dashed")
  plot3C
  #ggsave("Single_fig/3B.RH.pdf", width = 7, height = 6, units = "in")
  
  
  
##################################################################################
#4. What env distance (common garden env – genotype’s env) best predicts success?
  plot(MAT_Distance,Fruit_no_damage) # all could have quadratic components
  plot(MSP_Distance,Fruit_no_damage)
  plot(CMD_Distance,Fruit_no_damage)
  plot(RH_Distance,Fruit_no_damage)
  qu_envdist <- glm.nb(Fruit_no_damage ~ MAT_Distance + MSP_Distance + CMD_Distance + RH_Distance +
                     I(MAT_Distance^2) + I(MSP_Distance^2) + I(CMD_Distance^2) + I(RH_Distance^2), data=ksr_m)
  stepAIC(qu_envdist,direction="both") # MSP_Distance + MAT_Distance^2 selected
  qu_MAT_MSP_D <- glm.nb(Fruit_no_damage ~ MAT_Distance + MSP_Distance + 
                           RH_Distance + I(MAT_Distance^2) + I(RH_Distance^2)) 
  Anova(qu_MAT_MSP_D,type=3) # MSP distance marginally significant, 
  visreg(qu_MAT_MSP_D, "MAT_Distance", scale="response", partial=TRUE, gg=TRUE)
  visreg(qu_MAT_MSP_D, "MSP_Distance", scale="response", partial=TRUE, gg=TRUE)
  
  #Mat distance alone
  qu_MAT_D<- glm.nb(Fruit_no_damage ~ MAT_Distance + I(MAT_Distance^2), data=ksr_m)
  stepAIC(qu_MAT_D,direction="both") #Keep quadratic
  Anova(qu_MAT_D,type=3)
  
  #Get peak of RH ggplot regression line
  plot4_peak <- visreg(qu_MAT_D, "MAT_Distance", scale="response", partial=TRUE)
  maxseed <- max(plot4_peak$fit$visregFit)
  max_all <- plot4_peak$fit %>% filter(visregFit==maxseed)
  max_MATd <- max_all[1,1]
  
  #Plot
  plot4 <-visreg(qu_MAT_D, "MAT_Distance", scale="response", partial=TRUE, gg=TRUE, line=list(col="black")) +
    geom_point(size=1)+ scale_x_continuous(name="Temperature Distance (°C)")+
    scale_y_continuous(name="Seed Number")+ theme_classic()
  plot4 <- plot4 + theme(axis.text.x = element_text(size=13, face="bold"),
                         axis.text.y = element_text(size=13,face="bold"),
                         axis.title.x = element_text(color="black", size=15, vjust = 0, face="bold"),
                         axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))+
    geom_vline(xintercept= max_MATd)+
    geom_vline(xintercept= 0,linetype="dashed")
  plot4
  
  #ggsave("Single_fig/4.MAT_distance.pdf", width = 7, height = 6, units = "in")
  

  
## Cowplot export at 7 X 9 inches
  plot_grid(plot1,plot2,plot3A,plot3B,ncol = 2)
  
  
  
  
##################################################################################
  # What plant traits predict plant fitness?
##################################################################################
  
##################################################################################
#5. Does herb impact predict success?

  #a. Do leaf herbiovry and xylem feeders impact seed number?
  leaf_bug <- ksr_m %>% select(Pop,Fruit_no_damage, Leaf_Herb_Sept, bug) #subset data
  leaf_bug <- na.omit(leaf_bug) #remove NA rows
  cor(leaf_bug$Leaf_Herb_Sept,leaf_bug$bug) #not correlated
  
  #Test leaf bug models
  plot(Leaf_Herb_Sept,Fruit_no_damage) # all could have quadratic components
  plot(bug,Fruit_no_damage)
  qu_leaf_bug <- glm.nb(Fruit_no_damage ~ Leaf_Herb_Sept + bug + I(Leaf_Herb_Sept^2) + I(bug^2), data=ksr_m)
  stepAIC(qu_leaf_bug,direction="both") # bug only selected
  qu_bug <- glm.nb(Fruit_no_damage ~ bug + I(bug^2), data=ksr_m)
  Anova(qu_leaf_bug,type=3) #Leaf herbivory not significant
  Anova(qu_bug,type=3) #Bug and Bug^2 highly significant
 #bug not having a negative effect no seed number
  plot5a <- visreg(qu_bug, "bug", scale="response", partial=TRUE, gg=TRUE, line=list(col="black")) +
    geom_point(size=1)+ scale_x_continuous(name="P. spumarius Number")+
    scale_y_continuous(name="Seed Number")+ theme_classic()
  plot5a <- plot5a + theme(axis.text.x = element_text(size=13, face="bold"),
                         axis.text.y = element_text(size=13,face="bold"),
                         axis.title.x = element_text(color="black", size=12, vjust = 0, face="bold.italic"),
                         axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
  plot5a
  #ggsave("Single_fig/5A.bug.pdf", width = 7, height = 6, units = "in")
  
  #c. Does seed predation impact seed number?
  sf_mb <- ksr_m %>% select(Pop,Fruit_no_damage, S.florida, M.brevivatella) #subset data
  sf_mb <- na.omit(sf_mb) #remove NA rows
  cor(sf_mb$S.florida,sf_mb$M.brevivatella) #not correlated
  
  #Test S.florida, M.brevivatella models
  plot(S.florida,Fruit_no_damage) # No evidence of quadratic model
  plot(M.brevivatella,Fruit_no_damage) # No evidence of quadratic model
  qu_sf_mb <- glm.nb(Fruit_no_damage ~ S.florida + M.brevivatella, data=ksr_m)
  stepAIC(qu_sf_mb,direction="both") # Both seed predators selected
  Anova(qu_sf_mb,type=3) #S.florida significant, M.brevivietall marginally significant
  #plots
  #Not the most relevant explanation of the pattern
  plot5b <- visreg(qu_sf_mb, "S.florida", scale="response", partial=TRUE, gg=TRUE, line=list(col="black")) +
    geom_point(size=1)+ scale_x_continuous(name="S. florida Damanged Fruits")+
    scale_y_continuous(name="Seed Number")+ theme_classic()
  plot5b <- plot5b + theme(axis.text.x = element_text(size=13, face="bold"),
                           axis.text.y = element_text(size=13,face="bold"),
                           axis.title.x = element_text(color="black", size=12, vjust = 0, face="bold.italic"),
                           axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
  plot5b
  #ggsave("Single_fig/5B.sf.pdf", width = 7, height = 6, units = "in")
  
  plot5c <- visreg(qu_sf_mb, "M.brevivatella", scale="response", partial=TRUE, gg=TRUE, line=list(col="black")) +
    geom_point(size=1)+ scale_x_continuous(name="M. brevivitella Damanged Fruits")+
    scale_y_continuous(name="Seed Number")+ theme_classic()
  plot5c <- plot5c + theme(axis.text.x = element_text(size=13, face="bold"),
                           axis.text.y = element_text(size=13,face="bold"),
                           axis.title.x = element_text(color="black", size=12, vjust = 0, face="bold.italic"),
                           axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
  plot5c
  #ggsave("Single_fig/5C.mb.pdf", width = 7, height = 6, units = "in")

  ## Cowplot export at 4 X 11 inches
  plot_grid(plot5a,plot5b,plot5c,ncol = 3)
  
  #Quantile regression
  #Predict the 10th quantile of plant performance using specialist seed predators
  quant10sf <- rq(Fruit_no_damage ~ S.florida, data = ksr_m, tau = 0.10)
  summary(quant10sf,se = "nid")
  plot_5q1 <- ggplot(ksr_m, aes(S.florida,Fruit_no_damage)) +
    geom_point() + 
    geom_abline(intercept=coef(quant10sf)[1], slope=coef(quant10sf)[2])+
    scale_x_continuous(name="S. florida Damaged Fruits")+
    scale_y_continuous(name="Seed Number")+ pref_theme
  plot_5q1
  #ggsave("Single_fig/Quan1.mb.pdf", width = 7, height = 6, units = "in")
  
  quant10mb <- rq(Fruit_no_damage ~ M.brevivatella, data = ksr_m, tau = 0.10)
  summary(quant10mb,se = "nid")
  plot_5q2 <- ggplot(ksr_m, aes(M.brevivatella,Fruit_no_damage)) +
    geom_point() + 
    geom_abline(intercept=coef(quant10mb)[1], slope=coef(quant10mb)[2])+
    scale_x_continuous(name="M. brevivitella Damaged Fruits")+
    scale_y_continuous(name="Seed Number")+ pref_theme
  plot_5q2
  #ggsave("Single_fig/Quan1.mb.pdf", width = 7, height = 6, units = "in")
  
##################################################################################
#6. Does phenology predict success?
  pheno <- ksr_m %>% select(Pop, Fruit_no_damage, Flowering_Date, Bolt_Date, Growth_Rate) #subset data
  pheno <- na.omit(pheno) #remove NA rows
  pheno_matrix <- as.matrix(pheno)
  rcorr(pheno_matrix) #correlation r<|0.45| for all
  
  plot(pheno$Flowering_Date,pheno$Fruit_no_damage) # all could have quadratic components
  plot(pheno$Bolt_Date,pheno$Fruit_no_damage)
  plot(pheno$Growth_Rate,pheno$Fruit_no_damage)
  
#Test phenology models
  qu_pheno <- glm.nb(Fruit_no_damage ~ Flowering_Date + I(Flowering_Date^2) + Bolt_Date  
                       + Growth_Rate + I(Growth_Rate^2), data=pheno)
  stepAIC(qu_pheno,direction="both") # AIC scores lower without Bolt^2
  qu_pheno_2 <- glm.nb(Fruit_no_damage ~ Flowering_Date + I(Flowering_Date^2) + Bolt_Date 
                       + Growth_Rate + I(Growth_Rate^2), data=pheno)
  Anova(qu_pheno_2,type=3) #Flowering Date and Growth Rate significant
#Graphs
  #Flowering Date
  plot6a <- visreg(qu_pheno_2, "Flowering_Date", scale="response", partial=TRUE, gg=TRUE , line=list(col="black")) +
  geom_point(size=1)+ scale_x_continuous(name="Flowering Date",limits = c(150,300))+
  scale_y_continuous(name="Seed Number")+ theme_classic()
  plot6a <- plot6a + theme(axis.text.x = element_text(size=13, face="bold"),
                             axis.text.y = element_text(size=13,face="bold"),
                             axis.title.x = element_text(color="black", size=12, vjust = 0, face="bold"),
                             axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
  plot6a
  #ggsave("Single_fig/6a.Flowering.pdf", width = 7, height = 6, units = "in")
#Bolt Date
  plot6b <- visreg(qu_pheno_2, "Bolt_Date", scale="response", partial=TRUE, gg=TRUE, line=list(col="black")) +
  geom_point(size=1)+ scale_x_continuous(name="Bolt Date")+
  scale_y_continuous(name="Seed Number")+ theme_classic()
  plot6b <- plot6b + theme(axis.text.x = element_text(size=13, face="bold"),
                         axis.text.y = element_text(size=13,face="bold"),
                         axis.title.x = element_text(color="black", size=12, vjust = 0, face="bold"),
                         axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
  plot6b
  #ggsave("Single_fig/6b.bolt.pdf", width = 7, height = 6, units = "in")
  
  #Quantile regression
  #Predict the 10th quantile of plant performance using specialist seed predators
  quant10bd <- rq(Fruit_no_damage ~ Bolt_Date, data = ksr_m, tau = 0.1)
  summary(quant10bd,se = "nid")
  plot_5q3 <- ggplot(ksr_m, aes(Bolt_Date,Fruit_no_damage)) +
    geom_point() + 
    geom_abline(intercept=coef(quant10bd)[1], slope=coef(quant10bd)[2])+
    scale_x_continuous(name="Bolt Date")+
    scale_y_continuous(name="Seed Number")+ pref_theme
  plot_5q3
  
#Growth Rate
  plot6c <- visreg(qu_pheno_2, "Growth_Rate", scale="response", partial=TRUE, gg=TRUE, line=list(col="black")) +
  geom_point(size=1)+ scale_x_continuous(name="Growth Rate")+
  scale_y_continuous(name="Seed Number")+ theme_classic()
  plot6c <- plot6c + theme(axis.text.x = element_text(size=13, face="bold"),
                         axis.text.y = element_text(size=13,face="bold"),
                         axis.title.x = element_text(color="black", size=12, vjust = 0, face="bold"),
                         axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
  plot6c   
  
  #Peak Numbers
  plot6_peak <- visreg(qu_pheno_2, "Flowering_Date", scale="response", partial=TRUE)
  maxseed <- max(plot6_peak$fit$visregFit)
  max_all <- plot6_peak$fit %>% filter(visregFit==maxseed)
  max_all[1,1]
  plot6_peak <- visreg(qu_pheno_2, "Growth_Rate", scale="response", partial=TRUE)
  maxseed <- max(plot6_peak$fit$visregFit)
  max_all <- plot6_peak$fit %>% filter(visregFit==maxseed)
  max_all[1,3]
  
    
## Cowplot export at 4 X 11 inches
  plot_grid(plot6b,plot6c)  
    
    
    
##################################################################################
#7. Does morphology predict success?
  morpho <- ksr_m %>% select(Pop, Fruit_no_damage, SLA, Water_Content, Leaf_Toughness, Num_Trichomes) #subset data
  morpho <- na.omit(morpho) #remove NA rows
  morpho_matrix <- as.matrix(morpho)
  rcorr(morpho_matrix) #Trichomes highl correalted with leaf thoughness. Keep trichomes. Rest cor <0.4
  morpho <- ksr_m %>% select(Pop, Fruit_no_damage, SLA, Water_Content, Num_Trichomes) #subset data
  plot(morpho$SLA,morpho$Fruit_no_damage) #All could have quadratic relationship
  plot(morpho$Water_Content,morpho$Fruit_no_damage)
  plot(morpho$Num_Trichomes,morpho$Fruit_no_damage)
  
  #Test Morphology Models
  qu_morpho <- glm.nb(Fruit_no_damage ~ SLA + I(SLA^2) + Water_Content + I(Water_Content^2) + 
                        Num_Trichomes + I(Num_Trichomes^2), data=morpho)
  stepAIC(qu_morpho,direction="both") # AIC scores lower without I(SLA^2)
  qu_morpho_2 <- glm.nb(Fruit_no_damage ~ SLA + Water_Content + I(Water_Content^2) + 
                          Num_Trichomes + I(Num_Trichomes^2), data=morpho)
  qu_morpho_2
  Anova(qu_morpho_2 ,type=3) #SLA not significant

  #Trichomes
  plot7a <- visreg(qu_morpho_2, "Num_Trichomes", scale="response", partial=TRUE, gg=TRUE, line=list(col="black")) +
    geom_point(size=1)+ scale_x_continuous(name="Trichome Number",breaks=c(50,100,150,200,250))+
    scale_y_continuous(name="Seed Number")+ theme_classic()
  plot7a <- plot7a + theme(axis.text.x = element_text(size=13, face="bold"),
                           axis.text.y = element_text(size=13,face="bold"),
                           axis.title.x = element_text(color="black", size=12, vjust = 0, face="bold"),
                           axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
  plot7a
  #ggsave("Single_fig/7a.Tricomes.pdf", width = 7, height = 6, units = "in")
  
  
  
  #Water Content
  plot7b <- visreg(qu_morpho_2, "Water_Content", scale="response", partial=TRUE, gg=TRUE, line=list(col="black")) +
    geom_point(size=1)+ scale_x_continuous(name="% Water Content",breaks=c(65,70,75,80))+
    scale_y_continuous(name="Seed Number")+ theme_classic()
  plot7b <- plot7b + theme(axis.text.x = element_text(size=13, face="bold"),
                           axis.text.y = element_text(size=13,face="bold"),
                           axis.title.x = element_text(color="black", size=12, vjust = 0, face="bold"),
                           axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
  plot7b
  #ggsave("Single_fig/7b.wc.pdf", width = 7, height = 6, units = "in")
  
  
  #SLA
  plot7c <- visreg(qu_morpho_2, "SLA", scale="response", partial=TRUE, gg=TRUE, line=list(col="black")) +
    geom_point(size=1)+ scale_x_continuous(name="SLA",breaks=c(40,60,80,100))+
    scale_y_continuous(name="Seed Number")+ theme_classic()
  plot7c <- plot7c + theme(axis.text.x = element_text(size=13, face="bold"),
                           axis.text.y = element_text(size=13,face="bold"),
                           axis.title.x = element_text(color="black", size=12, vjust = 0, face="bold"),
                           axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
  plot7c
  #ggsave("Single_fig/7c.SLA.pdf", width = 7, height = 6, units = "in")
  
  #Peak Numbers
  plot7_peak <- visreg(qu_morpho_2, "Num_Trichomes", scale="response", partial=TRUE)
  maxseed <- max(plot7_peak$fit$visregFit)
  max_all <- plot7_peak$fit %>% filter(visregFit==maxseed)
  max_all[1,3]
  plot7_peak <- visreg(qu_morpho_2, "Water_Content", scale="response", partial=TRUE)
  maxseed <- max(plot7_peak$fit$visregFit)
  max_all <- plot7_peak$fit %>% filter(visregFit==maxseed)
  max_all[1,2]
  
  
  ## Cowplot Fig 4 export at 7 X 10 inches
  plot_grid(plot6a,plot7a,plot7b,plot7c,ncol = 2)  
  
##################################################################################
  #Chemistry Data
##################################################################################

  #Generate oxidative capacity as totphe - pH10
  ksr_m <- ksr_m %>% mutate(Leaf_Oxidative_Capacity=Leaf_Totphe-Leaf_pH_10) %>%
                  mutate(Flower_Oxidative_Capacity=Flower_Totphe-Flower_pH_10) %>%
                  mutate(Fruit_Oxidative_Capacity=Fruit_Totphe-Fruit_pH_10)

##################################################################################
#8. Does broad chemistry predict success?
  tphe <- ksr_m %>% select(Pop, Fruit_no_damage, Leaf_Totphe,Flower_Totphe,Fruit_Totphe,
                         Leaf_Oxidative_Capacity,Flower_Oxidative_Capacity,Fruit_Oxidative_Capacity) #subset data
  tphe <- na.omit(tphe) #remove NA rows, leaf separate
  tphe_matrix <- tphe %>% select(-Pop,-Fruit_no_damage) #subset data
  tphe_matrix <- as.matrix(tphe_matrix)
  rcorr(tphe_matrix) # Oxidative capacity is highly correlated to total phenolics. Remove oxidative capacity.
  
  #generate dataframes with only the correct tissue
  tphe_leaf<-ksr_m %>% select(Pop, Fruit_no_damage, Leaf_Totphe)
  tphe_leaf<- na.omit(tphe_leaf) #remove NA rows, leaf separate
  tphe_flr<-ksr_m %>% select(Pop, Fruit_no_damage, Flower_Totphe, Fruit_Totphe)
  tphe_flr<- na.omit(tphe_flr) #remove NA rows, leaf separate
  
  plot(tphe_leaf$Leaf_Totphe,tphe_leaf$Fruit_no_damage) #Could be quadratic
  plot(tphe_flr$Flower_Totphe,tphe_flr$Fruit_no_damage)
  plot(tphe_flr$Fruit_Totphe,tphe_flr$Fruit_no_damage)
  
  
################################
  #models for Leaf
  qu_tphe_leaf2 <- glm.nb(Fruit_no_damage ~ Leaf_Totphe + I(Leaf_Totphe^2), data=tphe_leaf)
  stepAIC(qu_tphe_leaf2,direction="both") #keep remove Leaf^2
  qu_tphe_leaf <- glm.nb(Fruit_no_damage ~ Leaf_Totphe, data=tphe_leaf)
  qu_tphe_leaf
  Anova(qu_tphe_leaf,type = 3) # leaf not significant
  
  #Leaf Total Phenolics
  plot8a<-visreg(qu_tphe_leaf, "Leaf_Totphe", scale="response", partial=TRUE, gg=TRUE, line=list(col="black")) +
    geom_point(size=1)+ scale_y_continuous(name="Seed Number")+
    scale_x_continuous(name="Leaf Total Phenolics (mg/g)")+ theme_classic()
  plot8a <- plot8a +   theme(axis.text.x = element_text(size=13, face="bold"),
                             axis.text.y = element_text(size=13,face="bold"),
                             axis.title.x = element_text(color="black", size=12, vjust = 0, face="bold"),
                             axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
  plot8a
  #ggsave("Single_fig/8a_Leaf_totphe.pdf",width=7,height=6,units="in")

################################ 
  #models for Flower 
  qu_tph_fl <- glm.nb(Fruit_no_damage ~ Flower_Totphe + I(Flower_Totphe^2), data=tphe_flr) 
  stepAIC(qu_tph_fl,direction="both") #remove flower_Totphe 1st order
  qu_flr_2 <- glm.nb(Fruit_no_damage ~ Flower_Totphe + I(Flower_Totphe^2), data=tphe_flr) 
  Anova(qu_flr_2 ,type = 3) #Not significant, qudratic marginal


  #Flower Total Phenolics
  plot8b<-visreg(qu_flr_2, "Flower_Totphe", scale="response", partial=TRUE, gg=TRUE, line=list(col="black")) +
    geom_point(size=1)+ scale_y_continuous(name="Seed Number")+
    scale_x_continuous(name="Flower Total Phenolics (mg/g)",limits = c(20,105),breaks=c(20,40,60,80,100))+ theme_classic()
  plot8b <- plot8b +   theme(axis.text.x = element_text(size=13, face="bold"),
                             axis.text.y = element_text(size=13,face="bold"),
                             axis.title.x = element_text(color="black", size=12, vjust = 0, face="bold"),
                             axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
  plot8b
  #ggsave("Single_fig/8b_Flower_totphe.pdf",width=7,height=6,units="in")
  
  #Peak Numbers
  plot7_peak <- visreg(qu_flr_2, "Flower_Totphe", scale="response", partial=TRUE)
  maxseed <- max(plot7_peak$fit$visregFit)
  max_all <- plot7_peak$fit %>% filter(visregFit==maxseed)
  max_all[1,1]
  
################################   
  #models for Fruit 
  qu_tph_r <- glm.nb(Fruit_no_damage ~ Fruit_Totphe + I(Fruit_Totphe^2), data=tphe_flr) 
  stepAIC(qu_tph_r,direction="both") # Keep both main effect and quadratic effect
  qu_flr_3 <- glm.nb(Fruit_no_damage ~ Fruit_Totphe + I(Fruit_Totphe^2), data=tphe_flr) 
  Anova(qu_flr_3 ,type = 3) #
  
  #Fruit Total Phenolics
  plot8c<-visreg(qu_flr_3, "Fruit_Totphe", scale="response", partial=TRUE, gg=TRUE, line=list(col="black")) +
    geom_point(size=1)+ scale_y_continuous(name="Seed Number")+
    scale_x_continuous(name="Fruit Total Phenolics (mg/g)",breaks=c(60,100,140,180))+ theme_classic()
  plot8c <- plot8c +   theme(axis.text.x = element_text(size=13, face="bold"),
                             axis.text.y = element_text(size=13,face="bold"),
                             axis.title.x = element_text(color="black", size=12, vjust = 0, face="bold"),
                             axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
  plot8c
  #ggsave("Single_fig/8c_Fruit_totphe.pdf",width=7,height=6,units="in")
  
  ## Cowplot export at 4 X 8 inches
  plot_grid (plot8b,plot8c,ncol = 3)  
  
  
##################################################################################
#9. Does detailed chemistry predict success? (Oe=Oenothein)
    oenothein <- ksr_m %>% select(Pop, Fruit_no_damage, Leaf_Oenothein_B, Leaf_Oenothein_A,
                           Flower_Oenothein_B, Flower_Oenothein_A,
                           Fruit_Oenothein_B, Fruit_Oenothein_A,) #subset data
    oenothein.matrix <- ksr_m %>% select(Leaf_Oenothein_B, Leaf_Oenothein_A,
                                Flower_Oenothein_B, Flower_Oenothein_A,
                                Fruit_Oenothein_B, Fruit_Oenothein_A) #subset data
    oenothein <- na.omit(oenothein) #remove NA rows
    oenothein <- na.omit(oenothein.matrix) #remove NA rows
    oenothein_matrix <- as.matrix(oenothein.matrix)
    rcorr.oe<-rcorr(oenothein_matrix) # All oenothein highly correlated. Do each tissue separately. Lump A and oxA together
        write.csv(rcorr.oe$r,"oe_rcorr.csv", row.names = TRUE)

#models Leaf
    oe_Leaf<-ksr_m %>% select(Pop, Fruit_no_damage, Leaf_Oenothein_B, Leaf_Oenothein_A,Leaf_Ox_Oenothein_A)
    plot(oe_Leaf$Leaf_Oenothein_A,oe_Leaf$Fruit_no_damage) #Could be second order
    plot(oe_Leaf$Leaf_Oenothein_B,oe_Leaf$Fruit_no_damage) #First order only
    
    oe_Leaf <- na.omit(oe_Leaf)
    
    qu_oeA_Leaf <- glm.nb(Fruit_no_damage ~ Leaf_Oenothein_A + I(Leaf_Oenothein_A^2),data=oe_Leaf)
    stepAIC(qu_oeA_Leaf,direction="both") # Keep both
    qu_A_Leaf <- glm.nb(Fruit_no_damage ~ Leaf_Oenothein_A + I(Leaf_Oenothein_A^2) ,data=oe_Leaf)
    qu_A_Leaf
    Anova(qu_A_Leaf,type = 3) # Nothing significant
    
    plot9a<-visreg(qu_A_Leaf, "Leaf_Oenothein_A", scale="response", partial=TRUE, gg=TRUE, line=list(col="black")) +
      geom_point(size=1)+ scale_y_continuous(name="Seed Number")+
      scale_x_continuous(name="Leaf Oenothein A (mg/g)")+ theme_classic()
    plot9a <- plot9a +   theme(axis.text.x = element_text(size=13, face="bold"),
                               axis.text.y = element_text(size=13,face="bold"),
                               axis.title.x = element_text(color="black", size=12, vjust = 0, face="bold"),
                               axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
    plot9a
    #ggsave("Single_fig/9a_Leaf_oe.pdf",width=7,height=6,units="in")
    
    
    
#models Flower
    oe_Flower<-ksr_m %>% select(Pop, Fruit_no_damage, Flower_Oenothein_B, Flower_Oenothein_A,Flower_Ox_Oenothein_A)
    plot(oe_Flower$Flower_Oenothein_A,oe_Flower$Fruit_no_damage) #Could also be qudratic
   plot(oe_Flower$Flower_Oenothein_B,oe_Flower$Fruit_no_damage) #First order only
    
    qu_oeA_Flower <- glm.nb(Fruit_no_damage ~ Flower_Oenothein_A + I(Flower_Oenothein_A^2),data=oe_Flower)
    stepAIC(qu_oeA_Flower,direction="both") #Both kept
    qu_A_Flower <- glm.nb(Fruit_no_damage ~ Flower_Oenothein_A + I(Flower_Oenothein_A^2),data=oe_Flower)
    Anova(qu_A_Flower,type = 3) # OeA significant
    
    plot9b<-visreg(qu_A_Flower, "Flower_Oenothein_A", scale="response", partial=TRUE, gg=TRUE, line=list(col="black")) +
      geom_point(size=1)+ scale_y_continuous(name="Seed Number")+
      scale_x_continuous(name="Flower Oenothein A (mg/g)")+ theme_classic()
    plot9b <- plot9b +   theme(axis.text.x = element_text(size=13, face="bold"),
                               axis.text.y = element_text(size=13,face="bold"),
                               axis.title.x = element_text(color="black", size=12, vjust = 0, face="bold"),
                               axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
    plot9b
    #ggsave("Single_fig/9b_Flower_oe.pdf",width=7,height=6,units="in") #missing out outlier
    
    #Peak Numbers
    plot8_peak <- visreg(qu_A_Flower, "Flower_Oenothein_A", scale="response", partial=TRUE)
    maxseed <- max(plot8_peak$fit$visregFit)
    max_all <- plot8_peak$fit %>% filter(visregFit==maxseed)
    max_all[1,1]
    
#models Fruit
    oe_Fruit<-ksr_m %>% select(Pop, Fruit_no_damage, Fruit_Oenothein_B, Fruit_Oenothein_A,Fruit_Ox_Oenothein_A)
    plot(oe_Fruit$Fruit_Oenothein_B,oe_Fruit$Fruit_no_damage) #First order only
    plot(oe_Fruit$Fruit_Oenothein_A,oe_Fruit$Fruit_no_damage)
    
    qu_oeA_Fruit2 <- glm.nb(Fruit_no_damage ~ Fruit_Oenothein_A ,data=oe_Fruit)
    qu_oeA_Fruit2
        Anova(qu_oeA_Fruit2,type = 3) # OeA significant

    plot9c<-visreg(qu_oeA_Fruit2, "Fruit_Oenothein_A", scale="response", partial=TRUE, gg=TRUE, line=list(col="black")) +
      geom_point(size=1)+ scale_y_continuous(name="Seed Number")+
      scale_x_continuous(name="Fruit Oenothein A (mg/g)", breaks=c(60,100,140,180))+ theme_classic()
    plot9c <- plot9c +   theme(axis.text.x = element_text(size=13, face="bold"),
                               axis.text.y = element_text(size=13,face="bold"),
                               axis.title.x = element_text(color="black", size=12, vjust = 0, face="bold"),
                               axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
    plot9c
    #ggsave("Single_fig/9c_Fruit_oe.pdf",width=7,height=6,units="in") #missing out outlier
    
    ## Cowplot Fig5 export at 7 X 8 inches
    plot_grid(plot8b,plot8c,plot9b,plot9c,ncol = 2)
    
    ## Cowplot Fig S3 export at 4 X 8 inches
    plot_grid(plot8a,plot9a)
    

  

##################################################################################
# 10. What traits are most important in predicting seed number
    
corr_predict <- ksr_m %>% select(Flowering_Date,Bolt_Date,Growth_Rate,Num_Trichomes,Water_Content,
                                Flower_Totphe,Fruit_Totphe,Flower_Oenothein_A,Fruit_Oenothein_A,
                                Leaf_Herb_Sept, bug, S.florida, M.brevivatella)
corr_table <- rcorr(as.matrix(corr_predict)) #Remove Oenothein data, correation too high with total phenolics
r_corr_table <- as.data.frame(corr_table$r)
write.table(r_corr_table, file = "Data/corr_predictors.csv", sep = ",", row.names = T) 

# model
data_10 <- ksr_m %>% select(Fruit_no_damage,Flowering_Date,Bolt_Date,Growth_Rate,Num_Trichomes,Water_Content,
                            Flower_Totphe,Fruit_Totphe)
#data_10 <- as.data.frame(na.omit(data_10))

data_10 <- na.omit(data_10)

qu_10 <- glm.nb(Fruit_no_damage ~ Flowering_Date + Bolt_Date + Growth_Rate + Num_Trichomes + Water_Content +
                    Flower_Totphe + Fruit_Totphe + I(Flowering_Date^2) + I(Bolt_Date^2) +
                    I(Num_Trichomes^2) + I(Water_Content^2) + I(Flower_Totphe^2) + I(Fruit_Totphe^2), 
                    data=data_10)
stepAIC(qu_10,direction="both") # 

qu_all <- glm.nb(Fruit_no_damage ~ Flowering_Date + Bolt_Date + Growth_Rate + 
                   Num_Trichomes + Fruit_Totphe + I(Flowering_Date^2) + 
                   I(Growth_Rate^2) + I(Num_Trichomes^2), data=data_10)
qu_all
  Anova(qu_all,type=3) # both highly significant


    
    
    
    
    
    
    
    