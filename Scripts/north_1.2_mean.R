##################################################################################
## Analyses lat, climate, and mean trait data predicting seed number
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
library(RColorBrewer)

#import data
ksr_m <- read.csv("Data/ksr_m.csv", header=T) # Imports individual dataset
attach(ksr_m)


##################################################################################
# What geographic/environmental traits predict plant fitness?
##################################################################################


#1. Does geography predict success?
  plot(Latitude,Seeds) #possible quadratic pattern
  plot(Longitude,Seeds) #possible quadratic pattern
  qu_geo <- glm.nb (Seeds ~ Latitude + Longitude + I(Latitude^2) + I(Longitude^2), data=ksr_m) #run glm
  stepAIC(qu_geo,direction="both") # lowest AIC is Lat and Lat^2
  qu_lat <- lm (Seeds ~ Latitude + I(Latitude^2), data=ksr_m) #run reduced glm
  summary(qu_lat)
  Anova(qu_lat,type=2) # both highly significant
  
  #Get peak of ggplot regression line
  plot1_peak <- visreg(qu_lat, "Latitude", scale="response", partial=TRUE)
  maxseed <- max(plot1_peak$fit$visregFit)
  max_all <- plot1_peak$fit %>% filter(visregFit==maxseed)
  max_lat <- max_all[1,1]
  max_lat - 44.026479 #degree distance from peak to common garden
  
  #Make latitude plot predicting seed number
  plot1<-visreg(qu_lat, "Latitude", scale="response", partial=TRUE, gg=TRUE, line=list(col="black")) +
    geom_point(size=1)+ scale_y_continuous(name="Seed Number",limits=c(-10000,100000))+ 
    scale_x_continuous(name="Latitude (°N)")+ theme_classic()
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
#2. Does distance to common garden predict Seeds?
  plot(Distance,Seeds)
  qu_dist <- glm.nb (Seeds ~ Distance) #run glm
  qu_dist
  summary(qu_dist)
  Anova(qu_dist,type=2)
  
  #Make distance plot predicting seed number
  plot2<- visreg(qu_dist, "Distance", scale="response", partial=TRUE, gg=TRUE, line=list(col="black")) +
    geom_point(size=1)+ scale_y_continuous(name="Seed Number",limits=c(-10000,100000))+ 
    scale_x_continuous(name="Distance (km)")+ theme_classic()
  plot2 <- plot2 + theme(axis.text.x = element_text(size=13, face="bold"),
                         axis.text.y = element_text(size=13,face="bold"),
                         axis.title.x = element_text(color="black", size=15, vjust = 0, face="bold"),
                         axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
  plot2
  #ggsave("Single_fig/2.Diatance.pdf", width = 7, height = 6, units = "in")

##################################################################################  
#3. What environmental variables at each location best predicts success?
  plot(MAT,Seeds) # all could have quadratic components
  plot(MSP,Seeds)
  plot(CMD,Seeds)
  plot(RH,Seeds)
  qu_env <- glm.nb(Seeds ~ MAT + MSP + CMD + RH + I(MAT^2) + I(MSP^2) + I(CMD^2) + I(RH^2), data=ksr_m) #run glm
  stepAIC(qu_env,direction="both") # Keep MAT and RH
  qu_MAT_RH <- glm.nb(Seeds ~ MAT + RH + I(MAT^2) + I(RH^2), data=ksr_m) #run reduced glm
  summary(qu_MAT_RH)
  Anova(qu_MAT_RH,type=2) #MAT significant, RH marginally significant
  
  #Get peak of MAT ggplot regression line
  plot3_peak <- visreg(qu_MAT_RH, "MAT", scale="response", partial=TRUE)
  maxseed <- max(plot3_peak$fit$visregFit)
  max_all <- plot3_peak$fit %>% filter(visregFit==maxseed)
  max_MAT <- max_all[1,1]
  max_MAT - 7.9 #Distance from peak MAT to MAT at common garden
  
  #Make MAT plot predicting seed number
  plot3A <-visreg(qu_MAT_RH, "MAT", scale="response", partial=TRUE, gg=TRUE, line=list(col="black")) +
    geom_point(size=1)+ scale_x_continuous(name="Mean Annual Temperature (°C)")+
    scale_y_continuous(name="Seed Number",limits=c(-10000,100000))+ theme_classic()
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
  max_RH - 69 #Distance from peak RH to RH at common garden
  
  #Make RH plot prediting seed number 
  plot3B <-visreg(qu_MAT_RH, "RH", scale="response", partial=TRUE, gg=TRUE, line=list(col="black")) +
    geom_point(size=1)+ scale_x_continuous(name="Relative Huminidty (%)")+
    scale_y_continuous(name="Seed Number",limits=c(-10000,100000))+ theme_classic()
  plot3B <- plot3B + theme(axis.text.x = element_text(size=13, face="bold"),
                           axis.text.y = element_text(size=13,face="bold"),
                           axis.title.x = element_text(color="black", size=15, vjust = 0, face="bold"),
                           axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))+
    geom_vline(xintercept= max_RH)+
    geom_vline(xintercept= 69,linetype="dashed")
  plot3B
  #ggsave("Single_fig/3B.RH.pdf", width = 7, height = 6, units = "in")
  
  
##################################################################################
#4. What env distance (common garden env – genotype’s env) best predicts success?
  plot(MAT_Distance,Seeds) # all could have quadratic components
  plot(MSP_Distance,Seeds)
  plot(CMD_Distance,Seeds)
  plot(RH_Distance,Seeds)
  qu_envdist <- glm.nb(Seeds ~ MAT_Distance + MSP_Distance + CMD_Distance + RH_Distance +
                     I(MAT_Distance^2) + I(MSP_Distance^2) + I(CMD_Distance^2) + I(RH_Distance^2), data=ksr_m) #run glm
  stepAIC(qu_envdist,direction="both") #MSP_Distance + MAT_Distance^2 dplyr::selected
  qu_MAT_MSP_D <- glm.nb(Seeds ~ MSP_Distance + I(MAT_Distance^2)) # Run reduced model
  summary(qu_MAT_MSP_D)
  Anova(qu_MAT_MSP_D,type=2) # MSP distance marginally significant, 

  #Mat distance alone
  qu_MAT_D<- glm.nb(Seeds ~ MAT_Distance + I(MAT_Distance^2), data=ksr_m)
  stepAIC(qu_MAT_D,direction="both") #Keep quadratic
  summary(qu_MAT_D)
  Anova(qu_MAT_D,type=2)
  
  
  #Get peak of RH ggplot regression line
  plot4_peak <- visreg(qu_MAT_D, "MAT_Distance", scale="response", partial=TRUE)
  maxseed <- max(plot4_peak$fit$visregFit)
  max_all <- plot4_peak$fit %>% filter(visregFit==maxseed)
  max_MATd <- max_all[1,1]
  max_MATd 
  
  #Make plot of MAT_distance predicting seed number
  plot4 <-visreg(qu_MAT_D, "MAT_Distance", scale="response", partial=TRUE, gg=TRUE, line=list(col="black")) +
    geom_point(size=1)+ scale_x_continuous(name="Temperature Distance (°C)")+
    scale_y_continuous(name="Seed Number",limits=c(0,100000))+ theme_classic()
  plot4 <- plot4 + theme(axis.text.x = element_text(size=13, face="bold"),
                         axis.text.y = element_text(size=13,face="bold"),
                         axis.title.x = element_text(color="black", size=15, vjust = 0, face="bold"),
                         axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))+
    geom_vline(xintercept= max_MATd)+
    geom_vline(xintercept= 0,linetype="dashed")
  plot4
  
  ggsave("Single_fig/4.MAT_distance.pdf", width = 7, height = 6, units = "in")
  

  
## Cowplot export at 7 X 9 inches
  plot_grid(plot1,plot2,plot3A,plot3B,ncol = 2)
  
  
  
  
##################################################################################
  # What plant traits predict plant fitness?
##################################################################################
  
##################################################################################
#5. Does herb impact predict success?

  #a. Do leaf herbiovry and xylem feeders impact seed number?
  leaf_bug <- ksr_m %>% dplyr::select(Pop,Seeds,Leaf_Herb_Sept,bug) #subset data
  leaf_bug <- na.omit(leaf_bug) #remove NA rows
  cor(leaf_bug$Leaf_Herb_Sept,leaf_bug$bug) #not correlated
  
  #Test leaf herbivory bug (P. spumarius) models
  plot(Leaf_Herb_Sept,Seeds) # all could have quadratic components
  plot(bug,Seeds)
  qu_leaf_bug <- glm.nb(Seeds ~ Leaf_Herb_Sept + bug + I(Leaf_Herb_Sept^2) + I(bug^2), data=ksr_m) #run glm
  stepAIC(qu_leaf_bug,direction="both") # bug only dplyr::selected
  qu_bug <- glm.nb(Seeds ~ bug + I(bug^2), data=ksr_m) #run reduced glm
  summary(qu_bug)
  Anova(qu_bug,type=2) #Bug and Bug^2 highly significant
  
 #Plot bug predicting latitude
  plot5a <- visreg(qu_bug, "bug", scale="response", partial=TRUE, gg=TRUE, line=list(col="black")) +
    geom_point(size=1)+ scale_x_continuous(name="P. spumarius Number")+
    scale_y_continuous(name="Seed Number",limits=c(0,105000))+ theme_classic()
  plot5a <- plot5a + theme(axis.text.x = element_text(size=13, face="bold"),
                         axis.text.y = element_text(size=13,face="bold"),
                         axis.title.x = element_text(color="black", size=12, vjust = 0, face="bold.italic"),
                         axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
  plot5a
  #ggsave("Single_fig/5A.bug.pdf", width = 7, height = 6, units = "in")
  
  #c. Does seed predation impact seed number?
  sf_mb <- ksr_m %>% dplyr::select(Pop,Seeds, S.florida, M.brevivatella) #subset data
  sf_mb <- na.omit(sf_mb) #remove NA rows
  cor(sf_mb$S.florida,sf_mb$M.brevivatella) #not correlated
  
  #Test S.florida, M.brevivatella models
  plot(S.florida,Seeds) # No evidence of quadratic model
  plot(M.brevivatella,Seeds) # No evidence of quadratic model
  qu_sf_mb <- glm.nb(Seeds ~ S.florida + M.brevivatella, data=ksr_m) #run model
  stepAIC(qu_sf_mb,direction="both") # Both seed predators dplyr::selected
  summary(qu_sf_mb)
  Anova(qu_sf_mb,type=2) #S.florida significant, M.brevivitella marginally significant
  qu_sf <- glm.nb(Seeds ~ S.florida, data=ksr_m) 
  
  #Plot S. florida against seed number
  plot5b <- visreg(qu_sf, "S.florida", scale="response", partial=TRUE, gg=TRUE, line=list(col="black")) +
    geom_point(size=1)+ scale_x_continuous(name="S. florida Damanged Fruits")+
    scale_y_continuous(name="Seed Number",limits=c(0,100000))+ theme_classic()
  plot5b <- plot5b + theme(axis.text.x = element_text(size=13, face="bold"),
                           axis.text.y = element_text(size=13,face="bold"),
                           axis.title.x = element_text(color="black", size=12, vjust = 0, face="bold.italic"),
                           axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
  plot5b
  #ggsave("Single_fig/5B.sf.pdf", width = 7, height = 6, units = "in")
  
  #Plot M.brevivitella against seed number
  plot5c <- visreg(qu_sf_mb, "M.brevivatella", scale="response", partial=TRUE, gg=TRUE, line=list(col="black")) +
    geom_point(size=1)+ scale_x_continuous(name="M. brevivitella Damanged Fruits")+
    scale_y_continuous(name="Seed Number",limits=c(0,100000))+ theme_classic()
  plot5c <- plot5c + theme(axis.text.x = element_text(size=13, face="bold"),
                           axis.text.y = element_text(size=13,face="bold"),
                           axis.title.x = element_text(color="black", size=12, vjust = 0, face="bold.italic"),
                           axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
  plot5c
  #ggsave("Single_fig/5C.mb.pdf", width = 7, height = 6, units = "in")

  ## Cowplot export at 4 X 11 inches
  plot_grid(plot5a,plot5b,plot5c,ncol = 3)
  


##################################################################################
#6. Does phenology predict success?
  pheno <- ksr_m %>% dplyr::select(Pop, Seeds, Flowering_Date, Bolt_Date, Growth_Rate) #subset data
  pheno <- na.omit(pheno) #remove NA rows
  pheno_matrix <- as.matrix(pheno)
  rcorr(pheno_matrix) #correlation r<|0.45| for all
  
  
  plot(pheno$Flowering_Date,pheno$Seeds)  # could have quadratic components
  plot(pheno$Bolt_Date,pheno$Seeds) # linear only
  plot(pheno$Growth_Rate,pheno$Seeds) # could have quadratic components
  
#Test phenology models
  qu_pheno <- glm.nb(Seeds ~ Flowering_Date + I(Flowering_Date^2) + Bolt_Date
                       + Growth_Rate + I(Growth_Rate^2), data=pheno)
  stepAIC(qu_pheno,direction="both") # AIC scores lower without Bolt^2
  qu_pheno_2 <- glm.nb(Seeds ~ Flowering_Date + I(Flowering_Date^2) + Bolt_Date 
                       + Growth_Rate + I(Growth_Rate^2), data=pheno)
  summary(qu_pheno_2)
  Anova(qu_pheno_2,type=2) 
  
#Graphs
  #Flowering Date
  plot6a <- visreg(qu_pheno_2, "Flowering_Date", scale="response", partial=TRUE, gg=TRUE , line=list(col="black")) +
  geom_point(size=1)+ scale_x_continuous(name="Flowering Date",limits = c(150,300))+
  scale_y_continuous(name="Seed Number",limits=c(0,100000))+ theme_classic()
  plot6a <- plot6a + theme(axis.text.x = element_text(size=13, face="bold"),
                             axis.text.y = element_text(size=13,face="bold"),
                             axis.title.x = element_text(color="black", size=12, vjust = 0, face="bold"),
                             axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
  plot6a
  #ggsave("Single_fig/6a.Flowering.pdf", width = 7, height = 6, units = "in")
#Bolt Date
  plot6b <- visreg(qu_pheno_2, "Bolt_Date", scale="response", partial=TRUE, gg=TRUE, line=list(col="black")) +
  geom_point(size=1)+ scale_x_continuous(name="Bolt Date")+
  scale_y_continuous(name="Seed Number",limits=c(0,100000))+ theme_classic()
  plot6b <- plot6b + theme(axis.text.x = element_text(size=13, face="bold"),
                         axis.text.y = element_text(size=13,face="bold"),
                         axis.title.x = element_text(color="black", size=12, vjust = 0, face="bold"),
                         axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
  plot6b
  #ggsave("Single_fig/6b.bolt.pdf", width = 7, height = 6, units = "in")
  
#Growth Rate
  plot6c <- visreg(qu_pheno_2, "Growth_Rate", scale="response", partial=TRUE, gg=TRUE, line=list(col="black")) +
  geom_point(size=1)+ scale_x_continuous(name="Growth Rate")+
  scale_y_continuous(name="Seed Number",limits=c(0,100000))+ theme_classic()
  plot6c <- plot6c + theme(axis.text.x = element_text(size=13, face="bold"),
                         axis.text.y = element_text(size=13,face="bold"),
                         axis.title.x = element_text(color="black", size=12, vjust = 0, face="bold"),
                         axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
  plot6c   
  #ggsave("Single_fig/6c.Growth_rate.pdf", width = 7, height = 6, units = "in")
  
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
  morpho <- ksr_m %>% dplyr::select(Pop, Seeds, SLA, Water_Content, Leaf_Toughness, Num_Trichomes) #subset data
  morpho <- na.omit(morpho) #remove NA rows
  morpho_matrix <- as.matrix(morpho)
  rcorr(morpho_matrix) #Trichomes highl correalted with leaf thoughness. Keep trichomes. Rest cor <0.4
  morpho <- ksr_m %>% dplyr::select(Pop, Seeds, SLA, Water_Content, Num_Trichomes) #subset data
  plot(morpho$SLA,morpho$Seeds) #All could have quadratic relationship
  plot(morpho$Water_Content,morpho$Seeds)
  plot(morpho$Num_Trichomes,morpho$Seeds)
  
  #Test Morphology Models
  qu_morpho <- glm.nb(Seeds ~ SLA + I(SLA^2) + Water_Content + I(Water_Content^2) + 
                        Num_Trichomes + I(Num_Trichomes^2), data=morpho) # run glm
  stepAIC(qu_morpho,direction="both") # AIC scores lower without I(SLA^2)
  qu_morpho_2 <- glm.nb(Seeds ~ SLA + Water_Content + I(Water_Content^2) + 
                          Num_Trichomes + I(Num_Trichomes^2), data=morpho) #re-run glm
  qu_morpho_2
  summary(qu_morpho_2)
  Anova(qu_morpho_2 ,type=2) #SLA not significant

  #Plot Trichomes
  plot7a <- visreg(qu_morpho_2, "Num_Trichomes", scale="response", partial=TRUE, gg=TRUE, line=list(col="black")) +
    geom_point(size=1)+ scale_x_continuous(name="Trichome Number",breaks=c(50,100,150,200,250))+
    scale_y_continuous(name="Seed Number",limits=c(0,100000))+ theme_classic()
  plot7a <- plot7a + theme(axis.text.x = element_text(size=13, face="bold"),
                           axis.text.y = element_text(size=13,face="bold"),
                           axis.title.x = element_text(color="black", size=12, vjust = 0, face="bold"),
                           axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
  plot7a
  #ggsave("Single_fig/7a.Tricomes.pdf", width = 7, height = 6, units = "in")
  
  
  
  #Plot Water Content
  plot7b <- visreg(qu_morpho_2, "Water_Content", scale="response", partial=TRUE, gg=TRUE, line=list(col="black")) +
    geom_point(size=1)+ scale_x_continuous(name="% Water Content",breaks=c(65,70,75,80))+
    scale_y_continuous(name="Seed Number",limits=c(0,100000))+ theme_classic()
  plot7b <- plot7b + theme(axis.text.x = element_text(size=13, face="bold"),
                           axis.text.y = element_text(size=13,face="bold"),
                           axis.title.x = element_text(color="black", size=12, vjust = 0, face="bold"),
                           axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
  plot7b
  #ggsave("Single_fig/7b.wc.pdf", width = 7, height = 6, units = "in")
  
  
  #Plot SLA
  plot7c <- visreg(qu_morpho_2, "SLA", scale="response", partial=TRUE, gg=TRUE, line=list(col="black")) +
    geom_point(size=1)+ scale_x_continuous(name="SLA",breaks=c(40,60,80,100))+
    scale_y_continuous(name="Seed Number",limits=c(0,100000))+ theme_classic()
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
  
  
  ## Cowplot export at 10 X 5 inches
  plot_grid(plot6a,plot6b,plot6c,plot7a,plot7b,plot7c,ncol = 3)  
  
  
  ##Include herbivores and traits all in Fig 3. Export at 9 X 8 inches landsacpe
  plot_grid(plot5a,plot5b,plot5c,plot6a,plot6b,plot6c,plot7a,plot7b,plot7c,ncol = 3)  
  
  
##################################################################################
  #Chemistry Data
##################################################################################

  #Generate oxidative capacity as totphe - pH10
  ksr_m <- ksr_m %>% mutate(Leaf_Oxidative_Capacity=Leaf_Totphe-Leaf_pH_10) %>%
                  mutate(Flower_Oxidative_Capacity=Flower_Totphe-Flower_pH_10) %>%
                  mutate(Fruit_Oxidative_Capacity=Fruit_Totphe-Fruit_pH_10)

##################################################################################
#8. Does broad chemistry predict success?
  tphe <- ksr_m %>% dplyr::select(Pop, Seeds, Leaf_Totphe,Flower_Totphe,Fruit_Totphe,
                         Leaf_Oxidative_Capacity,Flower_Oxidative_Capacity,Fruit_Oxidative_Capacity) #subset data
  tphe <- na.omit(tphe) #remove NA rows, leaf separate
  tphe_matrix <- tphe %>% dplyr::select(-Pop,-Seeds) #subset data
  tphe_matrix <- as.matrix(tphe_matrix)
  rcorr(tphe_matrix) # Oxidative capacity is highly correlated to total phenolics. Remove oxidative capacity.
  
  #generate dataframes with only the correct tissue
  tphe_leaf<-ksr_m %>% dplyr::select(Pop, Seeds, Leaf_Totphe)
  tphe_leaf<- na.omit(tphe_leaf) #remove NA rows, leaf separate
  tphe_flr<-ksr_m %>% dplyr::select(Pop, Seeds, Flower_Totphe, Fruit_Totphe)
  #tphe_flr<- na.omit(tphe_flr) #remove NA rows, leaf separate
  
  plot(tphe_leaf$Leaf_Totphe,tphe_leaf$Seeds) #Could be quadratic
  plot(tphe_flr$Flower_Totphe,tphe_flr$Seeds)
  plot(tphe_flr$Fruit_Totphe,tphe_flr$Seeds)
  
  
################################
  #models for leaf phenolics
  qu_tphe_leaf2 <- glm.nb(Seeds ~ Leaf_Totphe + I(Leaf_Totphe^2), data=tphe_leaf)  # run glm
  stepAIC(qu_tphe_leaf2,direction="both") #remove Leaf^2
  qu_tphe_leaf <- glm.nb(Seeds ~ Leaf_Totphe, data=tphe_leaf) # rerun glm
  qu_tphe_leaf
  summary(qu_tphe_leaf)
  Anova(qu_tphe_leaf,type = 2) # leaf not significant
  
  #Plot Leaf Total Phenolics
  plot8a<-visreg(qu_tphe_leaf, "Leaf_Totphe", scale="response", partial=TRUE, gg=TRUE, line=list(col="black")) +
    geom_point(size=1)+ scale_y_continuous(name="Seed Number", limits=c(0,100000),
                                           breaks=c(25000,50000,75000,100000))+
    scale_x_continuous(name="Leaf Total Phenolics (mg/g)")+ theme_classic()
  plot8a <- plot8a +   theme(axis.text.x = element_text(size=13, face="bold"),
                             axis.text.y = element_text(size=13,face="bold"),
                             axis.title.x = element_text(color="black", size=12, vjust = 0, face="bold"),
                             axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
  plot8a
  #ggsave("Single_fig/8a_Leaf_totphe.pdf",width=7,height=6,units="in")

################################ 
  #models for flower phenolics
  qu_tph_fl <- glm.nb(Seeds ~ Flower_Totphe + I(Flower_Totphe^2), data=tphe_flr)  # run glm
  stepAIC(qu_tph_fl,direction="both") #remove flower_Totphe 1st order
  qu_flr_2 <- glm.nb(Seeds ~ Flower_Totphe + I(Flower_Totphe^2), data=tphe_flr) 
  summary(qu_flr_2)
  Anova(qu_flr_2 ,type = 2) 
  
  #Peak Numbers
  plot7_peak <- visreg(qu_flr_2, "Flower_Totphe", scale="response", partial=TRUE)
  maxseed <- max(plot7_peak$fit$visregFit)
  max_all <- plot7_peak$fit %>% filter(visregFit==maxseed)
  max_all[1,1]

  #Plot Flower Total Phenolics
  plot8b<-visreg(qu_flr_2, "Flower_Totphe", scale="response", partial=TRUE, gg=TRUE, line=list(col="black")) +
    geom_point(size=1)+ scale_y_continuous(name="Seed Number", limits=c(0,100000),
                                           breaks=c(25000,50000,75000,100000))+
    scale_x_continuous(name="Flower Total Phenolics (mg/g)")+ theme_classic()
                       
                       #,limits = c(20,105),breaks=c(20,40,60,80,100))
  plot8b <- plot8b +   theme(axis.text.x = element_text(size=13, face="bold"),
                             axis.text.y = element_text(size=13,face="bold"),
                             axis.title.x = element_text(color="black", size=12, vjust = 0, face="bold"),
                             axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
  plot8b
  #ggsave("Single_fig/8b_Flower_totphe.pdf",width=7,height=6,units="in")
  
################################   
  #models for fruit phenolics
  qu_tph_r <- glm.nb(Seeds ~ Fruit_Totphe + I(Fruit_Totphe^2), data=tphe_flr)  # run glm
  stepAIC(qu_tph_r,direction="both") # Keep both main effect and quadratic effect
  qu_flr_3 <- glm.nb(Seeds ~ Fruit_Totphe + I(Fruit_Totphe^2), data=tphe_flr)  
  summary(qu_flr_3)
  Anova(qu_flr_3 ,type = 2) #
  
  #Plot Fruit Total Phenolics
  plot8c<-visreg(qu_flr_3, "Fruit_Totphe", scale="response", partial=TRUE, gg=TRUE, line=list(col="black")) +
    geom_point(size=1)+ scale_y_continuous(name="Seed Number", limits=c(0,100000),
                        breaks=c(25000,50000,75000,100000))+
    scale_x_continuous(name="Fruit Total Phenolics (mg/g)",breaks=c(60,100,140,180))+ theme_classic()
  plot8c <- plot8c +   theme(axis.text.x = element_text(size=13, face="bold"),
                             axis.text.y = element_text(size=13,face="bold"),
                             axis.title.x = element_text(color="black", size=12, vjust = 0, face="bold"),
                             axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
  plot8c
  #ggsave("Single_fig/8c_Fruit_totphe.pdf",width=7,height=6,units="in")
  
  ## Cowplot export at 4 X 8 inches
  plot_grid (plot8b,plot8c,ncol = 2)  
  
  
##################################################################################
#9. Does detailed chemistry predict success? (Oe=Oenothein)
    oenothein <- ksr_m %>% dplyr::select(Pop, Seeds, Leaf_Oenothein_B, Leaf_Oenothein_A,
                           Flower_Oenothein_B, Flower_Oenothein_A,
                           Fruit_Oenothein_B, Fruit_Oenothein_A) #subset data
    oenothein.matrix <- ksr_m %>% dplyr::select(Leaf_Oenothein_B, Leaf_Oenothein_A,
                                Flower_Oenothein_B, Flower_Oenothein_A,
                                Fruit_Oenothein_B, Fruit_Oenothein_A) #subset data
    oenothein <- na.omit(oenothein) #remove NA rows
    oenothein <- na.omit(oenothein.matrix) #remove NA rows
    oenothein_matrix <- as.matrix(oenothein.matrix)
    rcorr.oe<-rcorr(oenothein_matrix) # All oenothein highly correlated. Do each tissue separately. Lump A and oxA together
        write.csv(rcorr.oe$r,"oe_rcorr.csv", row.names = TRUE)

#models for leaf oenothein A
    oe_Leaf<-ksr_m %>% dplyr::select(Pop, Seeds, Leaf_Oenothein_B, Leaf_Oenothein_A)
    plot(oe_Leaf$Leaf_Oenothein_A,oe_Leaf$Seeds) #Could be second order
    plot(oe_Leaf$Leaf_Oenothein_B,oe_Leaf$Seeds) #First order only
    
    oe_Leaf <- na.omit(oe_Leaf)
    
    qu_oeA_Leaf <- glm.nb(Seeds ~ Leaf_Oenothein_A + I(Leaf_Oenothein_A^2),data=oe_Leaf) # run glm
    stepAIC(qu_oeA_Leaf,direction="both") # Keep both
    qu_A_Leaf <- glm.nb(Seeds ~ Leaf_Oenothein_A + I(Leaf_Oenothein_A^2) ,data=oe_Leaf) 
    qu_A_Leaf
    summary(qu_A_Leaf)
    Anova(qu_A_Leaf,type = 2) # Nothing significant
    
    #Plot leaf oenothein A vs seed number
    plot9a<-visreg(qu_A_Leaf, "Leaf_Oenothein_A", scale="response", partial=TRUE, gg=TRUE, line=list(col="black")) +
      geom_point(size=1)+ scale_y_continuous(name="Seed Number", limits=c(0,100000),
                                             breaks=c(25000,50000,75000,100000))+
      scale_x_continuous(name="Leaf Oenothein A (mg/g)")+ theme_classic()
    plot9a <- plot9a +   theme(axis.text.x = element_text(size=13, face="bold"),
                               axis.text.y = element_text(size=13,face="bold"),
                               axis.title.x = element_text(color="black", size=12, vjust = 0, face="bold"),
                               axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
    plot9a
    #ggsave("Single_fig/9a_Leaf_oe.pdf",width=7,height=6,units="in")
    
    
#model for flower oenothein A
    oe_Flower<-ksr_m %>% dplyr::select(Pop, Seeds, Flower_Oenothein_B, Flower_Oenothein_A)
    plot(oe_Flower$Flower_Oenothein_A,oe_Flower$Seeds) #Could also be qudratic
   plot(oe_Flower$Flower_Oenothein_B,oe_Flower$Seeds) #First order only
    
    qu_oeA_Flower <- glm.nb(Seeds ~ Flower_Oenothein_A + I(Flower_Oenothein_A^2),data=oe_Flower)
    stepAIC(qu_oeA_Flower,direction="both") #Both kept
    qu_A_Flower <- glm.nb(Seeds ~ Flower_Oenothein_A + I(Flower_Oenothein_A^2),data=oe_Flower)
    summary(qu_A_Flower)
    Anova(qu_A_Flower,type = 2) # OeA significant
    
    #Plot flower oenothein A vs seed number
    plot9b<-visreg(qu_A_Flower, "Flower_Oenothein_A", scale="response", partial=TRUE, gg=TRUE, line=list(col="black")) +
      geom_point(size=1)+ scale_y_continuous(name="Seed Number", limits=c(0,100000),
                                             breaks=c(25000,50000,75000,100000))+
      scale_x_continuous(name="Flower Oenothein A (mg/g)")+ theme_classic()
    plot9b <- plot9b +   theme(axis.text.x = element_text(size=13, face="bold"),
                               axis.text.y = element_text(size=13,face="bold"),
                               axis.title.x = element_text(color="black", size=12, vjust = 0, face="bold"),
                               axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
    plot9b
    #ggsave("Single_fig/9b_Flower_oe.pdf",width=7,height=6,units="in") 
    
    #Peak Numbers
    plot8_peak <- visreg(qu_A_Flower, "Flower_Oenothein_A", scale="response", partial=TRUE)
    maxseed <- max(plot8_peak$fit$visregFit)
    max_all <- plot8_peak$fit %>% filter(visregFit==maxseed)
    max_all[1,1]
    
#model for fruit oenothein A
    oe_Fruit<-ksr_m %>% dplyr::select(Pop, Seeds, Fruit_Oenothein_B, Fruit_Oenothein_A)
    plot(oe_Fruit$Fruit_Oenothein_B,oe_Fruit$Seeds) #First order only
    plot(oe_Fruit$Fruit_Oenothein_A,oe_Fruit$Seeds)
    
    qu_oeA_Fruit2 <- glm.nb(Seeds ~ Fruit_Oenothein_A ,data=oe_Fruit)
    qu_oeA_Fruit2
    summary(qu_oeA_Fruit2)
        Anova(qu_oeA_Fruit2,type = 2) # OeA significant

    #Plot fruit oenothein A vs seed number
    plot9c<-visreg(qu_oeA_Fruit2, "Fruit_Oenothein_A", scale="response", partial=TRUE, gg=TRUE, line=list(col="black")) +
      geom_point(size=1)+ scale_y_continuous(name="Seed Number", limits=c(0,100000),
                                             breaks=c(25000,50000,75000,100000))+
      scale_x_continuous(name="Fruit Oenothein A (mg/g)", breaks=c(60,100,140,180))+ theme_classic()
    plot9c <- plot9c +   theme(axis.text.x = element_text(size=13, face="bold"),
                               axis.text.y = element_text(size=13,face="bold"),
                               axis.title.x = element_text(color="black", size=12, vjust = 0, face="bold"),
                               axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
    plot9c
    #ggsave("Single_fig/9c_Fruit_oe.pdf",width=7,height=6,units="in")
    
    ## Cowplot Fig5 export at 7 X 8 inches
    plot_grid(plot8b,plot8c,plot9b,plot9c,ncol = 2)
    
    ## Cowplot Fig S3 export at 4 X 8 inches
    plot_grid(plot8a,plot9a)
    

  


  
    
    
    
    
    
    
    