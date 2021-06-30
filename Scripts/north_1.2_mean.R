##################################################################################
# Analyses lat, climate, and plat data predicting seed number
## Daniel Anstett
## February 2 2021
##################################################################################

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

##################################################################################
# What geographic/environmental traits predict plant fitness?
##################################################################################


#1. Does geography predict success?
  plot(Latitude,Seeds) #possible quadratic pattern
  plot(Longitude,Seeds) #possible quadratic pattern
  qu_geo <- glm.nb (Seeds ~ Latitude + Longitude + I(Latitude^2) + I(Longitude^2), data=ksr_m)
  stepAIC(qu_geo,direction="both") # lowest AIC is Lat and Lat^2
  qu_lat <- lm (Seeds ~ Latitude + I(Latitude^2), data=ksr_m)
  Anova(qu_lat,type=3) # both highly significant

  #Make Plot
  plot1<-visreg(qu_lat, "Latitude", scale="response", partial=TRUE, gg=TRUE, line=list(col="black")) +
    geom_point(size=1)+ scale_y_continuous(name="Seed Number")+ theme_classic()
  plot1 <- plot1 + theme(axis.text.x = element_text(size=13, face="bold"),
      axis.text.y = element_text(size=13,face="bold"),
      axis.title.x = element_text(color="black", size=15, vjust = 0, face="bold"),
      axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
  plot1
ggsave("1.Lat.pdf", width = 7, height = 6, units = "in")


##################################################################################
#2. Does distance to common garden predict Seeds?
  plot(Distance,Seeds)
  qu_dist <- glm.nb (Seeds ~ Distance)
  Anova(qu_dist,type=3)
  
  #Make Plot
  plot2<- visreg(qu_dist, "Distance", scale="response", partial=TRUE, gg=TRUE, line=list(col="black")) +
    geom_point(size=1)+ scale_y_continuous(name="Seed Number")+ theme_classic()
  plot2 <- plot2 + theme(axis.text.x = element_text(size=13, face="bold"),
                         axis.text.y = element_text(size=13,face="bold"),
                         axis.title.x = element_text(color="black", size=15, vjust = 0, face="bold"),
                         axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
  plot2
  ggsave("2.Diatance.pdf", width = 7, height = 6, units = "in")

##################################################################################  
#3. What environmental variables at each location best predicts success?
  plot(MAT,Seeds) # all could have quadratic components
  plot(MSP,Seeds)
  plot(CMD,Seeds)
  plot(RH,Seeds)
  qu_env <- glm.nb(Seeds ~ MAT + MSP + CMD + RH + I(MAT^2) + I(MSP^2) + I(CMD^2) + I(RH^2), data=ksr_m)
  stepAIC(qu_env,direction="both") # Keep MAT and RH
  qu_MAT_RH <- glm.nb(Seeds ~ MAT + RH + I(MAT^2) + I(RH^2), data=ksr_m)
  Anova(qu_MAT_RH,type=3) #MAT significant, RH marginally significant
  
  #MAT
  plot3A <-visreg(qu_MAT_RH, "MAT", scale="response", partial=TRUE, gg=TRUE, line=list(col="black")) +
    geom_point(size=1)+ scale_x_continuous(name="Mean Annual Temperature (°C)")+
    scale_y_continuous(name="Seed Number")+ theme_classic()
  plot3A <- plot3A + theme(axis.text.x = element_text(size=13, face="bold"),
                         axis.text.y = element_text(size=13,face="bold"),
                         axis.title.x = element_text(color="black", size=15, vjust = 0, face="bold"),
                         axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
  plot3A
  ggsave("3A.Diatance.pdf", width = 7, height = 6, units = "in")
  
  #RH 
  visreg(qu_MAT_RH, "RH", scale="response", partial=TRUE, gg=TRUE)
  plot3B <-visreg(qu_MAT_RH, "RH", scale="response", partial=TRUE, gg=TRUE, line=list(col="black")) +
    geom_point(size=1)+ scale_x_continuous(name="Relative Huminidty (%)")+
    scale_y_continuous(name="Seed Number")+ theme_classic()
  plot3B <- plot3B + theme(axis.text.x = element_text(size=13, face="bold"),
                           axis.text.y = element_text(size=13,face="bold"),
                           axis.title.x = element_text(color="black", size=15, vjust = 0, face="bold"),
                           axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
  plot3B
  ggsave("3B.Diatance.pdf", width = 7, height = 6, units = "in")
  
  
##################################################################################
#4. What env distance (common garden env – genotype’s env) best predicts success?
  plot(MAT_Distance,Seeds) # all could have quadratic components
  plot(MSP_Distance,Seeds)
  plot(CMD_Distance,Seeds)
  plot(RH_Distance,Seeds)
  qu_envdist <- glm.nb(Seeds ~ MAT_Distance + MSP_Distance + CMD_Distance + RH_Distance +
                     I(MAT_Distance^2) + I(MSP_Distance^2) + I(CMD_Distance^2) + I(RH_Distance^2), data=ksr_m)
  stepAIC(qu_envdist,direction="both") # MSP_Distance + MAT_Distance^2 selected
  qu_MAT_MSP_D <- glm.nb(Seeds ~ MSP_Distance + I(MAT_Distance^2)) 
  Anova(qu_MAT_MSP_D,type=3) # MSP distance marginally significant, 
  visreg(qu_MAT_MSP_D, "MAT_Distance", scale="response", partial=TRUE, gg=TRUE)
  
  #Mat distance alone
  qu_MAT_D<- glm.nb(Seeds ~ MAT_Distance + I(MAT_Distance^2), data=ksr_m)
  stepAIC(qu_MAT_D,direction="both") #Keep quadratic
  Anova(qu_MAT_D,type=3) 
  #Plot
  plot4 <-visreg(qu_MAT_D, "MAT_Distance", scale="response", partial=TRUE, gg=TRUE, line=list(col="black")) +
    geom_point(size=1)+ scale_x_continuous(name="Temperature Distance (°C)")+
    scale_y_continuous(name="Seed Number")+ theme_classic()
  plot4 <- plot4 + theme(axis.text.x = element_text(size=13, face="bold"),
                         axis.text.y = element_text(size=13,face="bold"),
                         axis.title.x = element_text(color="black", size=15, vjust = 0, face="bold"),
                         axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
  plot4
  
  ggsave("4.MAT_distance.pdf", width = 7, height = 6, units = "in")
  
## Cowplot export at 11 X 7 inches
  plot_grid(plot1,plot2,plot3,plot4,ncol = 2)
  
  
  
  
##################################################################################
  # What plant traits predict plant fitness?
##################################################################################
  
##################################################################################
#5. Does herb impact predict success?
    #a. Do leaf herbiovry and xylem feeders impact seed number?
  leaf_bug <- ksr_m %>% select(Pop,Seeds, Leaf_Herb_Sept, bug) #subset data
  leaf_bug <- na.omit(leaf_bug) #remove NA rows
  cor(leaf_bug$Leaf_Herb_Sept,leaf_bug$bug) #not correlated
  
  #Test leaf bug models
  plot(Leaf_Herb_Sept,Seeds) # all could have quadratic components
  plot(bug,Seeds)
  qu_leaf_bug <- glm.nb(Seeds ~ Leaf_Herb_Sept + bug + I(Leaf_Herb_Sept^2) + I(bug^2), data=ksr_m)
  stepAIC(qu_leaf_bug,direction="both") # bug only selected
  qu_bug <- glm.nb(Seeds ~ bug + I(bug^2), data=ksr_m)
  Anova(qu_leaf_bug,type=3) #Leaf herbivory not significant
  Anova(qu_bug,type=3) #Bug and Bug^2 highly significant
 #bug not having a negative effect no seed number
  plot5a <- visreg(qu_bug, "bug", scale="response", partial=TRUE, gg=TRUE, line=list(col="black")) +
    geom_point(size=1)+ scale_x_continuous(name="Philaenus spumarius")+
    scale_y_continuous(name="Seed Number")+ theme_classic()
  plot5a <- plot5a + theme(axis.text.x = element_text(size=13, face="bold"),
                         axis.text.y = element_text(size=13,face="bold"),
                         axis.title.x = element_text(color="black", size=12, vjust = 0, face="bold.italic"),
                         axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
  plot5a
  ggsave("5A.bug.pdf", width = 7, height = 6, units = "in")
  
  #b. Does seed predation impact seed number?
  sf_mb <- ksr_m %>% select(Pop,Seeds, S.florida, M.brevivatella) #subset data
  sf_mb <- na.omit(sf_mb) #remove NA rows
  cor(sf_mb$S.florida,sf_mb$M.brevivatella) #not correlated
  
  #Test S.florida, M.brevivatella models
  plot(S.florida,Seeds) # No evidence of quadratic model
  plot(M.brevivatella,Seeds) # No evidence of quadratic model
  qu_sf_mb <- glm.nb(Seeds ~ S.florida + M.brevivatella, data=ksr_m)
  stepAIC(qu_sf_mb,direction="both") # Both seed predators selected
  Anova(qu_sf_mb,type=3) #S.florida significant, M.brevivietall marginally significant
  #plots
  #Not the most relevant explanation of the pattern
  plot5b <- visreg(qu_sf_mb, "S.florida", scale="response", partial=TRUE, gg=TRUE, line=list(col="black")) +
    geom_point(size=1)+ scale_x_continuous(name="# S. florida Damanged Fruits")+
    scale_y_continuous(name="Seed Number")+ theme_classic()
  plot5b <- plot5b + theme(axis.text.x = element_text(size=13, face="bold"),
                           axis.text.y = element_text(size=13,face="bold"),
                           axis.title.x = element_text(color="black", size=12, vjust = 0, face="bold.italic"),
                           axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
  plot5b
  ggsave("5B.sf.pdf", width = 7, height = 6, units = "in")
  
  plot5c <- visreg(qu_sf_mb, "M.brevivatella", scale="response", partial=TRUE, gg=TRUE, line=list(col="black")) +
    geom_point(size=1)+ scale_x_continuous(name="M. brevivitella Damanged Fruits")+
    scale_y_continuous(name="Seed Number")+ theme_classic()
  plot5c <- plot5c + theme(axis.text.x = element_text(size=13, face="bold"),
                           axis.text.y = element_text(size=13,face="bold"),
                           axis.title.x = element_text(color="black", size=12, vjust = 0, face="bold.italic"),
                           axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
  plot5c
  ggsave("5C.mb.pdf", width = 7, height = 6, units = "in")

  ## Cowplot export at 4 X 11 inches
  plot_grid(plot5a,plot5b,plot5c,ncol = 3)
  

##################################################################################
#6. Does phenology predict success?
  pheno <- ksr_m %>% select(Pop, Seeds, Flowering_Date, Bolt_Date, Growth_Rate) #subset data
  pheno <- na.omit(pheno) #remove NA rows
  pheno_matrix <- as.matrix(pheno)
  rcorr(pheno_matrix) #correlation r<|0.45| for all
  
  plot(pheno$Flowering_Date,pheno$Seeds) # all could have quadratic components
  plot(pheno$Bolt_Date,pheno$Seeds)
  plot(pheno$Growth_Rate,pheno$Seeds)
  
#Test phenology models
  qu_pheno <- glm.nb(Seeds ~ Flowering_Date + I(Flowering_Date^2) + Bolt_Date + I(Bolt_Date^2) 
                       + Growth_Rate + I(Growth_Rate^2), data=pheno)
  stepAIC(qu_pheno,direction="both") # AIC scores lower without Bolt^2
  qu_pheno_2 <- glm.nb(Seeds ~ Flowering_Date + I(Flowering_Date^2) + Bolt_Date 
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
  ggsave("6a.Flowering.pdf", width = 7, height = 6, units = "in")
#Bolt Date
  plot6b <- visreg(qu_pheno_2, "Bolt_Date", scale="response", partial=TRUE, gg=TRUE, line=list(col="black")) +
  geom_point(size=1)+ scale_x_continuous(name="Bolt Date")+
  scale_y_continuous(name="Seed Number")+ theme_classic()
  plot6b <- plot6b + theme(axis.text.x = element_text(size=13, face="bold"),
                         axis.text.y = element_text(size=13,face="bold"),
                         axis.title.x = element_text(color="black", size=12, vjust = 0, face="bold"),
                         axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
  plot6b
  ggsave("6b.bolt.pdf", width = 7, height = 6, units = "in")
#Growth Rate
  plot6c <- visreg(qu_pheno_2, "Growth_Rate", scale="response", partial=TRUE, gg=TRUE, line=list(col="black")) +
  geom_point(size=1)+ scale_x_continuous(name="Growth Rate")+
  scale_y_continuous(name="Seed Number")+ theme_classic()
  plot6c <- plot6c + theme(axis.text.x = element_text(size=13, face="bold"),
                         axis.text.y = element_text(size=13,face="bold"),
                         axis.title.x = element_text(color="black", size=12, vjust = 0, face="bold"),
                         axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
  plot6c   
  ggsave("6c.Growth_rate.pdf", width = 7, height = 6, units = "in")
    
## Cowplot export at 4 X 11 inches
  plot_grid(plot6a,plot6b,plot6c,ncol = 3)  
    
    
    
##################################################################################
#7. Does morphology predict success?
  morpho <- ksr_m %>% select(Pop, Seeds, SLA, Water_Content, Leaf_Toughness, Num_Trichomes) #subset data
  morpho <- na.omit(morpho) #remove NA rows
  morpho_matrix <- as.matrix(morpho)
  rcorr(morpho_matrix) #Trichomes highl correalted with leaf thoughness. Keep trichomes. Rest cor <0.4
  morpho <- ksr_m %>% select(Pop, Seeds, SLA, Water_Content, Num_Trichomes) #subset data
  plot(morpho$SLA,morpho$Seeds) #All could have quadratic relationship
  plot(morpho$Water_Content,morpho$Seeds)
  plot(morpho$Num_Trichomes,morpho$Seeds)
  
  #Test Morphology Models
  qu_morpho <- glm.nb(Seeds ~ SLA + I(SLA^2) + Water_Content + I(Water_Content^2) + 
                        Num_Trichomes + I(Num_Trichomes^2), data=morpho)
  stepAIC(qu_morpho,direction="both") # AIC scores lower without I(SLA^2)
  qu_morpho_2 <- glm.nb(Seeds ~ SLA + Water_Content + I(Water_Content^2) + 
                          Num_Trichomes + I(Num_Trichomes^2), data=morpho)
  Anova(qu_morpho_2 ,type=3) #SLA not significant

  #Trichomes
  plot7a <- visreg(qu_morpho_2, "Num_Trichomes", scale="response", partial=TRUE, gg=TRUE, line=list(col="black")) +
    geom_point(size=1)+ scale_x_continuous(name="Trichome Number")+
    scale_y_continuous(name="Seed Number")+ theme_classic()
  plot7a <- plot7a + theme(axis.text.x = element_text(size=13, face="bold"),
                           axis.text.y = element_text(size=13,face="bold"),
                           axis.title.x = element_text(color="black", size=12, vjust = 0, face="bold"),
                           axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
  plot7a
  ggsave("7a.Tricomes.pdf", width = 7, height = 6, units = "in")
  
  
  
  #Water Content
  plot7b <- visreg(qu_morpho_2, "Water_Content", scale="response", partial=TRUE, gg=TRUE, line=list(col="black")) +
    geom_point(size=1)+ scale_x_continuous(name="% Water Content")+
    scale_y_continuous(name="Seed Number")+ theme_classic()
  plot7b <- plot7b + theme(axis.text.x = element_text(size=13, face="bold"),
                           axis.text.y = element_text(size=13,face="bold"),
                           axis.title.x = element_text(color="black", size=12, vjust = 0, face="bold"),
                           axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
  plot7b
  ggsave("7b.wc.pdf", width = 7, height = 6, units = "in")
  
  
  #SLA
  plot7c <- visreg(qu_morpho_2, "SLA", scale="response", partial=TRUE, gg=TRUE, line=list(col="black")) +
    geom_point(size=1)+ scale_x_continuous(name="SLA")+
    scale_y_continuous(name="Seed Number")+ theme_classic()
  plot7c <- plot7c + theme(axis.text.x = element_text(size=13, face="bold"),
                           axis.text.y = element_text(size=13,face="bold"),
                           axis.title.x = element_text(color="black", size=12, vjust = 0, face="bold"),
                           axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
  plot7c
  ggsave("7c.SLA.pdf", width = 7, height = 6, units = "in")
  
  
  ## Cowplot export at 4 X 11 inches
  plot_grid(plot7a,plot7b,plot7c,ncol = 3)  
  
##################################################################################
  #Chemistry Data
##################################################################################

  #Generate oxidative capacity as totphe - pH10
  ksr_m <- ksr_m %>% mutate(Leaf_Oxidative_Capacity=Leaf_Totphe-Leaf_pH_10) %>%
                  mutate(Flower_Oxidative_Capacity=Flower_Totphe-Flower_pH_10) %>%
                  mutate(Fruit_Oxidative_Capacity=Fruit_Totphe-Fruit_pH_10)

##################################################################################
#8. Does broad chemistry predict success?
  tphe <- ksr_m %>% select(Pop, Seeds, Leaf_Totphe,Flower_Totphe,Fruit_Totphe,
                         Leaf_Oxidative_Capacity,Flower_Oxidative_Capacity,Fruit_Oxidative_Capacity) #subset data
  tphe <- na.omit(tphe) #remove NA rows, leaf separate
  tphe_matrix <- tphe %>% select(-Pop,-Seeds) #subset data
  tphe_matrix <- as.matrix(tphe_matrix)
  rcorr(tphe_matrix) # Oxidative capacity is highly correlated to total phenolics. Remove oxidative capacity.
  
  #generate dataframes with only the correct tissue
  tphe_leaf<-ksr_m %>% select(Pop, Seeds, Leaf_Totphe)
  tphe_leaf<- na.omit(tphe_leaf) #remove NA rows, leaf separate
  tphe_flr<-ksr_m %>% select(Pop, Seeds, Flower_Totphe, Fruit_Totphe)
  tphe_flr<- na.omit(tphe_flr) #remove NA rows, leaf separate
  
  plot(tphe_leaf$Leaf_Totphe,tphe_leaf$Seeds) #Could be quadratic
  plot(tphe_flr$Flower_Totphe,tphe_flr$Seeds)
  plot(tphe_flr$Fruit_Totphe,tphe_flr$Seeds)
  
  #models for Leaf
  qu_tphe_leaf <- glm.nb(Seeds ~ Leaf_Totphe + I(Leaf_Totphe^2), data=tphe_leaf)
  stepAIC(qu_tphe_leaf,direction="both") #keep leaf and Leaf^2
  Anova(qu_tphe_leaf,type = 3) # leaf not significant
  
  #Leaf Total Phenolics
  plot8a<-visreg(qu_tphe_leaf, "Leaf_Totphe", scale="response", partial=TRUE, gg=TRUE, line=list(col="black")) +
    geom_point(size=1)+ scale_y_continuous(name="Seed Number", limits=c(0,150000),
                                           breaks=c(25000,50000,75000,100000,125000,150000))+
    scale_x_continuous(name="Leaf Total Phenolics (mg/g)")+ theme_classic()
  plot8a <- plot8a +   theme(axis.text.x = element_text(size=13, face="bold"),
                             axis.text.y = element_text(size=13,face="bold"),
                             axis.title.x = element_text(color="black", size=12, vjust = 0, face="bold"),
                             axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
  plot8a
  ggsave("8a_Leaf_totphe.pdf",width=7,height=6,units="in")
  
  #models for Flower & Fruit 
  qu_tph_flr <- glm.nb(Seeds ~ Flower_Totphe + I(Flower_Totphe^2) + 
                         Fruit_Totphe + I(Fruit_Totphe^2), data=tphe_flr) 
  stepAIC(qu_tph_flr,direction="both") #remove flower_Totphe 1st order
  qu_flr_2 <- glm.nb(Seeds ~ I(Flower_Totphe^2) + Fruit_Totphe + I(Fruit_Totphe^2), data=tphe_flr) 
  Anova(qu_flr_2 ,type = 3) #Remove qudratic effect of Fruit_Totphe


  #Flower Total Phenolics
  plot8b<-visreg(qu_flr_2, "Flower_Totphe", scale="response", partial=TRUE, gg=TRUE, line=list(col="black")) +
    geom_point(size=1)+ scale_y_continuous(name="Seed Number", limits=c(0,150000),
                                           breaks=c(25000,50000,75000,100000,125000,150000))+
    scale_x_continuous(name="Flower Total Phenolics (mg/g)")+ theme_classic()
  plot8b <- plot8b +   theme(axis.text.x = element_text(size=13, face="bold"),
                             axis.text.y = element_text(size=13,face="bold"),
                             axis.title.x = element_text(color="black", size=12, vjust = 0, face="bold"),
                             axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
  plot8b
  ggsave("8b_Flower_totphe.pdf",width=7,height=6,units="in")
  
  #Fruit Total Phenolics
  plot8c<-visreg(qu_flr_2, "Fruit_Totphe", scale="response", partial=TRUE, gg=TRUE, line=list(col="black")) +
    geom_point(size=1)+ scale_y_continuous(name="Seed Number", limits=c(0,150000),
                        breaks=c(25000,50000,75000,100000,125000,150000))+
    scale_x_continuous(name="Fruit Total Phenolics (mg/g)")+ theme_classic()
  plot8c <- plot8c +   theme(axis.text.x = element_text(size=13, face="bold"),
                             axis.text.y = element_text(size=13,face="bold"),
                             axis.title.x = element_text(color="black", size=12, vjust = 0, face="bold"),
                             axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
  plot8c
  ggsave("8c_Fruit_totphe.pdf",width=7,height=6,units="in")
  
  ## Cowplot export at 4 X 11 inches
  plot_grid(plot8a,plot8b,plot8c,ncol = 3)  
  
  
##################################################################################
#9. Does detailed chemistry predict success? (Oe=Oenothein)
    oenothein <- ksr_m %>% select(Pop, Seeds, Leaf_Oenothein_B, Leaf_Oenothein_A, Leaf_Ox_Oenothein_A,
                           Flower_Oenothein_B, Flower_Oenothein_A, Flower_Ox_Oenothein_A,
                           Fruit_Oenothein_B, Fruit_Oenothein_A,Fruit_Ox_Oenothein_A) #subset data
    oenothein.matrix <- ksr_m %>% select(Leaf_Oenothein_B, Leaf_Oenothein_A, Leaf_Ox_Oenothein_A,
                                Flower_Oenothein_B, Flower_Oenothein_A, Flower_Ox_Oenothein_A,
                                Fruit_Oenothein_B, Fruit_Oenothein_A,Fruit_Ox_Oenothein_A) #subset data
    oenothein <- na.omit(oenothein) #remove NA rows
    oenothein <- na.omit(oenothein.matrix) #remove NA rows
    oenothein_matrix <- as.matrix(oenothein.matrix)
    rcorr.oe<-rcorr(oenothein_matrix) # All oenothein highly correlated. Do each tissue separately. Lump A and oxA together
        write.csv(rcorr.oe$r,"oe_rcorr.csv", row.names = TRUE)

#models Leaf
    oe_Leaf<-ksr_m %>% select(Pop, Seeds, Leaf_Oenothein_B, Leaf_Oenothein_A,Leaf_Ox_Oenothein_A)
    plot(oe_Leaf$Leaf_Oenothein_A,oe_Leaf$Seeds) #Could be second order
    plot(oe_Leaf$Leaf_Oenothein_B,oe_Leaf$Seeds) #First order only
    plot(oe_Leaf$Leaf_Ox_Oenothein_A,oe_Leaf$Seeds) #Could be second order
    
    qu_oeA_Leaf <- glm.nb(Seeds ~ Leaf_Oenothein_A + I(Leaf_Oenothein_A^2) +
                                Leaf_Ox_Oenothein_A + I(Leaf_Ox_Oenothein_A^2),data=oe_Leaf)
    stepAIC(qu_oeA_Leaf,direction="both") # Quadratic effects removed
    qu_A_Leaf <- glm.nb(Seeds ~ Leaf_Oenothein_A + Leaf_Ox_Oenothein_A ,data=oe_Leaf)
    Anova(qu_A_Leaf,type = 3) # Nothing significant
    
    plot9a<-visreg(qu_A_Leaf, "Leaf_Oenothein_A", scale="response", partial=TRUE, gg=TRUE, line=list(col="black")) +
      geom_point(size=1)+ scale_y_continuous(name="Seed Number", limits=c(0,100000),
                                             breaks=c(25000,50000,75000,100000))+
      scale_x_continuous(name="Leaf Oenothein A (mg/g)")+ theme_classic()
    plot9a <- plot9a +   theme(axis.text.x = element_text(size=13, face="bold"),
                               axis.text.y = element_text(size=13,face="bold"),
                               axis.title.x = element_text(color="black", size=12, vjust = 0, face="bold"),
                               axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
    plot9a
    ggsave("9a_Leaf_oe.pdf",width=7,height=6,units="in")
    
    
    
#models Flower
    oe_Flower<-ksr_m %>% select(Pop, Seeds, Flower_Oenothein_B, Flower_Oenothein_A,Flower_Ox_Oenothein_A)
    plot(oe_Flower$Flower_Oenothein_A,oe_Flower$Seeds) #Could also be qudratic
   plot(oe_Flower$Flower_Oenothein_B,oe_Flower$Seeds) #First order only
    plot(oe_Flower$Flower_Ox_Oenothein_A,oe_Flower$Seeds) #First order only
    
    qu_oeA_Flower <- glm.nb(Seeds ~ Flower_Oenothein_A + I(Flower_Oenothein_A^2) + Flower_Oenothein_B 
                            + Flower_Ox_Oenothein_A,data=oe_Flower)
    stepAIC(qu_oeA_Flower,direction="both") #ox oenothein A removed
    qu_A_Flower <- glm.nb(Seeds ~ Flower_Oenothein_A + I(Flower_Oenothein_A^2),data=oe_Flower)
    Anova(qu_A_Flower,type = 3) # OeA significant
    
    plot9b<-visreg(qu_A_Flower, "Flower_Oenothein_A", scale="response", partial=TRUE, gg=TRUE, line=list(col="black")) +
      geom_point(size=1)+ scale_y_continuous(name="Seed Number", limits=c(0,100000),
                                             breaks=c(25000,50000,75000,100000))+
      scale_x_continuous(name="Flower Oenothein A (mg/g)")+ theme_classic()
    plot9b <- plot9b +   theme(axis.text.x = element_text(size=13, face="bold"),
                               axis.text.y = element_text(size=13,face="bold"),
                               axis.title.x = element_text(color="black", size=12, vjust = 0, face="bold"),
                               axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
    plot9b
    ggsave("9b_Flower_oe.pdf",width=7,height=6,units="in") #missing out outlier
    
    


#models Fruit
    oe_Fruit<-ksr_m %>% select(Pop, Seeds, Fruit_Oenothein_B, Fruit_Oenothein_A,Fruit_Ox_Oenothein_A)
    plot(oe_Fruit$Fruit_Oenothein_B,oe_Fruit$Seeds) #First order only
    plot(oe_Fruit$Fruit_Oenothein_A,oe_Fruit$Seeds)
    plot(oe_Fruit$Fruit_Ox_Oenothein_A,oe_Fruit$Seeds)
    
    qu_oeA_Fruit <- glm.nb(Seeds ~ Fruit_Oenothein_A + Fruit_Oenothein_B + 
                             Fruit_Ox_Oenothein_A,data=oe_Fruit)
    stepAIC(qu_oeA_Fruit,direction="both")
    qu_oeA_Fruit2 <- glm.nb(Seeds ~ Fruit_Oenothein_A + Fruit_Ox_Oenothein_A,data=oe_Fruit)
        Anova(qu_oeA_Fruit2,type = 3) # OeA significant

    plot9c<-visreg(qu_oeA_Fruit2, "Fruit_Oenothein_A", scale="response", partial=TRUE, gg=TRUE, line=list(col="black")) +
      geom_point(size=1)+ scale_y_continuous(name="Seed Number", limits=c(0,100000),
                                             breaks=c(25000,50000,75000,100000))+
      scale_x_continuous(name="Fruit Oenothein A (mg/g)")+ theme_classic()
    plot9c <- plot9c +   theme(axis.text.x = element_text(size=13, face="bold"),
                               axis.text.y = element_text(size=13,face="bold"),
                               axis.title.x = element_text(color="black", size=12, vjust = 0, face="bold"),
                               axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
    plot9c
    ggsave("9c_Fruit_oe.pdf",width=7,height=6,units="in") #missing out outlier
    
    ## Cowplot export at 4 X 11 inches
    plot_grid(plot9a,plot9b,plot9c,ncol = 3)    

    

  

##################################################################################
# 10. What traits are most important in predicting seed number
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    #Unclear what exactly is being plotted on the y-axis. Its not seeds which is what I need.
    visreg(qu_tfr,"Fruit_Totphe",gg=TRUE) +
      scale_x_continuous(name="Fruit Total Phenolics", limits=c(0,200))+
      #scale_y_continuous(limits=c(0,14))+
      scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
      scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
      theme_classic()+ theme(
        axis.text.x = element_text(size=12, face="bold"),
        axis.text.y = element_text(size=14,face="bold"),
        axis.title.x = element_text(size=14, vjust = 0.5, face="bold"),
        axis.title.y = element_text(color="black", size=14,vjust = 2, face="bold",hjust=0.5))
    
    
    
    #Produce graphs of each variable seperately
    #MAT
    qu_MAT<- glm.nb(Seeds ~ MAT + I(MAT^2), data=ksr_m)
    stepAIC(qu_MAT,direction="both") #Keep quadratic
    Anova(qu_MAT,type=3) 
    #Plot
    plot3 <-visreg(qu_MAT, "MAT", scale="response", partial=TRUE, gg=TRUE, line=list(col="black")) +
      geom_point(size=1)+ scale_x_continuous(name="Mean Annual Temperature (°C)")+
      scale_y_continuous(name="Seed Number")+ theme_classic()
    plot3 <- plot3 + theme(axis.text.x = element_text(size=13, face="bold"),
                           axis.text.y = element_text(size=13,face="bold"),
                           axis.title.x = element_text(color="black", size=15, vjust = 0, face="bold"),
                           axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
    plot3
    
    #RH, not included in paper
    qu_RH<- glm.nb(Seeds ~ RH + I(RH^2), data=ksr_m)
    stepAIC(qu_RH,direction="both") #Keep quadratic
    Anova(qu_RH,type=3)
    visreg(qu_RH, "RH", scale="response", partial=TRUE, gg=TRUE)
    
    
  