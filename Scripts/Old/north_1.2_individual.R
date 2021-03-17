#################
# Analyses individual plant data
#################

library(MASS)
#library(glmmTMB)
library(lmtest)
library(visreg)
library(tidyverse)
library(Hmisc)
library(car)
#library(NBZIMM)
library(nlme)

ksr_i <- read.csv("Data/ksr_i.csv", header=T) # Imports individual dataset

########################
# What geographic/environmental traits predict plant fitness?
######################## 

#1. Does geography predict success?

  qu_geo <- glmer.nb(Seeds ~ Latitude*Longitude + (1|Pop) + (1|Block), 
                     control=glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=ksr_i)
  qu_geo_main <- glmer.nb(Seeds ~ Latitude+Longitude + (1|Pop) + (1|Block), data=ksr_i)
  lrtest(qu_geo,qu_geo_main) # no sign difference, pick main effects only


  qu_geo_lat <- glmmTMB(Seeds ~ Latitude + (1|Pop) + (1|Block), data=ksr_i, family=poisson, ziformula=~1)
  lrtest(qu_geo_main,qu_geo_lat) # Lat + Long significnatly better than Lat alone.
  qu_geo_long <- glmmTMB(Seeds ~ Longitude + (1|Pop) + (1|Block), data=ksr_i, family=poisson, ziformula=~1)
  lrtest(qu_geo_main,qu_geo_long) # Models not signifcantly different 
  qu_geo_none <- glmmTMB(Seeds ~ (1|Pop) + (1|Block), data=ksr_i, family=poisson, ziformula=~1)
  lrtest(qu_geo_long,qu_geo_none) # Longitude model significatlly better p = 0.002903
  
visreg(qu_geo_main, "Latitude", gg=TRUE)
visreg(qu_geo_main, "Longitude", gg=TRUE)
  
#2. Does distance to common garden predict Seeds?
  qu_dist <- glmer.nb(Seeds ~ Distance + (1|Pop) + (1|Block), data=ksr_i)
  qu_no <- glmmTMB(Seeds ~ (1|Pop) + (1|Block), data=ksr_i, family=poisson, ziformula=~1)
  lrtest(qu_dist,qu_no) #distance is significantly better than random only.
  visreg(qu_dist, "Distance", gg=TRUE)
  
#3. What environmental variable at site of origin best predicts success?
  qu_env <- glmmTMB(Seeds ~ MAT + MSP + CMD + RH + (1|Pop) + (1|Block), data=ksr_i, family=poisson, ziformula=~1)
  qu_noRH <- glmmTMB(Seeds ~ MAT + MSP + CMD + (1|Pop) + (1|Block), data=ksr_i, family=poisson, ziformula=~1)
  lrtest(qu_env,qu_noRH) #No difference, remove RH
  qu_noCMD <- glmmTMB(Seeds ~ MAT + MSP + (1|Pop) + (1|Block), data=ksr_i, family=poisson, ziformula=~1)
  lrtest(qu_noRH,qu_noCMD) #Model significaly better with CMD
  qu_noMSP <- glmmTMB(Seeds ~ MAT + CMD + (1|Pop) + (1|Block), data=ksr_i, family=poisson, ziformula=~1)
  lrtest(qu_noRH,qu_noMSP) # No difference with our without MSP. Remove MSP
  qu_noMAT <- glmmTMB(Seeds ~ CMD + (1|Pop) + (1|Block), data=ksr_i, family=poisson, ziformula=~1)
  lrtest(qu_noMSP,qu_noMAT) # No difference with MAT, remove.
  qu_no <- glmmTMB(Seeds ~ (1|Pop) + (1|Block), data=ksr_i, family=poisson, ziformula=~1)
  lrtest(qu_noMAT,qu_no) # Model significatlly better with CMD
  
#4. What env distance (common garden env – genotype’s env) best predicts success?
  qu_envdist <- glmmTMB(Seeds ~ MAT_Distance + MSP_Distance + CMD_Distance + RH_Distance + 
                      (1|Pop) + (1|Block), data=ksr_i, family=poisson, ziformula=~1)
  qu_RH_Distance <- glmmTMB(Seeds ~ MAT_Distance + MSP_Distance + CMD_Distance + 
      (1|Pop) + (1|Block), data=ksr_i, family=poisson, ziformula=~1)
  lrtest(qu_envdist,qu_RH_Distance) #No difference, remove RH
  qu_CMD_Distance <- glmmTMB(Seeds ~ MAT_Distance + MSP_Distance + (1|Pop) + (1|Block), data=ksr_i, family=poisson, ziformula=~1)
  lrtest(qu_RH_Distance,qu_CMD_Distance) #Model significaly better with CMD_Distance
  qu_MSP_Distance <- glmmTMB(Seeds ~ MAT_Distance + CMD_Distance + (1|Pop) + (1|Block), data=ksr_i, family=poisson, ziformula=~1)
  lrtest(qu_RH_Distance,qu_MSP_Distance) #No difference with our without MSP_Distance. Remove MSP_distance
  qu_MAT_Distance <- glmmTMB(Seeds ~ CMD_Distance + (1|Pop) + (1|Block), data=ksr_i, family=poisson, ziformula=~1)
  lrtest(qu_MSP_Distance,qu_MAT_Distance) # No difference with MAT_Distance, remove.
  qu_no_Distance <- glmmTMB(Seeds ~ + (1|Pop) + (1|Block), data=ksr_i, family=poisson, ziformula=~1)
  lrtest(qu_MAT_Distance,qu_no_Distance) # Model significatlly better with CMD_distance

  
########################
  # What plant traits predict plant fitness?
########################  
  
    
#5. Does herb impact predict success?
    #a. Do leaf herbiovry and xylem feeders impact seed number?
  leaf_bug <- ksr_i %>% select(Pop, Block, Seeds, Leaf_Herb_Sept, bug) #subset data
  leaf_bug <- na.omit(leaf_bug) #remove NA rows
  cor(leaf_bug$Leaf_Herb_Sept,leaf_bug$bug) #not correlated
  
  #Test leaf bug models
  qu_leaf_bug <- glmmTMB(Seeds ~ Leaf_Herb_Sept + bug + (1|Pop) + (1|Block), data=leaf_bug, family=poisson, ziformula=~1)
  qu_leaf <- glmmTMB(Seeds ~ Leaf_Herb_Sept + (1|Pop) + (1|Block), data=leaf_bug, family=poisson, ziformula=~1)
  lrtest(qu_leaf_bug,qu_leaf) # bug and herb model significantly better
  qu_bug <- glmmTMB(Seeds ~ bug + (1|Pop) + (1|Block), data=leaf_bug, family=poisson, ziformula=~1)
  lrtest(qu_leaf_bug,qu_bug) # bug and herb model significantly better
  qu_leafxbug <- glmmTMB(Seeds ~ Leaf_Herb_Sept*bug + (1|Pop) + (1|Block), data=leaf_bug, family=poisson, ziformula=~1)
  lrtest(qu_leaf_bug,qu_leafxbug) # bug and herb model significantly better
  
    #b. Does seed predation impact seed number?
  sf_mb <- ksr_i %>% select(Pop, Block, Seeds, S.florida, M.brevivatella) #subset data
  sf_mb <- na.omit(sf_mb) #remove NA rows
  cor(sf_mb$S.florida,sf_mb$M.brevivatella) #not correlated
  
  #Test S.florida, M.brevivatella models
  qu_sf_mb <- glmmTMB(Seeds ~ S.florida + M.brevivatella + (1|Pop) + (1|Block), data=sf_mb, family=poisson, ziformula=~1)
  qu_sf <- glmmTMB(Seeds ~ S.florida + (1|Pop) + (1|Block), data=sf_mb, family=poisson, ziformula=~1)
  lrtest(qu_sf_mb,qu_sf) # S.florida + M.brevivatella significnatly better
  qu_mb <- glmmTMB(Seeds ~ M.brevivatella + (1|Pop) + (1|Block), data=sf_mb, family=poisson, ziformula=~1)
  lrtest(qu_sf_mb,qu_mb) # S.florida + M.brevivatella significnatly better
  qu_sfXmb <- glmmTMB(Seeds ~ S.florida*M.brevivatella + (1|Pop) + (1|Block), data=sf_mb, family=poisson, ziformula=~1)
  lrtest(qu_sf_mb,qu_sfXmb) # S.florida * M.brevivatella significnatly better
  
#6. Does phenology predict success?
  pheno <- ksr_i %>% select(Pop, Block, Seeds, Flowering_Date, Bolt_Date, Growth_Rate) #subset data
  pheno <- na.omit(pheno) #remove NA rows
  pheno_matrix <- as.matrix(pheno)
  rcorr(pheno_matrix) #correlation r<|0.28| for all
  
  #Test phenology models
  qu_pheno <- glmmTMB(Seeds ~ Flowering_Date + Bolt_Date + Growth_Rate + (1|Pop) + (1|Block), 
                      data=pheno, family=poisson, ziformula=~1)
  qu_fl_bl <- glmmTMB(Seeds ~ Flowering_Date + Bolt_Date + (1|Pop) + (1|Block), data=pheno, family=poisson, ziformula=~1)
  lrtest(qu_pheno,qu_fl_bl) # model with Growth_Rate significally better
  qu_fl_gr <- glmmTMB(Seeds ~ Flowering_Date + Growth_Rate + (1|Pop) + (1|Block), data=pheno, family=poisson, ziformula=~1)
  lrtest(qu_pheno,qu_fl_gr) # model with Bolt_Date significally better
  qu_bl_gr <- glmmTMB(Seeds ~ Bolt_Date + Growth_Rate + (1|Pop) + (1|Block), data=pheno, family=poisson, ziformula=~1)
  lrtest(qu_pheno,qu_bl_gr) # model with Flowering _Date significally better, 3 mains effects model is the best.

  visreg(qu_pheno, "Flowering_Date", gg=TRUE)
 
  
#7. Does morphology predict success?
  morpho <- ksr_i %>% select(Pop, Block, Seeds, SLA, Water_Content, Leaf_Toughness, Num_Trichomes) #subset data
  morpho <- na.omit(morpho) #remove NA rows
  morpho_matrix <- as.matrix(morpho)
  rcorr(morpho_matrix) #SLA WC correlation r=0.33. All others not correlated
  
  #Test Morphology Models
  qu_morpho <- glmmTMB(Seeds ~ SLA + Water_Content + Leaf_Toughness + Num_Trichomes + (1|Pop) + (1|Block), 
                      data=morpho, family=poisson, ziformula=~1)
  qu_no_tri <- glmmTMB(Seeds ~ SLA + Water_Content + Leaf_Toughness + (1|Pop) + (1|Block), 
                      data=morpho, family=poisson, ziformula=~1)
  lrtest(qu_morpho,qu_no_tri) #Keep Trichomes
  qu_no_LT <- glmmTMB(Seeds ~ SLA + Water_Content + Num_Trichomes + (1|Pop) + (1|Block), 
                       data=morpho, family=poisson, ziformula=~1)
  lrtest(qu_morpho,qu_no_LT) #Keep Leaf_Toughness
  qu_no_WC <- glmmTMB(Seeds ~ SLA + Leaf_Toughness + Num_Trichomes + (1|Pop) + (1|Block), 
                       data=morpho, family=poisson, ziformula=~1)
  lrtest(qu_morpho,qu_no_WC) #Water content
  qu_no_SLA <- glmmTMB(Seeds ~ Water_Content + Leaf_Toughness + Num_Trichomes + (1|Pop) + (1|Block), 
                       data=morpho, family=poisson, ziformula=~1)
  lrtest(qu_morpho,qu_no_SLA) #Keep SLA. Model with 4 main effects highly significant
 
  
############
  #Chemistry Data
############
  
  #Use  glm.nb 
  #https://stat.ethz.ch/R-manual/R-devel/library/MASS/html/glm.nb.html
   
chm <- read.csv("Data/chm.csv", header=T) # Imports individual dataset
#Generate oxidative capacity as totphe - pH10
  chm <- chm %>% mutate(Leaf_Oxidative_Capacity=Leaf_Totphe-Leaf_pH_10) %>%
                  mutate(Flower_Oxidative_Capacity=Flower_Totphe-Flower_pH_10) %>%
                  mutate(Fruit_Oxidative_Capacity=Fruit_Totphe-Fruit_pH_10)

#8. Does broad chemistry predict success?
  tphe <- chm %>% select(Pop, Seeds, Leaf_Totphe,Flower_Totphe,Fruit_Totphe,
                         Leaf_Oxidative_Capacity,Flower_Oxidative_Capacity,Fruit_Oxidative_Capacity) #subset data
  tphe <- na.omit(tphe) #remove NA rows
  tphe_matrix <- as.matrix(tphe)
  rcorr(tphe_matrix) # Oxidative capacity is highly correlated to total phenolics. Remove oxidative capacity.
  tphe <- tphe %>% select(-Leaf_Oxidative_Capacity,-Flower_Oxidative_Capacity,-Fruit_Oxidative_Capacity)
  
  #models
  qu_tphe <- glm.nb(Seeds ~ Leaf_Totphe+Flower_Totphe+Fruit_Totphe, data=tphe)
  stepAIC(qu_tphe,direction="both")
  qu_tfr <- glm.nb(Seeds ~ Flower_Totphe+Fruit_Totphe, data=tphe) #Model with lower AIC
  summary(qu_tfr) #Flower and Fruit Significant
  #Anova(qu_tfr,type = 3) #not giving full ANOVA table
  
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

  
#9. Does detailed chemistry predict success? (Oe=Oenothein)
    oenothein <- chm %>% select(Pop, Seeds, Leaf_Oenothein_B, Leaf_Oenothein_A, Leaf_Ox_Oenothein_A,
                           Flower_Oenothein_B, Flower_Oenothein_A, Flower_Ox_Oenothein_A,
                           Fruit_Oenothein_B, Fruit_Oenothein_A,Fruit_Ox_Oenothein_A) #subset data
    oenothein <- na.omit(oenothein) #remove NA rows
    oenothein_matrix <- as.matrix(oenothein)
    #rcorr(oenothein_matrix) # All oenothein highly correlated. Do each tissue sperately. Lump A and oxA together
    
    #models Fruit
    OeAox <- glm.nb(Seeds ~ Fruit_Oenothein_A+Fruit_Ox_Oenothein_A,data=oenothein)
    stepAIC(OeA,direction="both")
    OeB <- glm.nb(Seeds ~ Fruit_Oenothein_B,data=oenothein)
    summary(OeAox)
    summary(OeB)
    OeA <- glm.nb(Seeds ~ Fruit_Oenothein_A,data=oenothein)
    summary(OeA)
    
    #models Flower
    OeAox.fl <- glm.nb(Seeds ~ Flower_Oenothein_A+Flower_Ox_Oenothein_A,data=oenothein)
    stepAIC(OeA.fl,direction="both")
    OeB.fl <- glm.nb(Seeds ~ Flower_Oenothein_B,data=oenothein)
    summary(OeAox.fl)
    summary(OeB.fl)
    OeA.fl <- glm.nb(Seeds ~ Flower_Ox_Oenothein_A,data=oenothein)
    summary(OeA.fl)
    
    #models Leaf
    OeAox.l <- glm.nb(Seeds ~ Leaf_Oenothein_A+Leaf_Ox_Oenothein_A,data=oenothein)
    stepAIC(OeA.l,direction="both")
    OeB.l <- glm.nb(Seeds ~ Leaf_Oenothein_B,data=oenothein)
    summary(OeAox.l)
    summary(OeB.l)
    OeA.l <- glm.nb(Seeds ~ Leaf_Oenothein_A,data=oenothein)
    summary(OeA.l)
    
  