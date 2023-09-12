########################################################################################################
# Set up of full data frames for all plant traits
# Carry out calculation to estimate seed number

#Author: Daniel Anstett
## Last updated Sept 12, 2023
########################################################################################################

# Clear environment
rm(list = ls())

library(tidyverse)
library(Hmisc)

############################
#Lat, distance, climate
############################
climate_lat <- read.csv("Data/climate_pop.csv", header=T) # Imports populations' climate dataset
#Dataset downloaded from climate NA for all study populations
climate_lat$MAR[2] <- climate_lat$MAR[1] # Replace error MAR values with near by population
climate_lat$MAR[12] <- climate_lat$MAR[18] 
climate_lat <- climate_lat %>% select(Pop,Latitude,Longitude,Distance, #Organize variables
                                      MAT,MWMT,MCMT,TD,NFFD,DD18,MAR, # Temperature/solar radiation variables
                                      MAP,MSP,CMD,RH) # precipitation and moisture variables
#Correlate all climate

climate_matrix <-climate_lat %>% select(MAT,MWMT,MCMT,TD,NFFD,DD18,MAR, # Temperature/solar radiation variables
                                        MAP,MSP,CMD,RH) # precipitation and moisture variables
climate_matrix <- as.matrix(climate_matrix)
rcorr(climate_matrix) # MAT highly correlated to everything except MSP, CMD, RH.
climate_lat <- climate_lat %>% select(Pop,Latitude,Longitude,Distance,MAT,MSP,CMD,RH) # Retain only MAT, MSP, CMD, RH

#KSR (Common garden field site) climate data

#See excel file for detailed calculation: "KSR_climate_calc.xlsx"
KSR_MAT<-7.9 #Calculation carried out for July 2012 to September 2013. 12 month average with repeated months (July, August, and September) averaged first. 
KSR_MSP<-456 # 2013 value for MSP only since supplemental water given in 2012, making 2012 data less useful.
KSR_CMD<-121 # Calculated the same as KSR_MAT except sum taken for 12 months
KSR_RH<-69 # Calculated the same as KSR_MAT

#Generate climatic differences between Pop_climate - KSR_Climate
climate_lat <- climate_lat %>% #This info can then be joined to invidual plant and chem means datasets
  mutate(MAT_Distance=MAT-KSR_MAT) %>% mutate(MSP_Distance=MSP-KSR_MSP) %>% 
  mutate(CMD_Distance=CMD-KSR_CMD) %>% mutate(RH_Distance=RH-KSR_RH)
climate_lat <- climate_lat %>% filter(Pop!=659) %>% # Remove population from Italy (European hybrid)
  filter(Pop!=661) %>% # Not correct species (oenothera oakesiana)
  filter(Pop!=877) # Remove,unclear where this population is from.

########################################################################################################
#Individual dataset seed number calculations
########################################################################################################
ksr_i <- read.csv("Data/KSR_individual_final.csv", header=T) # Imports raw indvidual dataset
ksr_i <- ksr_i %>% drop_na("Fruit") # Remove rows where fruit information is NA
ksr_i <- ksr_i %>% mutate(Fruit_no_damage=Fruit-S.florida-0.18*M.brevivatella) # Calculate undamaged portions of fruits
# -0.18*M.brevivatella is substracted from fruit total because this is the average proportion of damage of seeds that 
# one M.brevivitella larve is known to damage based on data provided by Marc Johnson (personal communcation). 

ksr_i$Fruit_no_damage[is.na(ksr_i$Fruit_no_damage)==TRUE]<-0 # Makes NA = 0 for above calculation
ksr_length <- ksr_i %>% select(l1,l2,l3,l4,l5) ; ksr_Ml<-rowMeans(ksr_length,na.rm=T) # Average fruit length
ksr_width <- ksr_i %>% select(d1,d2,d3,d4,d5) ; ksr_Mw<-rowMeans(ksr_width,na.rm=T) # Average fruit width
ksr_i <- ksr_i %>% mutate (F_length=ksr_Ml) %>% mutate (F_width=ksr_Mw) # Add averages to ksr_i
ksr_i$F_length[ksr_i$F_length=="NaN"]<-0 ; ksr_i$F_width[ksr_i$F_width=="NaN"]<-0 # remove NaNs and replace with 0
ksr_i <- ksr_i %>% mutate(Fruit_correction=ifelse(Fruit==0,0,1)) # Generate dummy variable to ensure 0 fruits becomes 0 seeds
ksr_i <- ksr_i %>% #Seeds per fruit formula from Agrawal et al. 2012 Am Nat
  mutate (Seeds_per_fruit=(2.33*F_length*F_width - 102.47)*Fruit_correction) %>% # Calculate number of seeds per 1 fruit
  mutate(Seeds_no_round=Fruit_no_damage*Seeds_per_fruit) %>% # calculate number of seeds per all undamaged fruits
  mutate(Seeds=round(Seeds_no_round))
#ksr_i$Seeds=round(ksr_i$Seeds)
ksr_i <- ksr_i %>% select(-l1,-l2,-l3,-l4,-l5,-d1,-d2,-d3,-d4,-d5,-Fruit,-F_length,-F_width,
                          -Flower_Number,-Seeds_no_round) # Remove uneeded variables
ksr_i <- ksr_i %>% filter(Pop!=659) %>% # Remove population from Italy (European hybrid)
  filter(Pop!=661) %>% # Not correct species (oenothera oakesiana)
  filter(Pop!=877) # Remove population,unclear where it is from.
ksr_i$Seeds[ksr_i$Seeds<0]<-0 #Nine cases of plants with <0 fruits assigned zero. Small L & W suggest aborted fruits


########################################################################################################
#Total phenolics, oxidative capacity, oenothein 
########################################################################################################
totphe.dat <- read.csv("Data/totphe.csv", header=T) # Imports total phenolics data
totphe.dat<-totphe.dat %>% select(-Control.sample.ID.) #remove asscending numerical variable
totphe_leaf <- totphe.dat %>% filter(Tissue=="Leaf") %>% select(-Tissue) %>% # filter per tissue
  rename (Leaf_Totphe=totphe,Leaf_pH_10=pH_10) #rename columns to include tissue name 
totphe_flower <- totphe.dat %>% filter(Tissue=="Flower") %>% select(-Tissue)%>% 
  rename (Flower_Totphe=totphe,Flower_pH_10=pH_10)
totphe_fruit <- totphe.dat %>% filter(Tissue=="Fruit") %>% select(-Tissue)%>% 
  rename (Fruit_Totphe=totphe,Fruit_pH_10=pH_10)
totphe_all <- left_join(totphe_leaf,totphe_flower,by="Pop") %>% left_join(totphe_fruit,by="Pop") #generate total phenolics dataframe


oe <- read.csv("Data/oe_final.csv", header=T) # Imports Oenothein data
oe<-oe %>% select(-Turku_ID) #rem
oe_leaf <- oe %>% filter(Tissue=="Leaf") %>% select(-Tissue) %>% # filter per tissue
  rename (Leaf_Oenothein_B=Oenothein_B,Leaf_Oenothein_A=Oenothein_A) #rename columns 
oe_flower <- oe %>% filter(Tissue=="Flower") %>% select(-Tissue)%>%
  rename (Flower_Oenothein_B=Oenothein_B,Flower_Oenothein_A=Oenothein_A) %>% #rename columns 
  filter(Pop!=700) #Remove outlier pop (issue with chemistry)
oe_fruit <- oe %>% filter(Tissue=="Fruit") %>% select(-Tissue)%>%
  rename (Fruit_Oenothein_B=Oenothein_B,Fruit_Oenothein_A=Oenothein_A) %>% #rename columns 
  filter(Pop!=709) #Remove outlier pop (issue with chemistry)
oe_all <- left_join(oe_leaf,oe_flower,by="Pop") %>% left_join(oe_fruit,by="Pop") #generate oenothein dataframe

chm<-left_join(totphe_all,oe_all,by="Pop") # join total phenolics with oenothein data
chm <- chm %>% filter(Pop!=659) %>% # Remove population from Italy (European hybrid)
  filter(Pop!=661) %>% # Not correct species (oenothera oakesiana)
  filter(Pop!=877) # Remove,unclear where it is from.
ksr_i <- ksr_i %>% select(-Plant.ID,-Block,-Fruit_correction,
                             -Seeds_per_fruit) # Remove variables where mean is not needed
#Take mean of each variable from ksr_i
  ksr_seed <- ksr_i %>% group_by(Pop) %>% summarise(Seeds=mean(Seeds,na.rm=TRUE))
  ksr_Fruit_no_damage <- ksr_i %>% group_by(Pop) %>% summarise(Fruit_no_damage=mean(Fruit_no_damage,na.rm=TRUE))
  ksr_fl <- ksr_i %>% group_by(Pop) %>% summarise(Flowering_Date=mean(Flowering_Date,na.rm=TRUE))
  ksr_lt <- ksr_i %>% group_by(Pop) %>% summarise(Leaf_Toughness=mean(Leaf_Toughness,na.rm=TRUE)) 
  ksr_wc <- ksr_i %>% group_by(Pop) %>% summarise(Water_Content=mean(Water_Content,na.rm=TRUE))
  ksr_gr <- ksr_i %>% group_by(Pop) %>% summarise(Growth_Rate=mean(Growth_Rate,na.rm=TRUE))
  ksr_bd <- ksr_i %>% group_by(Pop) %>% summarise(Bolt_Date=mean(Bolt_Date,na.rm=TRUE))
  ksr_SLA <- ksr_i %>% group_by(Pop) %>% summarise(SLA=mean(SLA,na.rm=TRUE))
  ksr_tri <- ksr_i %>% group_by(Pop) %>% summarise(Num_Trichomes=mean(Num_Trichomes,na.rm=TRUE))
  ksr_lhs <- ksr_i %>% group_by(Pop) %>% summarise(Leaf_Herb_Sept=mean(Leaf_Herb_Sept,na.rm=TRUE))
  ksr_bug <- ksr_i %>% group_by(Pop) %>% summarise(bug=mean(bug,na.rm=TRUE))
  ksr_h <- ksr_i %>% group_by(Pop) %>% summarise(Hight=mean(Hight,na.rm=TRUE))
  ksr_sf <- ksr_i %>% group_by(Pop) %>% summarise(S.florida=mean(S.florida,na.rm=TRUE)) 
  ksr_mb <- ksr_i %>% group_by(Pop) %>% summarise(M.brevivatella=mean(M.brevivatella,na.rm=TRUE))
  
#Join all means into one data frame
  ksr_means_prep<-left_join(ksr_Fruit_no_damage,ksr_seed,by="Pop") %>%
    left_join(ksr_fl,by="Pop") %>%
    left_join(ksr_lt,by="Pop") %>%
    left_join(ksr_wc,by="Pop") %>%
    left_join(ksr_gr,by="Pop") %>%
    left_join(ksr_bd,by="Pop") %>%
    left_join(ksr_SLA,by="Pop") %>%
    left_join(ksr_tri,by="Pop") %>%
    left_join(ksr_lhs,by="Pop") %>%
    left_join(ksr_bug,by="Pop") %>%
    left_join(ksr_h,by="Pop") %>%
    left_join(ksr_sf,by="Pop") %>%
    left_join(ksr_mb,by="Pop")
  
#Generate one full dataset for climate and trait means
ksr_m<- left_join(climate_lat,ksr_means_prep,by="Pop") %>% left_join(chm,by="Pop")
ksr_m[ksr_m=="NaN"]<-NA #Remove NaN and replace with NA


write.csv(ksr_m,'Data/ksr_m.csv') #Export data file

