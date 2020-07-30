#################
# Set up of full data frames for individual plant and plant means analysis. 
# Seed calculations
# Test of normality with transformations
#################

library(tidyverse)

############################
#Lat, distance, climate
############################
climate_lat <- read.csv("Data/climate_pop.csv", header=T) # Imports populations' climate dataset
climate_lat$MAR[2] <- climate_lat$MAR[1] # Replace error MAR values with near by population
climate_lat$MAR[12] <- climate_lat$MAR[18] 
climate_lat <- climate_lat %>% select(Pop,Latitude,Longitude,Distance, #Organize variables
                                      MAT,MWMT,MCMT,TD,NFFD,DD18,MAR, # Temperature/sun
                                      MAP,MSP,CMD,RH) # Precipt/moiusture
#Correlate all climate
library(Hmisc)
climate_matrix <-climate_lat %>% select(MAT,MWMT,MCMT,TD,NFFD,DD18,MAR, # Temperature/sun
                                        MAP,MSP,CMD,RH) # Precipt/moiusture
climate_matrix <- as.matrix(climate_matrix)
rcorr(climate_matrix) # MAT highly correlated to everything except MSP, CMD, RH.
climate_lat <- climate_lat %>% select(Pop,Latitude,Longitude,Distance,MAT,MSP,CMD,RH) # Retain only MAT, MSP, CMD, RH

#KSR climate data
#KSR_climate <- read.csv("Data/KSR_climate_reduced.csv", header=T) # Will not import. Excel calc instead, "KSR_climate_calc.xlsx"
KSR_MAT<-7.9 # Accounts for July 2012 to September 2013. 12 month average with repeated months averaged first. 
KSR_MSP<-456 # 2013 value for MSP, supplemental water given in 2012, making 2012 data less useful.
KSR_CMD<-121 # Calculated the same as KSR_MAT except sum taken for 12 months
KSR_RH<-69 # Calculated the same as KSR_MAT
#Generate climatic differences between Pop_climate - KSR_Climate
climate_lat <- climate_lat %>% #This info can then be joined to invidual plant and chem means datasets
  mutate(MAT_Distance=MAT-KSR_MAT) %>% mutate(MSP_Distance=MSP-KSR_MSP) %>% 
  mutate(CMD_Distance=CMD-KSR_CMD) %>% mutate(RH_Distance=RH-KSR_RH)

########################################################
#Individual dataset seed number calculations
########################################################
ksr_i <- read.csv("Data/KSR_individual.csv", header=T) # Imports raw indvidual dataset
ksr_i <- ksr_i %>% drop_na("Fruit") # Remove rows where fruit information is NA
ksr_i <- ksr_i %>% mutate(Fruit_no_damage=Fruit-S.florida-0.18*M.brevivatella) # Calculate undamaged portions of fruits
ksr_i$Fruit_no_damage[is.na(ksr_i$Fruit_no_damage)==TRUE]<-0 # Makes NA = 0 for above calculation
ksr_length <- ksr_i %>% select(l1,l2,l3,l4,l5) ; ksr_Ml<-rowMeans(ksr_length,na.rm=T) # Average fruit length
ksr_width <- ksr_i %>% select(d1,d2,d3,d4,d5) ; ksr_Mw<-rowMeans(ksr_width,na.rm=T) # Average fruit width
ksr_i <- ksr_i %>% mutate (F_length=ksr_Ml) %>% mutate (F_width=ksr_Mw) # Add averages to ksr_i
ksr_i$F_length[ksr_i$F_length=="NaN"]<-0 ; ksr_i$F_width[ksr_i$F_width=="NaN"]<-0 # remove NaNs and replace with 0
ksr_i <- ksr_i %>% mutate(Fruit_correction=ifelse(Fruit==0,0,1)) # Generate dummy variable to ensure 0 fruits becomes 0 seeds
ksr_i <- ksr_i %>%
  mutate (Seeds_per_fruit=(2.33*F_length*F_width - 102.47)*Fruit_correction) %>% # Calculate number of seeds per 1 fruit
  mutate(Seeds_no_round=Fruit_no_damage*Seeds_per_fruit) %>% # calculate number of seeds per all undamaged fruits
  mutate(Seeds=round(Seeds_no_round))
#ksr_i$Seeds=round(ksr_i$Seeds)
ksr_i <- ksr_i %>% select(-l1,-l2,-l3,-l4,-l5,-d1,-d2,-d3,-d4,-d5,-Fruit,-F_length,-F_width,
                          -Flower_Number,-Seeds_no_round) # Remove uneeded variables
ksr_i <- ksr_i %>% filter(Pop!=659) %>% # Remove population from Italy (European hybrid)
  filter(Pop!=661) %>% # Not correct species (oenothera oakesiana)
  filter(Pop!=877) # Remove,unclear where it is from.
ksr_i$Seeds[ksr_i$Seeds<0]<-0 #Nine cases of plants with <0 fruits assigned zero. Small L & W suggest aborted fruits

#Assess normality of seeds
qqnorm(ksr_i$Fruit_no_damage) # ~ Extreme left skew, zero inflated.
ggplot(data=ksr_i,aes(x=Fruit_no_damage)) + geom_histogram()+theme_classic()
qqnorm(log(1+ksr_i$Fruit_no_damage)) # ~ Extreme zero inflation
ggplot(data=ksr_i,aes(x=log(1+Fruit_no_damage))) + geom_histogram()+theme_classic()

ksr_i <- left_join(ksr_i,climate_lat,by="Pop") #Add lat/climate to individual dataset

write.csv(ksr_i,'Data/ksr_i.csv') #Export individual plants file


############################
#Total phenolics, oxidative capacity, oenothein 
############################
totphe.dat <- read.csv("Data/totphe.csv", header=T) # Imports total phenolics data
totphe.dat<-totphe.dat %>% select(-Control.sample.ID.)
totphe_leaf <- totphe.dat %>% filter(Tissue=="Leaf") %>% select(-Tissue) %>% # filter per tissue
  rename (Leaf_Totphe=totphe,Leaf_pH_10=pH_10) #rename columns to include tissue name 
totphe_flower <- totphe.dat %>% filter(Tissue=="Flower") %>% select(-Tissue)%>% 
  rename (Flower_Totphe=totphe,Flower_pH_10=pH_10)
totphe_fruit <- totphe.dat %>% filter(Tissue=="Fruit") %>% select(-Tissue)%>% 
  rename (Fruit_Totphe=totphe,Fruit_pH_10=pH_10)
totphe_all <- left_join(totphe_leaf,totphe_flower,by="Pop") %>% left_join(totphe_fruit,by="Pop") #generate total phenolics dataframe


oe <- read.csv("Data/oe.csv", header=T) # Imports Oenothein data
oe<-oe %>% select(-Turku_ID)
oe_leaf <- oe %>% filter(Tissue=="Leaf") %>% select(-Tissue) %>% # filter per tissue
  rename (Leaf_Oenothein_B=Oenothein_B,Leaf_Oenothein_A=Oenothein_A,Leaf_Ox_Oenothein_A=Oxidized_Oenothein_A) #rename columns 
oe_flower <- oe %>% filter(Tissue=="Flower") %>% select(-Tissue)%>%
  rename (Flower_Oenothein_B=Oenothein_B,Flower_Oenothein_A=Oenothein_A,Flower_Ox_Oenothein_A=Oxidized_Oenothein_A) %>% #rename columns 
  filter(Pop!=700) #Remove outlier pop (issue with chemistry)
oe_fruit <- oe %>% filter(Tissue=="Fruit") %>% select(-Tissue)%>%
  rename (Fruit_Oenothein_B=Oenothein_B,Fruit_Oenothein_A=Oenothein_A,Fruit_Ox_Oenothein_A=Oxidized_Oenothein_A) %>% #rename columns 
  filter(Pop!=709) #Remove outlier pop (issue with chemistry)
oe_all <- left_join(oe_leaf,oe_flower,by="Pop") %>% left_join(oe_fruit,by="Pop") #generate oenothein dataframe

chm<-left_join(totphe_all,oe_all,by="Pop") # join total phenolics with oenothein data
ksr_seed <- ksr_i %>% select(Pop,Seeds) # select seed data from indvidual plant data frame
ksr_seed <- ksr_seed %>% group_by(Pop) %>% summarise (Seeds=mean(Seeds)) # Take mean of seed data
chm<-left_join(chm,ksr_seed,by="Pop") # Add seed means to chemistry
chm <- chm %>% filter(Pop!=659) %>% # Remove population from Italy (European hybrid)
  filter(Pop!=661) %>% # Not correct species (oenothera oakesiana)
  filter(Pop!=877) # Remove,unclear where it is from.

write.csv(chm,'Data/chm.csv') #Export chemistry file

















