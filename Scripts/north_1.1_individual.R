#################
# Analyses individual plant data
#################

library(glmmTMB)

ksr_i <- read.csv("Data/ksr_i.csv", header=T) # Imports individual dataset

#1. Does geography predict success?
  Fitness ~ Lat*Long + (1|pop) + (1|block)
  

#2. Does distance to common garden predict fitness?
    Fitness ~ Distance + (1|pop) + (1|block)
  
#3. What environmental variable at site of origin best predicts success?
    Fitness ~ Env1 + Env2... EnvX + (1|pop) + (1|block)
  
#4. What env distance (common garden env – genotype’s env) best predicts success?
    Fitness ~ D1 + D2 ... Dx + (1| pop) + (1|block)
  
#5. Does herb impact predict success?
    Fitness ~ leaf herbivory_year1 + leaf herbivory_year2 + Shinia florida + Mompha brevivitella + Philaenus spumarius + (1| pop) + (1|block)
  
#6. Does phenology predict success?
    
    Fitness ~ flower time + bolt date + growth rate (1| pop) + (1|block)
  
#7. Does morphology predict success?
    
    Fitness ~ SLA + Water Content + leaf toughness + Trichomes + (1| pop) + (1|block)
  
  











