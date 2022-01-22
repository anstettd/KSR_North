##################################################################################
# Analyses of what traits best predict Seed Number
## Daniel Anstett
## Using randomForest code written by Nick J Lyon 
## Last updated Jan 19, 2022
##################################################################################

# Clear environment
rm(list = ls())

# Get this package retrieving function
## This function will automatically load packages that you already have
## and will install packages you don't yet have then load them
ipak <- function(pkg){
  # Function written by Dr. Evan Fricke
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = T)
  sapply(pkg, require, character.only = T)
}

# Define the packages that the script needs
myPackages <- c("tidyverse", "randomForest", "permimp", "vegan")

# Load the packages
ipak(myPackages)

##################################################################################
##Part 1: Prepare Data
ksr_m <- read.csv("Data/ksr_m.csv", header=T) # Imports individual dataset

#Subset data to just all trait variables
ksr_trait <- ksr_m %>% select(Seeds,Flowering_Date,Water_Content,Growth_Rate,
                              Bolt_Date,SLA,Num_Trichomes,Leaf_Herb_Sept,bug,S.florida,M.brevivatella,
                              Leaf_Totphe,Flower_Totphe,Fruit_Totphe,Leaf_Oenothein_A,Flower_Oenothein_A,
                              Fruit_Oenothein_A)
#colnames(ksr_trait) <- c("Seeds", "Flowering_Date", "Leaf Toughness","Water Content", "Growth Rate",
#                         "Bolt Date", "SLA", "Trichome Number", "Leaf Herbivory", "P. spumarius",
#                         "S.florida", "M.brevivitella","Leaf Total Phenolics","Flower Total Phenlics",
#                         "Fruit Total Phenolics", "Leaf Oenothein A", "Flower Oenothein A",
#                         "Fruit Oenothein A")

##################################################################################
##Part 2: Run random forest

# Run the random forest
rf1 <- randomForest(Seeds ~ .,
                    # The 'Y ~ .' format uses all other columns as predictors
                    # Makes formatting your data **crucial**
                    data = ksr_trait,
                    ntree = 1000,
                    # How many trees should be in the forest
                    mtry = 2,
                    # mtry is # variables / node in tree
                    na.action = na.omit,
                    keep.forest = T,
                    keep.inbag = T)

# Create a variable importance plot
plot1 <-randomForest::varImpPlot(x = rf1,
                         sort = T,
                         n.var = (ncol(ksr_trait) - 1),
                         main = "Variable Importance")
## Export at 5.5 X 6

##################################################################################
##Plots

# Set custom aesthetic
pref_theme <- theme_classic() + theme(axis.text = element_text(size = 13),
                                      axis.title = element_text(size = 15),
                                      legend.position = "none")












