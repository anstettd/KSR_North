##################################################################################
# Analyses of what traits best predict Seed Number
## Daniel Anstett
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
ksr_trait <- ksr_m %>% select(Seeds,Flowering_Date,Bolt_Date,Growth_Rate,Num_Trichomes,
                              Water_Content,Flower_Oenothein_A,Fruit_Oenothein_A,
                              Flower_Totphe,Fruit_Totphe)

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














