##################################################################################
# Map of O. biennis range
## Daniel Anstett
## January 5 2022
##################################################################################

#import libraries
library(tidyverse)
library(sf)
library(tmap)
library(rnaturalearth)
library(rnaturalearthdata)
#devtools::install_github("ropensci/rnaturalearthhires")
library(rnaturalearthhires)
library(rgeos)
library(RColorBrewer)

#import data
ksr_m <- read.csv("Data/ksr_m.csv", header=T) # Imports individual dataset


##################################################################################
#Define projections
EPSG4326<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" #setup WGS 1984 CRS
EPSG3857<- "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
robin <- "+proj=robin +lon_0=-69 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
lambert <- "+proj=lcc +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

#Setup data into sf object
lat_m <- ksr_m %>% select(Latitude,Longitude) #get lat/long
lat_sf <- st_as_sf(lat_m,coords=c("Longitude","Latitude"), crs=EPSG4326)
#ggplot()+ geom_sf(data = lat_sf)+ ggtitle("All Oenothera Populations")

#Bring in KSR Location
lat_ksr <- data.frame()
lat_ksr[1,1] <- 44.026349 ; lat_ksr[1,2] <- -79.544552
colnames(lat_ksr) <- c("Latitude","Longitude")
ksr <- st_as_sf(lat_ksr,coords=c("Longitude","Latitude"), crs=EPSG4326)


#Setup base map
states<-ne_states(country=c("canada","united states of america"),returnclass= "sf")
can_USA <- states %>%
  filter(name_en=="Oregon" | name_en=="California" | name_en=="Nevada" | name_en=="Arizona" | name_en=="Colorado" |
             name_en=="Idaho" | name_en=="Montana" | name_en=="New Mexico" | name_en=="Utah" | name_en=="Wyoming" |
             name_en=="Iowa" | name_en=="Kansas" | name_en=="Missouri" | name_en=="Nebraska" | name_en=="North Dakota" |
             name_en=="South Dakota" | name_en=="Illinois" | name_en=="Indiana" | name_en=="Michigan" |
             name_en=="Minnesota" | name_en=="Ohio" | name_en=="Wisconsin" | name_en=="Arkansas" |   
             name_en=="Louisiana," | name_en=="Oklahoma" | name_en=="Texas" | name_en=="Alabama" | name_en=="Florida" |
             name_en=="Georgia" | name_en=="Mississippi" | name_en=="South Carolina" | name_en=="North Carolina" |    
             name_en=="Virginia" | name_en=="West Virginia" | name_en=="Maryland" | name_en=="District of Columbia" |
           name_en=="Delaware" | name_en=="Kentucky" | name_en=="Tennessee" | name_en=="Connecticut" | 
           name_en=="Maine" | name_en=="Massachusetts" | name_en=="New Hampshire" | name_en=="Rhode Island" | 
           name_en=="Vermont" | name_en=="New Jersey" | name_en=="New York," | name_en=="Pennsylvania" | 
           name_en=="British Columbia" | name_en=="Alberta" | name_en=="Saskatchewan" | name_en=="Manitoba" | 
           name_en=="Ontario" | name_en=="Quebec" | name_en=="New Brunswick" | name_en=="Nova Scotia" | 
           name_en=="Prince Edward Island" | name_en=="Newfoundland and Labrador" | name_en=="Washington"|
           name_en=="Louisiana")
#st_crs(can_USA)

#Transform CRS
can_USA <- st_transform(can_USA,lambert)
lat_sf <- st_transform(lat_sf,lambert)
ksr_sf <- st_transform(ksr,lambert)

st_bbox(can_USA)

#Set bbox
#bbox_map <- st_bbox(c(xmin = -137, xmax = -52.6, ymax = 54, ymin = 24.5), EPSG4326)
#bbox_lambert <- st_bbox(c(xmin = -2306298, ymin = -152314, xmax = 2979277, ymax = 2711094), lambert)

#Make Map
tmap_mode("plot")
#tmap_mode("view")
fig1 <- tm_shape(can_USA)+
  tm_borders(col="black",lwd=0.3)+
  tm_shape(lat_sf)+
  tm_dots(size=0.08,shape=20)+
  tm_shape(ksr_sf)+
  tm_dots(size=0.4,shape=8)+
  tm_layout(legend.show = F)
fig1 
tmap_save(fig1, filename = "Figures/Fig1.pdf",width=5, height=6)











