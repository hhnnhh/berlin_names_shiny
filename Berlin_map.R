
#library(tidyverse)
library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)
library(rgdal)
library(leaflet)
#library(broom)
library(maps)
library(leaflet)
library(maptools)



# 1. set working directory
setwd("D:/Dropbox/R_wissen/berlinnames")
#setwd("C:/Users/hanna/Dropbox/R_wissen/berlinnames")
getwd()
list.files()#

# MAP Cloropleth https://www.r-graph-gallery.com/interactive-charts/

# BERLIN REGION OUTLINES: 
# Download .shp file on the web:
#download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip" , destfile="world_shape_file.zip")
# https://github.com/funkeinteraktiv/Berlin-Geodaten

# go to folder where Berlin geodata are
setwd('data/Berlin-Geodaten-master/')
list.files()
setwd("./berlin_bezirke_shp")
#list.files('../data/Berlin-Geodaten-master/berlin_bezirke_shp', pattern='\\.shp$')
list.files(pattern='\\.shp$')
file.exists('berliner_bezirke.shp')

# see "Data visualization" by Kieran Healy p.177

#load geodata = region outlines
library(rgdal)
berlin_spdf=readOGR(dsn= getwd(), layer="berliner_bezirke",use_iconv = TRUE, encoding = "UTF-8")
# convert shp data into data frame - THIS STEP IS ESSENTIAL 
#bm<-map_data(berlin_spdf)
#bm$Kiez<-bm$region
head(bm)
bm$region<-NULL
bm$subregion<-NULL

library(readr)
write_excel_csv(bm, "data/berlinmapdata.csv")

# the map can already be plotted
p<-ggplot(data=bm, mapping = aes(x=long,y=lat,group=group,fill=Kiez))

# make it less distorted with albers projection 
p + geom_polygon(color="grey90",size=0.1)+
  coord_map(projection="albers",lat0=13,lat1=53)+
  guides(fill=FALSE)

# check the data & rename data frame with values so it can be merged later
# regions are named "regions" in map data so I rename "name" to "regions" as well
#kiezdata$region<-kiezdata$name
#kiezdata$region
#kiezdata$name<-NULL

#rename regions with umlaut, otherwise they won't match! 
levels(berlin_spdf@data$name)

levels(berlin_spdf@data$name)[levels(berlin_spdf@data$name)=='Tempelhof-Schöneberg'] <- 'Tempelhof-Schoeneberg'
levels(berlin_spdf@data$name)[levels(berlin_spdf@data$name)=='Neukölln'] <- 'Neukoelln'
levels(berlin_spdf@data$name)[levels(berlin_spdf@data$name)=='Treptow-Köpenick'] <- 'Treptow-Koepenick'

#load data with frequency to join with spacial data. Filter, group and summarise first
setwd("D:/Dropbox/R_wissen/berlin_names_spacial")
df <- read.csv("data/berlin.csv")

library(dplyr)
kiezdata<-df %>% 
  select(vorname,Kiez,anzahl) %>% 
  filter(vorname == "Max")%>%
  group_by(Kiez)%>%
  summarise(s=sum(anzahl))
kiezdata

#check
head(bm)
summary(bm)
print(bm)
head(bm)
kiezdata$region

#check if they match, otherwise there will be sth wrong
which(bm$region == kiezdata$region)
bm$region == kiezdata$region
length(kiezdata$region)
str(kiezdata)
str(bm)

#merge map with anzahl-df, if there is only one similiarity it will take "region"
anzahlmap<-left_join(bm,kiezdata)
#bm <- merge(kiezdata, berlin_spdf@data, by="region")

p<-ggplot(data=anzahlmap, mapping = aes(x=long,y=lat,group=group,fill=anzahl))
p + geom_polygon(color="grey90",size=0.1)+
  coord_map(projection="albers",lat0=13,lat1=53)+
  guides(fill=anzahl)

#Plots Berlin with lightgreen background
par(mar=c(0,0,0,0))
plot(anzahlmap, col="#f2f2f2", fill=TRUE, bg="grey", lwd=0.25, mar=rep(0,4), border=0 )
library (ggplot2)
# https://medium.com/@anjesh/step-by-step-choropleth-map-in-r-a-case-of-mapping-nepal-7f62a84078d9

berlin<-ggplot(data = anzahlmap, aes(x = long, y = lat, group = group))
berlin+geom_path() # plottet die outlines der kieze

berlin + 
  geom_polygon(aes(fill = as.numeric(anzahl))) + # plottet die Kieze als Polygone
  coord_fixed(1.3) #+
  guides(fill = FALSE) 

#id <-berlin_spdf@data$anzahl

berlin + 
  geom_polygon(aes(fill = id), color = 'gray', size = 0.1) +
  coord_fixed(1.3)

# https://learn.r-journalism.com/en/mapping/census_maps/census-maps/
library(tigris)
# with tigris it is possible to join geodata with a dataframe
#anzahlmap <- geo_join(berlin_spdf, kiezdata, "name", "Kiez")
anzahlmap <- geo_join(berlin_spdf, p, "name", "Kiez")


pal <- colorNumeric("Greens", domain=anzahlmap$percentage)
popup_sb <- paste0("Rank:", as.character(anzahlmap$rank),"Percentage:", as.character(anzahlmap$percentage),"Total: ", as.character(anzahlmap$anzahl))


#library(leaflet)
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(13.41053,52.52437, zoom = 10) %>% 
  addPolygons(data = anzahlmap , 
              fillColor = ~pal(anzahlmap$percentage), 
              fillOpacity = 0.7, 
              weight = 0.2, 
              smoothFactor = 0.2, 
              popup = ~popup_sb) %>%
  addLegend(pal = pal, 
            values = anzahlmap$percentage, 
            position = "bottomright", 
            title = "Names")

