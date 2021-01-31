library(broom)
install.packages("maptools")
library(maptools)
# Geospatial data available in the geojson format!
install.packages("geojsonio")
library(geojsonio)
library(rgdal)

spdf_fortified <- tidy(berlin_spdf2,region="name")
spdf_fortified

berlin <- ggplot(data = spdf_fortified, aes(x = long, y = lat, group = group))
berlin+geom_path() # plottet die outlines der kieze

# hier an den Margins schrauben?!
berlin + 
  geom_polygon(aes(fill = id)) + # plottet die Kieze als Polygone
  coord_fixed(1.3) +
  guides(fill = FALSE) 



# Now I can plot this shape easily as described before:
ggplot() +
  geom_polygon(data = spdf_fortified, aes( x = long, y = lat, group = id)) +
  theme_void() +
  coord_map()

library(maptools)
install.packages("gpclib")
library(gpclib)
library(ggplot2)

setwd("C:/Users/Hannah Bohle/Dropbox/R_wissen/letters/Berlin-Geodaten-master/berlin_bezirke_shp")
list.files('C:/Users/Hannah Bohle/Dropbox/R_wissen/letters/Berlin-Geodaten-master/berlin_bezirke_shp', pattern='\\.shp$')
file.exists('berliner_bezirke.shp')

berlin_spdf=readOGR(dsn= getwd(), layer="berliner_bezirke",use_iconv = TRUE, encoding = "UTF-8")


shape<-readShapeSpatial("./berliner_bezirke.shp") 
shape@data$id <- rownames(shape@data)
shape.fort <- fortify(shape, region='id') 
shape.fort<-shape.fort[order(shape.fort$order), ] 
ggplot(data=shape.fort, aes(long, lat, group=group)) + 
  geom_polygon(colour='black',
               fill='white') +
  theme_bw()
