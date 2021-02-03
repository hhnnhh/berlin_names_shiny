library(rgdal)

df <- read.csv("data/berlin.csv") # data for name trend and wordcloud
map_df <- read.csv("data/map_df.csv") # data for rank,%,frequency in map
berlin_spdf=readOGR("data/map2", layer="berliner_bezirke",use_iconv = TRUE, encoding = "UTF-8")

min_year <- min(df$year)
max_year <- max(df$year)

bmap<- leaflet() %>%
  setView(13.41053,52.52437, zoom = 10)%>%
  addPolygons(data = berlin_spdf,
              fillColor = "#CBECCB",
              fillOpacity = 0.9,
              weight = 0.2,
              smoothFactor = 0.2)
