
library(data.table)
library(rgdal)

# slow app? 
# https://github.com/rstudio/shiny-server/issues/456

## progress bar source: 
#https://www.mazsoft.com/blog/post/2018/01/01/show-progress-bar-when-pre-loading-data-in-shiny-app

df<-NULL
berlin_spdf <- NULL

readData <- function(session, berlin_spdf, df) {
  progress <- Progress$new(session)
  progress$set(value = 0, message = 'Loading...')
  berlin_spdf <<- readOGR("data/map2", layer="berliner_bezirke",use_iconv = TRUE, encoding = "UTF-8")
  progress$set(value = 0.25, message = 'Loading...')
  df <<- readRDS("data/finaldf.rds")
  progress$set(value = 1, message = 'Loading...')
  progress$close()
}

#df <<- load("data_archive/finaldf.Rdata")
min_year <- 2012 #min(df$year)
max_year <- 2019 #max(df$year)

