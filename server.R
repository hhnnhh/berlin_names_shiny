#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
# https://gpilgrim.shinyapps.io/SwimmingProject-Click/?_ga=2.59410990.242922474.1612026216-878511382.1609948441
#
getwd()

library(shiny)
library(dplyr)
#library(ggplot2)
#library(RColorBrewer)
library(shinycssloaders)
library(tigris)
library(leaflet)
library(rgdal)

df <- read.csv("data/berlin.csv") # data for name trend and wordcloud
map_df <- read.csv("data/map_df.csv") # data for rank,%,frequency in map
# map data with polygons from https://github.com/funkeinteraktiv/Berlin-Geodaten
berlin_spdf=readOGR("data/map2", layer="berliner_bezirke",use_iconv = TRUE, encoding = "UTF-8")



shinyServer(function(input, output) {
    
        filteredName2 <- reactive({
         map_df %>% 
           filter(vorname == input$names2)%>%
           select(vorname,Kiez,summe,percentage,rank)
     })
     

     filteredFinal2 <- eventReactive(input$select2, {
       geo_join(berlin_spdf, filteredName2(), "name", "Kiez")

     })
     
     popup_sb <- reactive({
       paste0("<strong>",as.character(filteredFinal2()$vorname),
              "</strong>"," in ",as.character(filteredFinal2()$name),
              ":<br><strong>Rank: </strong>", as.character(filteredFinal2()$rank),
              "<br /><strong>Percentage: </strong>", as.character(filteredFinal2()$percentage),
              "%","<strong><br />Total: </strong>", as.character(filteredFinal2()$summe))
     })
     
     pal <- reactive({
       colorNumeric(
         palette = "Greens",
         domain = filteredFinal2()$rank,
         reverse=TRUE # lower rank = darker green
       )
     })
     
    #display table data for debugging 
       # output$berliy <- renderTable({
       #   filteredFinal2()
       # })
     
      output$berlin <- renderLeaflet({
           input$select2
           isolate(

         leaflet() %>%
           setView(13.41053,52.52437, zoom = 10)%>%
               addPolygons(data = berlin_spdf,
                           fillColor = ~pal()(filteredFinal2()$rank),
                           fillOpacity = 0.9,
                           weight = 0.2,
                           smoothFactor = 0.2,
                            highlight = highlightOptions(
                              weight = 5,
                              color = "#666",
                              fillOpacity = 0.7,
                              bringToFront = TRUE),
                           popup = ~popup_sb()) %>%
                 addLegend(pal = pal(),
                           values = filteredFinal2()$rank,
                           position = "bottomright",
                           title = "rank")

          )

      })
      
      filteredName <- reactive({
        df %>%
          filter(vorname == input$names) 
      })
      
      filteredYear <- reactive({
        filteredName() %>%
          group_by(year)%>%
          summarise(s=sum(anzahl))
      })
      
      
      filteredFinal <- eventReactive(input$select, {
        filteredYear()
      })
      
      
      output$distPlot <- renderPlot({
        output$view <- renderTable({
          filteredFinal()
        })
        plot(filteredFinal()$s~filteredFinal()$year, type="b" , lwd=2 , col=rgb(0.1,0.7,0.1,0.8) , ylab="number of names per year" , xlab="year" , bty="l" , pch=20 , cex=2)
        abline(h=seq(0,100,10) , col="grey", lwd=0.8)
      })

})
