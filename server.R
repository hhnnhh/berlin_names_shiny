#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#     http://shiny.rstudio.com/
#
#     Example:
#     https://gpilgrim.shinyapps.io/SwimmingProject-Click/?_ga=2.59410990.242922474.1612026216-878511382.1609948441
#


library(shiny)          # for web app
library(dplyr)          # for data wrangling
library(shinycssloaders)# for spinner when waiting to load
#options(tigris_use_cache = TRUE)
library(tigris)         # for geo_join (join spacial data with df)
library(leaflet)        # for interactive map
library(rgdal)          # for loading spacial data with readOGR
library(wordcloud)


# df <- read.csv("data/berlin.csv") # data for name trend and wordcloud
# map_df <- read.csv("data/map_df.csv") # data for rank,%,frequency in map
# map data with polygons from https://github.com/funkeinteraktiv/Berlin-Geodaten
df <- read.csv("data/berlin.csv") # data for name trend and wordcloud
map_df <- read.csv("data/map_df.csv") # data for rank,%,frequency in map
berlin_spdf=readOGR("data/map2", layer="berliner_bezirke",use_iconv = TRUE, encoding = "UTF-8")





shinyServer(function(input, output) {
  


######### back end for "Kiez popularity tab"   

  # filter by name input and select some columns
        filteredName2 <- reactive({
         map_df %>%
           filter(vorname == input$names2)%>%
           select(vorname,Kiez,summe,percentage,rank)
     })

  # join filtered data set with spacial data
     filteredFinal2 <- eventReactive(input$select2, {
       geo_join(berlin_spdf, filteredName2(), "name", "Kiez")

     })

   #reactive pop-up on map will display RANK, PERCENTAGE, TOTAL
     popup_sb <- reactive({
       paste0("<strong>",as.character(filteredFinal2()$vorname),
              "</strong>"," in ",as.character(filteredFinal2()$name),
              ":<br><strong>Rank: </strong>", as.character(filteredFinal2()$rank),
              "<br /><strong>Percentage: </strong>", as.character(filteredFinal2()$percentage),
              "%","<strong><br />Total: </strong>", as.character(filteredFinal2()$summe))
     })

    # reactive color dependent on rank (lower rank = darker green)
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

     #start Leaflet interactive map output
      output$berlin <- renderLeaflet({
        
        output$text2 <- renderText({
          paste("<strong>",input$names2,"</strong>: Rank, percentage and total number for each Kiez in Berlin between 2012 and 2019.<br><strong>Click on Kiez!</strong>")
        })
        
         input$select2 # wait for action button click before rendering map w/ isolate
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

          ) #close isolation

      }) # close leaflet output

######### back end for "Frequent Names Cloud"
      
      filtered_kiez <- reactive({
        df %>%
          filter(Kiez == input$kiezId)
      })
      
      filtered_gender <- reactive({
        filtered_kiez() %>% 
          filter(geschlecht == input$genderId)
      })
      
      
      filtered_year <- reactive({
        if (input$yearId>2017){
          filtered_gender() %>% 
            filter(year == input$yearId & position == input$position)
        }
        else{    filtered_gender() %>% 
            filter(year == input$yearId)}
      })
      
      one_filtered <- reactive({
        filtered_year() %>% 
          filter(filtered_year()$anzahl == 1)
      })
      
      fully_filtered <- eventReactive(input$select, {
        filtered_year()
      })
      
      output$plot <- renderPlot({
        
        output$text1 <- renderText({
          paste("Selection of names that were most frequent in", input$yearId,"in", input$kiezId)
        })
        validate(
          need(fully_filtered()$vorname, 'No names available for this selection.')
        )
        p<-wordcloud(words = fully_filtered()$vorname, freq = fully_filtered()$anzahl, colors=brewer.pal(8,"BrBG"),min.freq = 2, max.words=200, random.order=FALSE, rot.per=0.35,scale=c(3.5,0.25))
      })
 
######### back end for "Name Trend tab"   
      
    # filter by name
      filteredName <- reactive({
        df %>%
          filter(vorname == input$names) 
      })
    
     # group remaining data by year and sum total number of names 
      filteredYear <- reactive({
        filteredName() %>%
          group_by(year)%>%
          summarise(s=sum(anzahl))
      })
      
    # final output data  
      filteredFinal <- eventReactive(input$select, {
        filteredYear()
      })
      
    # plot output --> line graph  
      output$distPlot <- renderPlot({
        output$view <- renderTable({
          filteredFinal()
        })
        plot(filteredFinal()$s~filteredFinal()$year, type="b" , lwd=2 , col=rgb(0.1,0.7,0.1,0.8) , ylab="number of names per year" , xlab="year" , bty="l" , pch=20 , cex=2)
        abline(h=seq(0,100,10) , col="grey", lwd=0.8)
      }) # close render plot

}) # close reactive server instance
