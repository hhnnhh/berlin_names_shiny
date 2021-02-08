
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
#     useful tip for long loading time issue
#     https://github.com/rstudio/shiny-server/issues/456


library(shiny)            # for web app
library(dplyr)            # for data wrangling
library(shinycssloaders)  # for spinner when waiting to load
#options(tigris_use_cache = TRUE)
library(tigris)           # for geo_join (join spacial data with df)
library(leaflet)          # for interactive map
library(rgdal)            # for loading spacial data with readOGR
library(RColorBrewer)     # nice wordcloud color
library(wordcloud)        # make wordcloud
#library(profvis)         # for checking performance of app
library(data.table)       # loading data with progress bar
#devtools::install_github("lchiffon/wordcloud2")
#library(wordcloud2)
library(splitstackshape)  # drawing random samples, with one "vorname" for each Kiez ("unique name map")



# frequencies and names from Berlin Open Data
# map data with polygons from https://github.com/funkeinteraktiv/Berlin-Geodaten
# loading data without progress bar: 
# df <- readRDS("data/finaldf.rds")
# berlin_spdf=readOGR("data/map2", layer="berliner_bezirke",use_iconv = TRUE, encoding = "UTF-8")


 
server <- function(input, output, session) {

  #ShinyServer(function(input, output, session) {

  if(is.null(df)){
    readData(session,  berlin_spdf,df)
  }
  
  bmap<- leaflet() %>%
    setView(13.41053,52.52437, zoom = 10)%>%
    addPolygons(data = berlin_spdf,
                fillColor = "#CBECCB",
                fillOpacity = 0.9,
                weight = 0.2,
                smoothFactor = 0.2)

######### back end for "Kiez popularity tab" 
  
  #render template of map before calling reactive function (global.R)
  output$berlin <- renderLeaflet(bmap)
  output$berlin2 <- renderLeaflet(bmap)

# large number of options slow down app. --> save on server!
# https://stackoverflow.com/questions/38438920/shiny-selectinput-very-slow-on-larger-data-15-000-entries-in-browser
  updateSelectizeInput(session, "names2",  choices = unique(df$vorname), server = TRUE)
    observe({
   
  # filter by name input and select some columns
        filteredName2 <- reactive({
        req(input$names2)
         df %>%
           filter(vorname == input$names2)#%>%
         # select(vorname,Kiez,summe,percentage,rank) 
           
     })
        
        #### Trend plot small below select button 
        
        # group remaining data by year and sum total number of names
        filteredYear <- reactive({
          filteredName2() %>%
            group_by(year)%>%
            mutate(s=sum(anzahl))
        })
        
        # # final output data
        # filteredFinal <- reactive({
        #   filteredYear()
        # })
        

        
  # join filtered data set with spacial data
     filteredFinal2 <- eventReactive(input$select2, {
       geo_join(berlin_spdf, filteredName2(), "name", "Kiez")

     })
     

       popup_sb <- input$names2
       pal <- input$names2 
       


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
     
# Text will help explain what user is seeing 
     
          output$text2 <- renderText({
            paste("<strong>",input$names2,"</strong>: Rank, percentage and total number for each Kiez in Berlin between 2012 and 2019.")
          })

# these are only the parts of the map that vary with reactive content
# main part / template of the map ("bmap") is rendered at the beginning of server.R
          
     leafletProxy("berlin", session) %>%
       clearControls() %>%
       clearShapes() %>%
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

     # output$view <- renderTable({
     #   filteredYear()$s
     # })
     
     # plot output --> line graph
     output$trend <- renderPlot({
     plot(filteredYear()$s~filteredYear()$year, type="b" ,  lwd=2 , col=rgb(0.1,0.7,0.1,0.8) , ylab="total names per year" , xlab="year" ,lty=1, bty="l" , pch=20 , cex=2)
     }) # close render plot
     
     }) #close observe function



     
#display table data --> helpful for debugging purposes
       # output$berliy <- renderTable({
       #   filteredFinal2()
       # })



######### back end for "Frequent Names Cloud"
    
    updateSelectizeInput(session, "kiezId",  choices = unique(df$Kiez), server = TRUE)
      
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
          paste("Selection of names that were most frequent in", input$yearId,"in", input$kiezId,".")
        })
        validate(
          need(fully_filtered()$vorname, 'No names available for this selection.')
        )
        p<-wordcloud(words = fully_filtered()$vorname, freq = fully_filtered()$anzahl, colors=brewer.pal(8,"BrBG"),min.freq = 2, max.words=200, random.order=FALSE, rot.per=0.35,scale=c(3.5,0.25))
        #renderWordcloud2(wordcloud2(data = fully_filtered()$vorname,colors=brewer.pal(8,"BrBG")))
        }) # close output$plot
      
      
######### unique names map
      
    #  output$berlin2 <- renderLeaflet(bmap)
 #       unique <- df%>%
 #         filter(summe==1)%>%
 #         select(year,Kiez,vorname)
 # dim(unique)

      # one_filtered <- reactive({
      #   df %>%
      #     filter(summe == 1) %>%
      #     select(year,Kiez,vorname)
      # })

      #prepare data by filtering only unique first names
      
      unique <- df %>%
        filter(summe == 1) %>%
        select(year,Kiez,vorname)
      
      # user selects year with slider
      
      
      filteredyear<- reactive({
         unique %>%
          filter(year == input$yearid2)
      })
      
      # data for selected year is joined with map data

       unique_final <- reactive({
         inner_join(berlin_spdf@data, filteredyear(), by = c("name"= "Kiez"))
         #geo_join(berlin_spdf@data, filteredyear(), "name", "Kiez")
       })
      
      # when action button "select" is clicked, one sample of first names is chosen
    #   samp <- eventReactive(input$select3, {
    # #stratified(filteredyear(), "Kiez", 1)
    #     stratified(unique_final(), "name", 1)
    # })
    #   
      # when action button "refresh" is clicked, a new sample of first names is chosen
            samp <- eventReactive(input$refresh, {
        #stratified(filteredyear(), "Kiez", 1)
              stratified(unique_final(), "name", 1)
              
      })
            
           
      
    # first names are forwarded to label ??
      label2<- reactive({
        paste0(as.character(samp()$vorname))#,
              # "<b>",as.character(samp()$name))
            # "</strong>"," in ",as.character(samp()$Kiez))
      })
      
      
        
      # label <-reactive({
      #   samp() %>%
      #     filter(year == input$yearid2)
      #   })
      # 
      
      # 
      # unique_final <- eventReactive({
      # paste0(as.character(unique_final()$vorname),
      #        " (",as.character(unique_final()$name),")")
      # })


       # output$berlin2 <- renderLeaflet(leaflet() %>%
       #                                  setView(13.41053,52.52437, zoom = 10)%>%
      # leafletProxy("berlin2", session) %>%
      #   clearControls() %>%
      #   clearShapes() %>%
      #                                   addPolygons(data = berlin_spdf,
      #                                               fillColor = "#CBECCB",
      #                                               fillOpacity = 0.9,
      #                                               weight = 0.2,
      #                                               smoothFactor = 0.2,
      #                                               #labelId= unique_final()$name,
      #                                               label = ~label2(),
      #                                               labelOptions=labelOptions(permanent=TRUE,textsize = 14),
      #                                               layerId = berlin_spdf$name
      #                                               )#,
      observe({
       leafletProxy("berlin2", session) %>%
          clearControls() %>%
          clearShapes() %>%
         addPolygons(data = berlin_spdf,
                     fillColor = "#CBECCB",
                     fillOpacity = 0.9,
                     weight = 0.2,
                     smoothFactor = 0.2,
                     label = ~label2(),
                     labelOptions=labelOptions(permanent=TRUE,textsize = 14),
                     highlight = highlightOptions(
                       weight = 5,
                       color = "#666",
                       fillOpacity = 0.7,
                       bringToFront = TRUE))
      
       output$berliy <- renderTable({
         samp()
         #unique_final()
       })
       
       })
       
        # setView(13.41053,52.52437, zoom = 10)%>%
        # addPolygons(data=berlin_spdf)
        #addTiles()# %>%
        #addLabelOnlyMarkers(~long, ~lat, label = ~as.character(sample(unique_final()$vorname,5))#,
    #  labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T))

      
      # output$r <-renderUI({
      #     validate(
      #         need(filteredyear()$vorname, 'No names available for this selection.')
      #             )
      # 
      #   output$text2 <- renderText({
      #             paste("Selection of names that were unique in", input$yearid2)
      #                             })
      #           HTML(as.character(sample(filteredyear()$vorname,5), sep="<br/>"))
      # })

      
      # ######### back end for "Name Trend tab"   
#       
#     # filter by name
#       filteredName <- reactive({
#         df %>%
#           filter(vorname == input$names) 
#       })
#     
#      # group remaining data by year and sum total number of names 
#       filteredYear <- reactive({
#         filteredName() %>%
#           group_by(year)%>%
#           summarise(s=sum(anzahl))
#       })
#       
#     # final output data  
#       filteredFinal <- eventReactive(input$select, {
#         filteredYear()
#       })
#       
#     # plot output --> line graph  
#       output$distPlot <- renderPlot({
#         output$view <- renderTable({
#           filteredFinal()
#         })
#         plot(filteredFinal()$s~filteredFinal()$year, type="b" , lwd=2 , col=rgb(0.1,0.7,0.1,0.8) , ylab="number of names per year" , xlab="year" , bty="l" , pch=20 , cex=2)
#         abline(h=seq(0,100,10) , col="grey", lwd=0.8)
#       }) # close render plot

}#) # close reactive server instance
