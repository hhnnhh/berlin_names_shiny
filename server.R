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
library(ggplot2)
library(RColorBrewer)
library(shinycssloaders)


df <- read.csv("data/berlin.csv")
#df <- df %>% select(vorname, anzahl, year, Kiez,geschlecht)
bm <- read.csv("data/berlinmapdata.csv")
#df$year <-as.Year(df$year)
#str(df)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    # filteredGender <- reactive({
    #     df %>%
    #         filter(geschlecht == input$GenderId)
    # })
    
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
     
     
     filteredName2 <- reactive({
         df %>%
             filter(vorname == input$names2) 
     })
     
     
     filteredKiez <-reactive({
         filteredName2() %>%
             group_by(Kiez)%>%
             summarise(f=sum(anzahl))
    })
     
     # filteredShort <-reactive({
     #     filteredKiez() %>%
     #         group_by(Kiez)%>%
     #         summarise(ff=sum(f))
     # })
     
     filteredFinal2 <- eventReactive(input$select2, {
         left_join(bm,filteredKiez())
         #filteredShort()
         #filteredKiez()
         
     })
     
     
       # output$berlix <- renderTable({
       #     input$select2
       #     isolate(
       #     filteredShort()
       #     )
       # })
       
       output$berliy <- renderTable({
               filteredFinal2()
       })
     
      output$berlin <- renderPlot({
          input$select2
          isolate(
          ggplot(data=filteredFinal2(), mapping = aes(x=long,y=lat,group=group,fill=f))+
              geom_polygon(color="grey90",size=0.1)+
              coord_map(projection="albers",lat0=13,lat1=53)+
              scale_fill_gradient2(low="#d3d3d3",
                                   mid=scales::muted("#767171"),
                                   high=rgb(0.1,0.7,0.1,0.8))
          #guides(fill=FALSE)
          )

      })

})
