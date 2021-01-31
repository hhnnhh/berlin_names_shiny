#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)


button_color_css <- "
#DivCompClear, #FinderClear, #EnterTimes{
/* Change the background color of the update button
to blue. */
background: DodgerBlue;

/* Change the text size to 15 pixels. */
font-size: 15px;
}"

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    navbarPage("Berlin Names", theme = shinytheme("lumen"),
               tabPanel("Name Trend", fluid = TRUE, icon = icon("wave-square"),
                        tags$style(button_color_css),


    # Application title
    titlePanel("Trend from 2012 to 2019"),
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            
            # checkboxGroupInput(inputId = "GenderId",
            #                    label = "Select Gender(s):",
            #                    choices = c("Male" = "m", "Female" = "w"),
            #                    selected = "w"),
            
            selectInput("names",
                        "First names:",
                        choices=unique(df$vorname),
                        selected = NULL,
                        multiple = FALSE, 
                        selectize = TRUE),
        
            
            actionButton('select', 'Select'),
            
            ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot"),

        ))
    ),


tabPanel("Kiez Popularity", fluid = TRUE, icon = icon("globe"),
         tags$style(button_color_css),
         
         
         # Application title
         titlePanel("Frequency of Name by Kiez"),
         
         # Sidebar with a slider input for number of bins
         sidebarLayout(
             sidebarPanel(
                 # checkboxGroupInput(inputId = "GenderId",
                 #                    label = "Select Gender(s):",
                 #                    choices = c("Male" = "m", "Female" = "w"),
                 #                    selected = "w"),
                 
                 selectInput("names2",
                             "First names:",
                             choices=unique(df$vorname),
                             selected = NULL,
                             multiple = FALSE, 
                             selectize = TRUE),
                 
                 
                 actionButton('select2', 'Select'),
                 
             ),
             
             # Show a plot of the generated distribution
             mainPanel(
                 withSpinner((plotOutput("berlin")),color = getOption("spinner.color", default = "#D3D3D3")),
                 tableOutput("berlix"),
                 tableOutput("berliy")
                 
             ))
)#)
#))
)))