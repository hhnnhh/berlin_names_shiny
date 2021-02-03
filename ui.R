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
#library(rgdal)          # necessary for using readOGR
library(leaflet)
library(wordcloud)



button_color_css <- "
#DivCompClear, #FinderClear, #EnterTimes{
/* Change the background color of the update button
to blue. */
background: DodgerBlue;

/* Change the text size to 15 pixels. */
font-size: 15px;
}"

# Define UI for application 
shinyUI(fluidPage(

    navbarPage("Berlin Names", theme = shinytheme("lumen"),

######### front end for "Kiez Popularity tab"  

               tabPanel("Kiez Personalities", fluid = TRUE, icon = icon("globe"),
                        tags$style(button_color_css),

                        sidebarLayout(
                            sidebarPanel(
                                titlePanel("Hot in the Hood?"),
                                h4("Popularity of names by Kiez"),

                                helpText("You can choose a name from the selection or type a name of your choice."),
                                selectInput("names2",
                                            "First names:",
                                            choices=unique(df$vorname),
                                            selected = "Mia",
                                            #multiple = FALSE,
                                            #selectize = TRUE
                                            ),


                                actionButton('select2', 'Select'),
                                br(),
                                br(),
                                helpText("Select name and click on map for more information!"),
#),
                               # plotOutput("plot", height = 200),
                            ),

                            # Show the final map (optional: results table)
                            mainPanel(
                                
                                htmlOutput("text2"), 
                                shinycssloaders::withSpinner((leafletOutput("berlin")),color = getOption("spinner.color", default = "#D3D3D3"))#,
                                #tableOutput("berliy") #if used, don't forget to put comma behind leafletOutput

                            ))
               ),

######### front end for "Frequent names" tab

                tabPanel("Popular Names", fluid = TRUE, icon = icon("cloud"),
                    tags$style(button_color_css),

                        sidebarLayout(
                            sidebarPanel(
                                titlePanel("Everybodys darling?"),
                                h4("Frequency by Kiez, gender, year."),
                                #helpText("Displaying frequency of first names in Berlin by Kiez, gender, and year. source: Berlin open data."),
                                
                                #width = 2,
                                #selectInput(inputId="vorname", label="vorname", choices=unique(df$vorname), selected = NULL, multiple = FALSE),
                                
                                selectInput(
                                    inputId = "kiezId",
                                    label = "Kiez",
                                    choices = unique(df$Kiez),
                                    selected = "Charlottenburg-Wilmersdorf"
                                ),
                                
                                helpText("Data only available for binary gender.."),
                                selectInput(
                                    inputId = "genderId",
                                    label = "Gender",
                                    choices = c("female"="w","male"="m"),
                                    #choices = unique(df$geschlecht),
                                    selected = "female"
                                ),
                                
                                
                                sliderInput(
                                    inputId = "yearId",
                                    label = "Year", min = min_year, max = max_year,
                                    round = 2, 
                                    step = 1,
                                    sep = "",
                                    value = 2018
                                    
                                    
                                ),
                                helpText("Position data only available since 2017"),
                                conditionalPanel(condition="input.yearId>2016",
                                                 selectInput(
                                                     inputId = "position", 
                                                     label = "Choose position of name:",
                                                     choices = c("1","2","3","4","5","6","7"),
                                                     selected ="1")
                                ),
                                
                                br(), 
                                
                                actionButton('select', 'Select'),
                                
                            ),
                            
                            #check:
                            # https://stackoverflow.com/questions/50800892/change-rendertable-title-in-shiny
                            mainPanel(
                                textOutput("text1"), plotOutput("plot"),
                            )
                        )
                        
                        ),
    

# ######### front end for "Name Trend tab"
# 
#                tabPanel("Trendy or not?", fluid = TRUE, icon = icon("wave-square"),
#                         tags$style(button_color_css),
# 
# 
#     # Application title
#     # Sidebar with drop down menu
#     sidebarLayout(
#         sidebarPanel(
#             titlePanel("Coming or going?"),
#             h4("Trend from 2012 to 2019"),
#             
#             # checkboxGroupInput(inputId = "GenderId",
#             #                    label = "Select Gender(s):",
#             #                    choices = c("Male" = "m", "Female" = "w"),
#             #                    selected = "w"),
#             
#              helpText("You can choose a name from the selection or type a name of your choice."),
#             
#             selectInput("names",
#                         "First names:",
#                         choices=unique(df$vorname),
#                         selected = "Wolke",
#                         multiple = FALSE, 
#                         selectize = TRUE),
#         
#             
#             actionButton('select', 'Select'),
#             
#             ),
# 
#         # Show a line graph with name trend
#         mainPanel(
#             plotOutput("distPlot"),
# 
#         ))
#     ),
# 
# 
# 

######### front end for "About me - tab"


tabPanel("About", fluid = TRUE,
         fluidRow(
             column(6,
                    #br(),
                    h4(p("About the Project")),
                    h5(p("This project is intended to give an insight into the variety of names given to children in Berlin within the last years, to show the most frequent names and the preferences of parents in particular Berlin Kiezes. Play around and have fun!")),
                    br(),
                    h5(p("The project began as an attempt to combine my interest in names with the intention to practice R, a programming language used primarily for analyzing and reporting data.  It has two components.  The first is this app, which queries a dataset to return information in the form of plots, data tables etc.  The second is the dataset itself, which I assembled by tying together information from the sources below. The dataset was provided by", a("Berlin Open Data", href = "https://daten.berlin.de/tags/vornamen"),".")),
                    br(),
                    h5(p("I hope you find it interesting and/or useful.  Any comments or questions are welcome at info-at-hannahbohle-dot-de."),
                       p("The source code for this Shiny app is available ", a("on github", href = "https://github.com/hhnnhh"), "."))
                    
                    #hr(),
                    
             ),

)))

))