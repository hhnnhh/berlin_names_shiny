#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)          # for shiny app 
library(shinythemes)    # make it beautiful with theme "lumen"
library(rgdal)          # necessary for using readOGR
library(leaflet)        # necessary for building map
library(wordcloud)      # necessary for wordcloud
library(data.table)     # loading data with progress bar
library(bslib)          # customizing style with shinythemes, make css info available for PLOTS!
theme = shinythemes::shinytheme("lumen") # see also: https://talks.cpsievert.me/20200715/#26
shinyOptions(bslib = TRUE)
bs_global_theme()
thematic::thematic_shiny(font = "auto")


 # thematic_on(
 #   font = "auto"
 # )


#berlin_spdf=readOGR("data/map2", layer="berliner_bezirke",use_iconv = TRUE, encoding = "UTF-8")
#df <- readRDS("data/finaldf.rds")


button_color_css <- "
#DivCompClear, #FinderClear, #EnterTimes{
/* Change the background color of the update button
to blue. */
background: DodgerBlue;

/* Change the text size to 15 pixels. */
font-size: 15px;
}"

tags$script(src = "myscript.js")
# Define UI for application 
ui <- fluidPage( 
  




  
    navbarPage("Berlin Names", theme = theme, #theme = shinytheme("lumen"),

######### front end for "Kiez Popularity tab"  
    

               tabPanel("The Kiez Personalities", fluid = TRUE, icon = icon("globe"),
                        tags$style(button_color_css),

                        sidebarLayout(
                            sidebarPanel(
                                titlePanel("Hot in the Hood?"),
                                h4("Popularity of names by Kiez"),
                                
                                p("The map shows the rank of the chosen name in the particular neighborhood or \"Kiez\" as we say in Berlin. By", strong("clicking the map"), "you can access more information regarding the name's total number and percentage."),
                                br(),
                                helpText("Either choose or type a name of your choice."),
                                
                                selectizeInput("names2",
                                            "First names:",
                                            choices=c(Choose = '', unique(df$vorname)),
                                            selected = NULL,
                                            #multiple = FALSE,
                                            #selectize = TRUE
                                            ),
                                

                                actionButton('select2', 'Select'),
                                #br(),

                                #h5("Time course of the name's popularity from 2012 to 2019:"),
                                plotOutput("trend", height = 280)
                                #tableOutput("view")

                            ),

                            # Show the final map (optional: results table)
                            mainPanel(
                                
                                htmlOutput("text2"), 
                                shinycssloaders::withSpinner((leafletOutput("berlin")),color = getOption("spinner.color", default = "#D3D3D3"))#,
                                #tableOutput("berliy") #if used, don't forget to put comma behind leafletOutput

                            ))
               ),

######### front end for "Frequent names" tab

                tabPanel("The Favorites", fluid = TRUE, icon = icon("cloud"),
                    tags$style(button_color_css),

                        sidebarLayout(
                            sidebarPanel(
                                titlePanel("Everybodys darling?"),
                                h4("Frequency by Kiez, gender, year"),
                                
                                p("The wordcloud shows the popularity of the names for the selected features. Bigger names are more popular."),
                                
                                #width = 2,
                                #selectInput(inputId="vorname", label="vorname", choices=unique(df$vorname), selected = NULL, multiple = FALSE),
                                
                                selectizeInput(
                                    inputId = "kiezId",
                                    label = "Kiez",
                                    choices = c(Choose = '', unique(df$Kiez))#,
                                    #selected = NULL,
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
                                #h5("Bigger names were more frequent."),
                            ),
                            
                            #check:
                            # https://stackoverflow.com/questions/50800892/change-rendertable-title-in-shiny
                            mainPanel(
                              #textOutput("text1"), wordcloud2Output("plot", width = "50%", height = "700px"),  
                              textOutput("text1"), plotOutput("plot",height = 700)
                            )
                        )
                        
                        ),
    

tabPanel("The Special Ones", fluid = TRUE, icon = icon("star"),
                                  tags$style(button_color_css),

             # Application title
             # Sidebar with drop down menu
             sidebarLayout(
                 sidebarPanel(
                     titlePanel("One and only?"),
                     h4("Unique names in Berlin"),
                     p("Each time you press the button, you'll see a map with a new selection of first names that were", em("given only once"),"in that particular year in the Kiez."),
                     
                      helpText("choose a year"),

                     sliderInput(
                       inputId = "yearid2",
                       label = "Year", min = min_year, max = max_year,
                       round = 2,
                       step = 1,
                       sep = "",
                       value = 2018


                     ),


                     #actionButton('select3', 'Select'),
                     br(),
                     actionButton('refresh', 'select & refresh'),
                     h5("Refresh to see a new sample of unique names."),
                     ),

                 # Show a line graph with name trend
                 mainPanel(

                   #textOutput("text2"),htmlOutput("r")
                   shinycssloaders::withSpinner((leafletOutput("berlin2")),color = getOption("spinner.color", default = "#D3D3D3")),
                   #tableOutput("berliy")
                 ))
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
                       p("The source code for this Shiny app is available on", a("github", href = "https://github.com/hhnnhh/berlin_names_shiny"), "."))
                    
                    #hr(),
                    
             ),

)))

)#)

