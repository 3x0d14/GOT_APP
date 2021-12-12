library(shiny)
library(dplyr)
library(sf)
library(ggplot2)
library(leaflet)
library(plotly)
library(shinyjs)
library(shinyBS)
library(padr)
library(png)






mainpage <- function() {
  # Application title
  titlePanel("GAME OF THRONES: deaths by episode")
  
  # Sidebar with a slider input for number of bins 
  
  column(12,
         sidebarLayout(
           
           sidebarPanel (
             sliderInput("episodeID","Slide to chose an episode:",s_min,s_max,s_min),
             h3("Totale number of deaths in this episode:"),
             textOutput("dead_e"),
             tags$head(tags$style("#dead_e{color:red;font-size:1.5em}")),
             h3("Cumulated deaths over past episodes:"),
             textOutput("dead_c"),
             tags$head(tags$style("#dead_c{color:red;font-size:1.5em}"))
           ),
           
           
           
           # Show a plot of the generated distribution
           mainPanel(
             
             h4("Number of deaths in the chosen episode:"),
             plotOutput("deadEplot",width = "100%"),
             h4("Cumulated deaths over past episodes:"),
             plotOutput("deadCplot")
             
           )
           
         )
  )
  
}
#----------------------------------end characters' deaths page


# ----------------------------------characters' info page

charpage <- function(){
  sidebarLayout(
    
    # ----------------------------------
    
    #the side panel has a selectizeInput to chose a character
    
    sidebarPanel(
      selectizeInput(
        inputId = "characters",
        label = h3("select a character:"),
        choices = c(unique(char_file$name)),
        options = list(maxOptions = all)
        #size = 20,selectize = FALSE, # for selectInput(
        #selected = "ALL" # for selectInput(
      ),
    ),
    
    # ----------------------------------
    
    # the main panel displays characters' information
    mainPanel(
      
      tags$style(
        "div .imagediv {
                background-color: #eaeaea;
                padding:100px;
                height:400px;

            }
            img {
                border-radius: 4px;
                padding: 5px;
               
            }
            h4 {
                display:inline;
            }
            #sex, #name, #house, #killedby{
                display:inline
            }
            "
      ),
      h3("character:"),
      tags$div(
        class = "imagediv", checked = NA,
        uiOutput("myImage"),
        tags$p(
          
          h3(style="display:inline; text-align:left",textOutput("name")),
          h3(style="display:inline; text-align:left",textOutput("sex")),
          h3(style="display:inline; text-align:left",textOutput("house")),
          h3(style="display:inline; text-align:left",textOutput("killedby")),
        )
      )
    )
  )
}

#----------------------------------end characters' info page


#----------------------------------main character's duration per location page

locationpage <- function() {
  # Application title
  titlePanel("GAME OF THRONES: Duration of presence of each character per episode and Location")
  
  column(12,
         sidebarLayout(
           
           # ----------------------------------
           
           #the side panel has a selectizeInput to chose a character (main characters only)
           
           sidebarPanel(
             selectizeInput(
               inputId = "maincharacters",
               label = h3("select a character:"),
               choices = c("Jon Snow", "Tyrion Lannister","Daenerys Targaryen","Sansa Stark","Cersei Lannister","Arya Stark"),
               options = list(maxOptions = all)
               
             ),
             sliderInput("maincharepisodeID","Slide to chose an episode:",s_min,s_max,s_min)
             
           ),
           
           
           
           # ----------------------------------
           
           # Show a plot of the generated distribution
           mainPanel(
             tags$style(
               "
                h4 {
                    display:inline;
                }
                #mainchar, #mainchar_ep{
                    display:inline
                }
                "
             ),
             
             h4("Time of presence per location of :", textOutput("mainchar"),textOutput("mainchar_ep")),
             
             plotOutput("barchar",width = "100%"),
             
           )
           
         )
  )
  
}
#----------------------------------end main character's duration per location page



# ---------------------------------- UI----------------------------------

shinyUI(navbarPage(
  title = "GAME OF THRONES",
  theme = "style/style.css",
  footer = includeHTML("footer.html"),
  fluid = TRUE, 
  collapsible = TRUE,
  
  #----------------------------------
  # home page
  tabPanel("Home",
           includeHTML("home.html"),
           tags$script(src = "plugins/scripts.js")
  ),
  
  # ----------------------------------
  # tab panel 2 - deaths
  tabPanel("Death map",
           mainpage()
  ),
  
  # ----------------------------------
  # tab panel 3 - characters
  tabPanel("Character information",
           charpage()
  ),
  # ----------------------------------
  # tab panel 4 - Time presence per Location
  tabPanel("Time presence per Location",
           locationpage()
  ),
  
  
))