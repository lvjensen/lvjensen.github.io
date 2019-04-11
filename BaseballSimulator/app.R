#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(imputeTS)
library(XML)
library(RCurl)

teams <- c("ARI", "ATL", "BAL", "BOS", "CHC", "CIN","CLE", "COL", "CWS", "DET", "HOU", 
           "KC", "LAA", "LAD", "MIA", "MIL", "MIN", "NYM", "NYY", "OAK", "PHI", "PIT", 
           "SD", 'SEA', 'SF', 'STL', 'TB', 'TEX', 'TOR', 'WSH')

year <- c(1998:2018)
batters1 <- c("Empty")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Simulator"),
   
   # Sidebar with a slider input for number of bins 
   fluidRow(
     column(3,
            selectInput('team1', label='Choose First Team', choices = teams)),
     column(3, 
            selectInput('team1year', label='Choose Team 1\'s Year', choices = year)),
     column(3,
            selectInput('team2', label='Choose Second Team', choices = teams)),
     column(3, 
            selectInput('team2year', label='Choose Team 2\'s Year', choices = year))
   ),
   fluidRow(
     column(3,
            uiOutput('teambat1'),
            uiOutput('teambat2'),
            uiOutput('teambat3'),
            uiOutput('teambat4'),
            uiOutput('teambat5'),
            uiOutput('teambat6'),
            uiOutput('teambat7'),
            uiOutput('teambat8'),
            uiOutput('teambat9'),)
   ),
      
      # Show a plot of the generated distribution
      mainPanel(
         tableOutput("table1")
      )
   )


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  team1bat <- reactive ({
    url <- paste("https://www.baseball-reference.com/teams/", input$team1, "/", input$team1year ,".shtml", sep='')
    
    data <- url %>%
      getURL() %>%
      readHTMLTable() %>%
      .[[1]] %>%
      as.tibble() %>%
      filter(Rk != 'Rk') %>%
      mutate(Name = str_remove_all(Name, '\\#'), Name = str_remove_all(Name, '\\*'))
    
    return(data)
  })
  
  batoptions <- reactive({
    data <- team1bat()
    return(data[,3])
  })
  
  output$teambat1 <- renderUI({
    selectInput('team1bat2', label = 'Get Lit', choices = batoptions())
  })
   
   output$table1 <- renderTable({
      data <- team1bat()
      data
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

