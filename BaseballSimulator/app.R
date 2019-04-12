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
            uiOutput('teambat9'))
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
      as.data.frame() %>%
      filter(Rk != 'Rk') %>%
      mutate(Name = str_remove_all(Name, '\\#'), Name = str_remove_all(Name, '\\*'))
    
    return(data)
  })
  
  team2bat <- reactive ({
    url <- paste("https://www.baseball-reference.com/teams/", input$team2, "/", input$team2year ,".shtml", sep='')
    
    data <- url %>%
      getURL() %>%
      readHTMLTable() %>%
      .[[1]] %>%
      as.data.frame() %>%
      filter(Rk != 'Rk') %>%
      mutate(Name = str_remove_all(Name, '\\#'), Name = str_remove_all(Name, '\\*'))
    
    return(data)
  })
  
  team2pitch <- reactive ({
    url <- paste("https://www.baseball-reference.com/teams/", input$team2, "/", input$team2year ,".shtml", sep='')
    
    data <- url %>%
      getURL() %>%
      readHTMLTable() %>%
      .[[2]] %>%
      as.data.frame() %>%
      filter(Rk != 'Rk') %>%
      mutate(Name = str_remove_all(Name, '\\#'), Name = str_remove_all(Name, '\\*'))
    
    return(data)
  })
  
  batoptions <- reactive({
    data <- team1bat()
    return(data[,3])
  })
  
  output$teambat1 <- renderUI({
    selectInput('team1bat1', label = 'Batter 1', choices = batoptions())
  })
  
  
  output$teambat2 <- renderUI({
    selectInput('team1bat2', label = 'Batter 2', choices = batoptions())
  })
  
  
  output$teambat3 <- renderUI({
    selectInput('team1bat3', label = 'Batter 3', choices = batoptions())
  })
  
  
  output$teambat4 <- renderUI({
    selectInput('team1bat4', label = 'Batter 4', choices = batoptions())
  })
  
  
  output$teambat5 <- renderUI({
    selectInput('team1bat2', label = 'Batter 5', choices = batoptions())
  })
  
  
  output$teambat6 <- renderUI({
    selectInput('team1bat2', label = 'Batter 6', choices = batoptions())
  })
  
  
  output$teambat7 <- renderUI({
    selectInput('team1bat2', label = 'Batter 7', choices = batoptions())
  })
  
  
  output$teambat8 <- renderUI({
    selectInput('team1bat2', label = 'Batter 8', choices = batoptions())
  })
  
  
  output$teambat9 <- renderUI({
    selectInput('team1bat2', label = 'Batter 9', choices = batoptions())
  })
  
  createteam1 <- reactive({
    data <- team1bat() %>%
      mutate(Name = str_remove_all(Name, '\\#'), Name = str_remove_all(Name, '\\*'))
    team1 <- data %>%
      filter(Name == input$team1bat1)
    team1 <- rbind(team1, data %>%filter(Name == input$team1bat2))
    return(team1)
  })
   
   output$table1 <- renderTable({
      team2pitch()
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

