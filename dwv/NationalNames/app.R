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
library(ggthemes)
<<<<<<< HEAD
#names <- read_csv('/Users/loganvj/Documents/NationalNames.csv')
names <- read_csv('/Users/loganjensen/Downloads/NationalNames.csv')
=======
names <- read_csv('C:/Users/mormondude888/Documents/NationalNames.csv')
#names <- read_csv('/Users/loganjensen/Downloads/NationalNames.csv')
>>>>>>> bfcd7869de51ec446d6246db0a5ada0e72eb4b2e

# Define UI for application that draws a histogram

ui <- fluidPage(
  titlePanel("National Names"),
  
  sidebarPanel(
    textInput(inputId = 'lit', label = 'Desired Name', value = 'Logan')), 
  
  mainPanel(plotOutput('histogramboy'), 
            tags$style(type="text/css",
              ".shiny-output-error { visibility: hidden; }",
              ".shiny-output-error:before { visibility: hidden; }")))

server <- function(input, output) {
  
  dataInput <- reactive({
    names %>% filter(Name == input$lit)
  })
  
  
  output$histogramboy <- renderPlot(
    
    dataInput() %>%
      filter(Gender == 'M') %>%
      ggplot() +
      geom_bar(mapping = aes(x = Year, y = Count, fill = Gender), stat = 'identity') +
      scale_fill_manual(values = c('dodgerblue')) +
      theme(text = element_text(size = 20),
            panel.background = element_rect(fill = 'white', color = 'grey')) +
      facet_wrap(~Gender)
    
  )
  
"""  output$histogramgirl <- renderPlot(
    
    dataInput() %>%
      filter(Gender == 'F') %>%
      ggplot() +
      geom_bar(mapping = aes(x = Year, y = Count, fill = Gender), stat = 'identity') +
      scale_fill_manual(values = c('pink')) +
      theme(text = element_text(size = 20),
            panel.background = element_rect(fill = 'white', color = 'grey')),    
    height = 400
    
  )
  """
  
#  output$histo1 <- renderPlot(
 #   
#    dataInput() %>%
#      filter(Gender == 'F') %>%
#      ggplot() +
#      geom_bar(mapping = aes(x = Year, y = Count), stat = 'identity', fill = 'pink') +
#      scale_fill_manual(values = c('pink')) +
#      theme(text = element_text(size = 20),
#            panel.background = element_rect(fill = 'white', color = 'grey')),
#    width = 550,
#    height = 400
    
#  )
  
}

           
# Run the application 
shinyApp(ui = ui, server = server)

