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
courses <- read_xlsx('C:/Users/loganvj/Documents/Courseformatting.xlsx') %>%
  filter(College != 'Educationa and Human Development' & College != 'Academic')
numbers <- data_frame(num = c(seq(0, 100)))
  

  
# Define UI for application that draws a histogram
ui <- fluidPage(
  
   # Application title
   titlePanel("Estimated Sections Needed"),
   
   # Sidebar with a slider input for number of bins 
      sidebarPanel(uiOutput('college'),
      uiOutput('depart'),
      uiOutput('course'),
      uiOutput('semester'),
      uiOutput('students')),
      
      
      # Show a plot of the generated distribution
      mainPanel(
         tableOutput('counts'),
         tableOutput('sections')
      )
   )


# Define server logic required to draw a histogram
server <- function(input, output) {
  
   output$semester <- renderUI({
     selectInput(
       inputId = 'sem',
       label = 'Select Semester',
       choices = unique(courses$Semester)
     )
   })
   
   output$students <- renderUI({
     selectInput(
       inputId = 'studs',
       label = 'Select Number of Students per Section',
       choices = numbers$num
     )
   })

  
   output$college <- renderUI({
     selectInput(
       inputId = 'coll',
       label = 'Select College',
       choices = unique(courses$College)
     )
   })
   
   output$depart <- renderUI({
     available <- courses %>% filter(College == input$coll)
   
   selectInput(inputId = 'dep',
               label = 'Choose Department',
               choices = unique(available$Department))
   })
   
   output$course <- renderUI({
     
     newavailable <- courses %>% filter(College == input$coll & Department == input$dep)
     
     selectInput(inputId = 'course1',
                 label = 'Choose Course',
                 choices = unique(newavailable$Course))
   })
   
   output$counts <- renderTable({
     cool <- courses %>% filter(College == input$coll & Department == input$dep & Course == input$course1 & Semester == input$sem) %>% select(Classification, Value)
     
     head(cool, n = 5)
   })
   
   output$sections <- renderTable({
     cool <- courses %>% 
       filter(College == input$coll & Department == input$dep & Course == input$course1 & Semester == input$sem & Classification == 'Total') %>% 
       mutate(Value = as.numeric(Value)/as.numeric(input$studs)) %>%
       select(Value)
     
     colnames(cool)[1] <- 'Sections Neded'
     
     head(cool, n = 1)
   })
   

   
}

# Run the application 
shinyApp(ui = ui, server = server)

