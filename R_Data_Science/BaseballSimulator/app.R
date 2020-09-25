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

year <- c(1998:2019)
batters1 <- c("Empty")

setClass('batter', slots = list(name = 'character', average = 'numeric', obp = 'numeric', bb = 'numeric', single = 'numeric', double = 'numeric', triple = 'numeric', hr = 'numeric', base = 'numeric'))
setClass('pitcher', slots = list(name = 'character', obpa = 'numeric'))



ui <- fluidPage(
   
   # Application title
   titlePanel("Simulator"),
   
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
     column(6,
            p('Choose Lineup for Team 1'),
            uiOutput('teambat1'),
            uiOutput('teambat2'),
            uiOutput('teambat3'),
            uiOutput('teambat4'),
            uiOutput('teambat5'),
            uiOutput('teambat6'),
            uiOutput('teambat7'),
            uiOutput('teambat8'),
            uiOutput('teambat9'),
            p("Choose Starting Pitcher"),
            uiOutput('team1pitch')),
     column(6,
            p('Choose Lineup for Team 2'),
            uiOutput('team2bat1'),
            uiOutput('team2bat2'),
            uiOutput('team2bat3'),
            uiOutput('team2bat4'),
            uiOutput('team2bat5'),
            uiOutput('team2bat6'),
            uiOutput('team2bat7'),
            uiOutput('team2bat8'),
            uiOutput('team2bat9'),
            p("Choose Starting Pitcher"),
            uiOutput('team2pitch'))
   ),
      
      # Show a plot of the generated distribution
      mainPanel(
         tableOutput("table1"),
         textOutput('text1'),
         plotOutput('plot1'),
         plotOutput('plot2')
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
    
    for (i in 4:ncol(data)) {
      data[,i] <- as.numeric(as.character((data[,i])))
    }
    
    data1 <- data %>%
      filter(PA >= 150)
      
    
    return(data1)
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
    
    for (i in 4:ncol(data)) {
      data[,i] <- as.numeric(as.character((data[,i])))
    }
    
    data1 <- data %>%
      filter(PA >= 150)
    
    return(data1)
  })
  
  team1pitch <- reactive ({
    url <- paste("https://www.baseball-reference.com/teams/", input$team1, "/", input$team1year ,".shtml", sep='')
    
    data <- url %>%
      getURL() %>%
      readHTMLTable() %>%
      .[[2]] %>%
      as.data.frame() %>%
      filter(Rk != 'Rk') %>%
      mutate(Name = str_remove_all(Name, '\\#'), Name = str_remove_all(Name, '\\*'))
    
    for (i in 4:ncol(data)) {
      data[,i] <- as.numeric(as.character((data[,i])))
    }
    
    data1 <- data %>%
      filter(IP >= 30) %>%
      mutate(OBP = (H+HBP+BB)/(BF-IBB))
    
    return(data1)
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
    
    for (i in 4:ncol(data)) {
      data[,i] <- as.numeric(as.character((data[,i])))
    }
    
    data1 <- data %>%
      filter(IP >= 30) %>%
      mutate(OBP = (H+HBP+BB)/(BF-IBB))
    
    return(data1)
  })
  
  batoptions <- reactive({
    
    data <- team1bat()
    return(data[,3])
    
  })
  
  team2batoptions <- reactive({
    
    data <- team2bat()
    return(data[,3])
    
  })
  
  team1pitchoptions <- reactive({
    
    data <- team1pitch()
    return(data[,3])
    
  })
  
  team2pitchoptions <- reactive({
    
    data <- team2pitch()
    return(data[,3])
    
  })
  
  ### Team 1 Batting Lineup
  #
  #
  #
  
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
    selectInput('team1bat5', label = 'Batter 5', choices = batoptions())
  })
  
  
  output$teambat6 <- renderUI({
    selectInput('team1bat6', label = 'Batter 6', choices = batoptions())
  })
  
  
  output$teambat7 <- renderUI({
    selectInput('team1bat7', label = 'Batter 7', choices = batoptions())
  })
  
  
  output$teambat8 <- renderUI({
    selectInput('team1bat8', label = 'Batter 8', choices = batoptions())
  })
  
  
  output$teambat9 <- renderUI({
    selectInput('team1bat9', label = 'Batter 9', choices = batoptions())
  })
  
  ###Team 1 Pitcher
  
  output$team1pitch <- renderUI({
    selectInput('pitcher1', label = 'Pitcher', choices = team1pitchoptions())
  })

  
  ###Team 2 Batting lineup
  #
  #
  #
  #
  
  output$team2bat1 <- renderUI({
    selectInput('cteam2bat1', label = 'Batter 1', choices = team2batoptions())
  })  
  
  output$team2bat2 <- renderUI({
    selectInput('cteam2bat2', label = 'Batter 2', choices = team2batoptions())
  })
  
  
  output$team2bat3 <- renderUI({
    selectInput('cteam2bat3', label = 'Batter 3', choices = team2batoptions())
  })
  
  
  output$team2bat4 <- renderUI({
    selectInput('cteam2bat4', label = 'Batter 4', choices = team2batoptions())
  })
  
  
  output$team2bat5 <- renderUI({
    selectInput('cteam2bat5', label = 'Batter 5', choices = team2batoptions())
  })
  
  
  output$team2bat6 <- renderUI({
    selectInput('cteam2bat6', label = 'Batter 6', choices = team2batoptions())
  })
  
  
  output$team2bat7 <- renderUI({
    selectInput('cteam2bat7', label = 'Batter 7', choices = team2batoptions())
  })
  
  
  output$team2bat8 <- renderUI({
    selectInput('cteam2bat8', label = 'Batter 8', choices = team2batoptions())
  })
  
  
  output$team2bat9 <- renderUI({
    selectInput('cteam2bat9', label = 'Batter 9', choices = team2batoptions())
  })
  
  ###Team 1 Pitcher
  
  output$team2pitch <- renderUI({
    selectInput('pitcher2', label = 'Pitcher', choices = team2pitchoptions())
  })
  
  # createteam1 <- reactive({
  #   data <- team1bat() %>%
  #     mutate(Name = str_remove_all(Name, '\\#'), Name = str_remove_all(Name, '\\*'))
  #   team1 <- data %>%
  #     filter(Name == input$team1bat1)
  #   team1 <- rbind(team1, data %>%filter(Name == input$team1bat2))
  #   
  #   return(team1)
  # })
  
  createteam1 <- reactive({
    
    data1 <- team1bat() %>%
      mutate(PA2 = AB + BB + SF + HBP,
             homers = HR/PA2,
             triples = `3B`/PA2,
             doubles = `2B`/PA2,
             walks = (BB + HBP)/PA2,
             sing = H - HR - `2B` - `3B`,
             singles = sing/PA2,
             obp2 = singles + walks + doubles + triples + homers,
             singles = singles/obp2,
             doubles = doubles/obp2,
             triples = triples/obp2,
             walks = walks/obp2,
             homers = homers/obp) %>%
      select(Name, singles, doubles, triples, homers, walks, OBP, obp2)
      

    b1 <- data1 %>%
      filter(Name == input$team1bat1)
    b2 <- data1 %>%
      filter(Name == input$team1bat2)
    b3 <- data1 %>%
      filter(Name == input$team1bat3)
    b4 <- data1 %>%
      filter(Name == input$team1bat4)
    b5 <- data1 %>%
      filter(Name == input$team1bat5)
    b6 <- data1 %>%
      filter(Name == input$team1bat6)
    b7 <- data1 %>%
      filter(Name == input$team1bat7)
    b8 <- data1 %>%
      filter(Name == input$team1bat8)
    b9 <- data1 %>%
      filter(Name == input$team1bat9)
    
    data <- rbind(b1, b2, b3, b4, b5, b6, b7, b8, b9)
    
    # dataformatted <- data %>%
    #   mutate(PA2 = AB + BB + SF + HBP,
    #          homers = HR/PA2,
    #          triples = `3B`/PA2,
    #          doubles = `2B`/PA2,
    #          walks = (BB + HBP)/PA2,
    #          sing = H - HR - `2B` - `3B`,
    #          singles = sing/PA2,
    #          obp2 = singles + walks + doubles + triples + homers) %>%
    #   select(Name, singles, doubles, triples, homers, walks, OBP, obp2)
    
    b1 <- new('batter', name = b1$Name, obp = b1$obp2, single = b1$singles, double = b1$doubles, triple = b1$triples, hr = b1$homers, bb = b1$walks)
    b2 <- new('batter', name = b2$Name, obp = b2$obp2, single = b2$singles, double = b2$doubles, triple = b2$triples, hr = b2$homers, bb = b2$walks)
    b3 <- new('batter', name = b3$Name, obp = b3$obp2, single = b3$singles, double = b3$doubles, triple = b3$triples, hr = b3$homers, bb = b3$walks)
    b4 <- new('batter', name = b4$Name, obp = b4$obp2, single = b4$singles, double = b4$doubles, triple = b4$triples, hr = b4$homers, bb = b4$walks)
    b5 <- new('batter', name = b5$Name, obp = b5$obp2, single = b5$singles, double = b5$doubles, triple = b5$triples, hr = b5$homers, bb = b5$walks)
    b6 <- new('batter', name = b6$Name, obp = b6$obp2, single = b6$singles, double = b6$doubles, triple = b6$triples, hr = b6$homers, bb = b6$walks)
    b7 <- new('batter', name = b7$Name, obp = b7$obp2, single = b7$singles, double = b7$doubles, triple = b7$triples, hr = b7$homers, bb = b7$walks)
    b8 <- new('batter', name = b8$Name, obp = b8$obp2, single = b8$singles, double = b8$doubles, triple = b8$triples, hr = b8$homers, bb = b8$walks)
    b9 <- new('batter', name = b9$Name, obp = b9$obp2, single = b9$singles, double = b9$doubles, triple = b9$triples, hr = b9$homers, bb = b9$walks)
    
    lineup <- c(b1, b2, b3, b4, b5, b6, b7, b8, b9)

    return(lineup)
    
  })
  
  
  
  createpitcher1 <- reactive({
    
    data <- team1pitch() %>%
      filter(Name == input$pitcher1)
    
    p1 <- new('pitcher', name = data$Name, obpa = data$OBP)
    
    return(p1)
    
  })
  
  createteam2 <- reactive({

    data1 <- team2bat() %>%
      mutate(PA2 = AB + BB + SF + HBP,
             homers = HR/PA2,
             triples = `3B`/PA2,
             doubles = `2B`/PA2,
             walks = (BB + HBP)/PA2,
             sing = H - HR - `2B` - `3B`,
             singles = sing/PA2,
             obp2 = singles + walks + doubles + triples + homers,
             singles = singles/obp2,
             doubles = doubles/obp2,
             triples = triples/obp2,
             walks = walks/obp2,
             homers = homers/obp2) %>%
      select(Name, singles, doubles, triples, homers, walks, OBP, obp2)


    b1 <- data1 %>%
      filter(Name == input$cteam2bat1)
    b2 <- data1 %>%
      filter(Name == input$cteam2bat2)
    b3 <- data1 %>%
      filter(Name == input$cteam2bat3)
    b4 <- data1 %>%
      filter(Name == input$cteam2bat4)
    b5 <- data1 %>%
      filter(Name == input$cteam2bat5)
    b6 <- data1 %>%
      filter(Name == input$cteam2bat6)
    b7 <- data1 %>%
      filter(Name == input$cteam2bat7)
    b8 <- data1 %>%
      filter(Name == input$cteam2bat8)
    b9 <- data1 %>%
      filter(Name == input$cteam2bat9)

    data <- rbind(b1, b2, b3, b4, b5, b6, b7, b8, b9)

    # dataformatted <- data %>%
    #   mutate(PA2 = AB + BB + SF + HBP,
    #          homers = HR/PA2,
    #          triples = `3B`/PA2,
    #          doubles = `2B`/PA2,
    #          walks = (BB + HBP)/PA2,
    #          sing = H - HR - `2B` - `3B`,
    #          singles = sing/PA2,
    #          obp2 = singles + walks + doubles + triples + homers) %>%
    #   select(Name, singles, doubles, triples, homers, walks, OBP, obp2)

    b1 <- new('batter', name = b1$Name, obp = b1$obp2, single = b1$singles, double = b1$doubles, triple = b1$triples, hr = b1$homers, bb = b1$walks)
    b2 <- new('batter', name = b2$Name, obp = b2$obp2, single = b2$singles, double = b2$doubles, triple = b2$triples, hr = b2$homers, bb = b2$walks)
    b3 <- new('batter', name = b3$Name, obp = b3$obp2, single = b3$singles, double = b3$doubles, triple = b3$triples, hr = b3$homers, bb = b3$walks)
    b4 <- new('batter', name = b4$Name, obp = b4$obp2, single = b4$singles, double = b4$doubles, triple = b4$triples, hr = b4$homers, bb = b4$walks)
    b5 <- new('batter', name = b5$Name, obp = b5$obp2, single = b5$singles, double = b5$doubles, triple = b5$triples, hr = b5$homers, bb = b5$walks)
    b6 <- new('batter', name = b6$Name, obp = b6$obp2, single = b6$singles, double = b6$doubles, triple = b6$triples, hr = b6$homers, bb = b6$walks)
    b7 <- new('batter', name = b7$Name, obp = b7$obp2, single = b7$singles, double = b7$doubles, triple = b7$triples, hr = b7$homers, bb = b7$walks)
    b8 <- new('batter', name = b8$Name, obp = b8$obp2, single = b8$singles, double = b8$doubles, triple = b8$triples, hr = b8$homers, bb = b8$walks)
    b9 <- new('batter', name = b9$Name, obp = b9$obp2, single = b9$singles, double = b9$doubles, triple = b9$triples, hr = b9$homers, bb = b9$walks)

    lineup <- c(b1, b2, b3, b4, b5, b6, b7, b8, b9)

    return(lineup)

  })



  createpitcher2 <- reactive({

    data <- team2pitch() %>%
      filter(Name == input$pitcher2)

    p1 <- new('pitcher', name = data$Name, obpa = data$OBP)

    return(p1)

  })
  
  topinning <- reactive({
    
    lineup2 <- rep(createteam1(), 10)
    
    p1 <- createpitcher2()
    
    scores <- c()
    
    for (i in 1:1000) {
      runs <- 0
      inning <- 1
      lineup <- lineup2
      bat <- 0
      while (inning < 10) {
        totalbats <- 0
        outs <- 0
        onbase <- c()

        for (batter in lineup) {

          totalbats <- totalbats + 1

          num <- runif(1, min = 0, max = 1)#Create random number between 0 and 1
          slot(batter, 'obp') <- (slot(batter, 'obp') + slot(p1, 'obpa'))/2 #average pitcher's skill and batter's skill
          #if they get on base
          if (num < slot(batter, 'obp')) {
            num2 <- runif(1, min = 0, max = 1)
            
            if (num2 < slot(batter, 'single') + slot(batter, 'bb')) {#get a single or walk
              
              #move rest of batters on base
              onbase <- onbase + 1

              onbase <- c(onbase, 1)
            } else if (num2 < slot(batter, 'single') + slot(batter, 'bb') + slot(batter, 'double')) {#If batter doubles
              
              #move rest of batters on base
              
              onbase <- onbase + 2
              
              onbase <- c(onbase, 2)

            } else if (num2 < slot(batter, 'single') + slot(batter, 'bb') + slot(batter, 'double') + slot(batter, 'triple')) {#If batter triples
              
              #move rest of batters on base
              onbase <- onbase + 3

              onbase <- c(onbase, 3)

            } else if (num2 < slot(batter, 'single') + slot(batter, 'bb') + slot(batter, 'double')+ slot(batter, 'triple')+ slot(batter, 'hr')) {#If batter homeruns
              
              #move rest of batters on base
              onbase <- onbase + 4
              onbase <- c(onbase, 4)
              
            }} else { #batter is out
              outs <- outs + 1
            }
          
          if (outs == 3) {
            for (b in onbase) {
              if (b >= 4) {
                runs <- runs + 1
              }
            }
            for (p in 1:totalbats) {
              lineup[1] <- NULL
            }
            onbase <- c()
            break
          }}

        inning <- inning + 1
        
      }
      
      scores <- c(scores, runs)
    
    }
    
    return(scores)
    
  })
  
  bottominning <- reactive({
    
    lineup2 <- rep(createteam2(), 10)
    
    p1 <- createpitcher1()
    
    scores <- c()
    
    for (i in 1:1000) {
      runs <- 0
      inning <- 1
      lineup <- lineup2
      bat <- 0
      while (inning < 10) {
        totalbats <- 0
        outs <- 0
        onbase <- c()
        for (batter in lineup) {
          
          totalbats <- totalbats + 1
          
          num <- runif(1, min = 0, max = 1)#Create random number between 0 and 1
          slot(batter, 'obp') <- (slot(batter, 'obp') + slot(p1, 'obpa'))/2 #average pitcher's skill and batter's skill
          #if they get on base
          if (num < slot(batter, 'obp')) {
            num2 <- runif(1, min = 0, max = 1)
            
            if (num2 < slot(batter, 'single') + slot(batter, 'bb')) {#get a single or walk
              
              #move rest of batters on base
              onbase <- onbase + 1
              
              onbase <- c(onbase, 1)
              
            } else if (num2 < slot(batter, 'single') + slot(batter, 'bb') + slot(batter, 'double')) {#If batter doubles
              
              #move rest of batters on base
              
              onbase <- onbase + 2
              
              onbase <- c(onbase, 2)
              
            } else if (num2 < slot(batter, 'single') + slot(batter, 'bb') + slot(batter, 'double') + slot(batter, 'triple')) {#If batter triples
              
              #move rest of batters on base
              
              onbase <- onbase + 3
              
              onbase <- c(onbase, 3)
              
            } else if (num2 < slot(batter, 'single') + slot(batter, 'bb') + slot(batter, 'double')+ slot(batter, 'triple')+ slot(batter, 'hr')) {#If batter homeruns
              
              #move rest of batters on base
              onbase <- onbase + 4
              onbase <- c(onbase, 4)
              
            }} else { #batter is out
              outs <- outs + 1
            }
          
          if (outs == 3) {
            for (b in onbase) {
              if (b >= 4) {
                runs <- runs + 1
              }
            }
            for (p in 1:totalbats) {
              lineup[1] <- NULL
            }
            onbase <- c()
            break
          }}
        
        inning <- inning + 1
        
      }
      
      scores <- c(scores, runs)
      
    }
    
    return(scores)
    
  })
  
  fullgame <- reactive({
    
    team1s <- topinning()
    team2s <- bottominning()
    
    games <- as.data.frame(cbind(team1s, team2s)) %>%
      mutate(win = case_when(
        
        team1s > team2s ~ 1,
        team2s > team1s ~ 0,
        TRUE ~ -1
        
      )) %>%
      filter(win != -1)
    
    team1prob <- mean(games$win)
    
    return(team1prob)
    
  })
  
  output$plot1 <- renderPlot({
    
    hist(bottominning())
    
  })
  
  output$plot2 <- renderPlot({
    
    hist(topinning())
    
  })
  
  
  

   # output$table1 <- renderTable({
   #   createteam2()
   #   #team1bat()
   # })
   # 
   output$text1 <- renderText({
     # print(slot(createpitcher2(), 'name'))
     # 
     # for (i in createteam2()) {
     #   print(slot(i, 'name'))
     # }
     
     #print(fullgame())
     #print(mean(topinning()))
     
     #print(topinning())
     # for (i in topinning()) {
     #   
     #  print(i)
     #   
     # }
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

