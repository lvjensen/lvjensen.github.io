setClass('batter', slots = list(name = 'character', average = 'numeric', obp = 'numeric', bb = 'numeric', single = 'numeric', double = 'numeric', triple = 'numeric', hr = 'numeric', base = 'numeric'))
setClass('pitcher', slots = list(name = 'character', obpa = 'numeric'))
b1 <- new('batter', name = 'Juan', average = .247, obp = .332, single = .432, double = .135, triple = .063, hr = .072, bb = .297, base = 0)
b2 <- new('batter', name = 'Juan2', average = .247, obp = .332, single = .432, double = .135, triple = .063, hr = .072, bb = .297, base = 0)
b3 <- new('batter', name = 'Juan3', average = .247, obp = .332, single = .432, double = .135, triple = .063, hr = .072, bb = .297, base = 0)
b4 <- new('batter', name = 'Juan4', average = .247, obp = .332, single = .432, double = .135, triple = .063, hr = .072, bb = .297, base = 0)
b5 <- new('batter', name = 'Juan5', average = .247, obp = .332, single = .432, double = .135, triple = .063, hr = .072, bb = .297, base = 0)
b6 <- new('batter', name = 'Juan6', average = .247, obp = .332, single = .432, double = .135, triple = .063, hr = .072, bb = .297, base = 0)
b7 <- new('batter', name = 'Juan7', average = .247, obp = .332, single = .432, double = .135, triple = .063, hr = .072, bb = .297, base = 0)
b8 <- new('batter', name = 'Juan8', average = .247, obp = .332, single = .432, double = .135, triple = .063, hr = .072, bb = .297, base = 0)
b9 <- new('batter', name = 'Juan9', average = .247, obp = .332, single = .432, double = .135, triple = .063, hr = .072, bb = .297, base = 0)
p1 <- new('pitcher', name = 'John', obpa =.31)

lineup3 <- c(b1, b2, b3, b4, b5, b6, b7, b8, b9)

lineup2 <- rep(lineup3, 10)

#lineup[[1]] <- NULL

totalbats <- 0

length(lineup)
#finding out if for loop works
#Testing batter
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
  # for (n in lineup){
  #   print(slot(n, 'name'))
  # }
  #while (outs < 3) {
    for (batter in lineup) {
      #print(paste("totalbatters", totalbats))
      print(slot(batter, 'name'))
      totalbats <- totalbats + 1
      #bat <- bat + 1
      num <- runif(1, min = 0, max = 1)#Create random number between 0 and 1
      slot(batter, 'obp') <- (slot(batter, 'obp') + slot(p1, 'obpa'))/2 #average pitcher's skill and batter's skill
      #if they get on base
      if (num < slot(batter, 'obp')) {
        num2 <- runif(1, min = 0, max = 1)
        
        if (num2 < slot(batter, 'single') + slot(batter, 'bb')) {#get a single or walk
          
          #move rest of batters on base
          onbase <- onbase + 1
          # for (b in onbase) {
          #   if (b >= 4) {
          #     runs <- runs + 1
          #     b <- NULL
          #   }}
          onbase <- c(onbase, 1)
         } else if (num2 < slot(batter, 'single') + slot(batter, 'bb') + slot(batter, 'double')) {#If batter doubles

           #move rest of batters on base

           onbase <- onbase + 2
           # for (b in onbase) {
           #   if (b >= 4) {
           #     runs <- runs + 1
           #     b <- NULL
           #   }}
             
           onbase <- c(onbase, 2)
            #print(onbase)
         } else if (num2 < slot(batter, 'single') + slot(batter, 'bb') + slot(batter, 'double') + slot(batter, 'triple')) {#If batter triples

         #move rest of batters on base
           onbase <- onbase + 3
           # for (b in onbase) {
           #   if (b >= 4) {
           #     runs <- runs + 1
           #     b <- NULL
           #   }}
           onbase <- c(onbase, 3)
           #print(onbase)
         } else if (num2 < slot(batter, 'single') + slot(batter, 'bb') + slot(batter, 'double')+ slot(batter, 'triple')+ slot(batter, 'hr')) {#If batter homeruns
             print("Home Run!")
           #move rest of batters on base
             onbase <- onbase + 4
             onbase <- c(onbase, 4)
             # for (b in onbase) {
             #     runs <- runs + 1
             #     b <- NULL
             #   }

             #print(onbase)
          
        }} else { #batter is out
          outs <- outs + 1
        }
      # print(slot(batter, 'name'))
      # print(bat)
      # lineup[bat] <- NULL
      # print(slot(batter, 'name'))
      #print(onbase)
        
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
  #print(paste("Runs:", runs))
  print(totalbats)
  inning <- inning + 1
  print(runs)
  #print(paste("Inning: ",inning))
  #print(paste("runs: ", runs))
}
scores <- c(scores, runs)
}

games <- as.data.frame(cbind(scores, scores1)) %>%
  mutate(win = case_when(
    scores >= scores1 ~ 1,
    TRUE ~ 0
  ))

mean(games$win)

hist(scores)
sum(scores)/1000
table(scores)

game
x <- c(.45, .55)
y <- c('team1', 'team2')
probs <- as.data.frame(cbind('game probability', x, y))

probs

probs <- probs %>%
  mutate(x = as.numeric(as.character(x)))

probs %>%
  ggplot() +
  geom_bar(aes(y = x, x = V1, fill = y), stat = 'identity', width = .25) +
  #position_dodge(width = .5)
  coord_flip()

# x <- 0
# while (x<1) {
#   for (batter in lineup) {
#     print(slot(batter, 'obp'))
#     x <- x + 1
#   }
# }


# num <- runif(1, min = 0, max = 1)
# print(num)
# if (num <= slot(p1, 'average')) {
#   print(slot(p1, 'name'))
# }

# month1 <- month(Sys.Date())
# day1 <- day(Sys.Date())
# name <- paste('C:/data-', month1, '/', day1, '.csv', sep = '')
# write.csv(dataset, file = name) 

