setClass('batter', slots = list(name = 'character', average = 'numeric', obp = 'numeric', bb = 'numeric', single = 'numeric', double = 'numeric', triple = 'numeric', hr = 'numeric', base = 'numeric'))
setClass('pitcher', slots = list(name = 'character', obpa = 'numeric'))
b1 <- new('batter', name = 'Juan', average = .260, obp = .332, single = .432, double = .135, triple = .063, hr = .072, bb = .297, base = 0)
b2 <- new('batter', name = 'Juan2', average = .260, obp = .332, single = .432, double = .135, triple = .063, hr = .072, bb = .297, base = 0)
b3 <- new('batter', name = 'Juan3', average = .260, obp = .332, single = .432, double = .135, triple = .063, hr = .072, bb = .297, base = 0)
b4 <- new('batter', name = 'Juan4', average = .260, obp = .332, single = .432, double = .135, triple = .063, hr = .072, bb = .297, base = 0)
b5 <- new('batter', name = 'Juan5', average = .260, obp = .332, single = .432, double = .135, triple = .063, hr = .072, bb = .297, base = 0)
b6 <- new('batter', name = 'Juan6', average = .260, obp = .332, single = .432, double = .135, triple = .063, hr = .072, bb = .297, base = 0)
b7 <- new('batter', name = 'Juan7', average = .260, obp = .332, single = .432, double = .135, triple = .063, hr = .072, bb = .297, base = 0)
b8 <- new('batter', name = 'Juan8', average = .260, obp = .332, single = .432, double = .135, triple = .063, hr = .072, bb = .297, base = 0)
b9 <- new('batter', name = 'Juan9', average = .260, obp = .332, single = .432, double = .135, triple = .063, hr = .072, bb = .297, base = 0)
p1 <- new('pitcher', name = 'John', obpa =.360)

lineup <- c(b1, b2, b3, b4, b5, b6, b7, b8, b9)
N <- 100
inning <- 1
score <- rep(NA, N)


#finding out if for loop works
for (i in 1:N) {

  runs <- 0
  
  for (batter in lineup){
    
    slot(batter, 'obp') <- (slot(batter, 'obp') + slot(p1, 'obpa'))/2
    
    num <- runif(1, min = 0, max = 1)
    
    if (num <= slot(batter, 'obp')) {
      
      runs <- runs + 1
    
      }
  }
  
  score[i] <- runs
}

hist(score)

#Testing batter
scores <- c()
for (i in 1:1000) {
runs <- 0
inning <- 1
while (inning < 10) {
  outs <- 0
  onbase <- c()
  #while (outs < 3) {
    for (batter in lineup) {
      num <- runif(1, min = 0, max = 1)#Create random number between 0 and 1
      slot(batter, 'obp') <- (slot(batter, 'obp') + slot(p1, 'obpa'))/2 #average pitcher's skill and batter's skill
      #if they get on base
      if (num < slot(batter, 'obp')) {
        num2 <- runif(1, min = 0, max = 1)
        
        if (num2 < slot(batter, 'single') + slot(batter, 'bb')) {#get a single or walk
          
          #move rest of batters on base
          for (b in onbase) {
            b <- b + 1
            if (b >= 4) {
              runs <- runs + 1
              b <- NULL
            }
          }
          onbase <- c(onbase, 1)
         } else if (num2 < slot(batter, 'single') + slot(batter, 'bb') + slot(batter, 'double')) {#If batter doubles
           slot(batter, 'base') <- 2

           #move rest of batters on base
           for (b in onbase) {
             b <- b + 2
             if (b >= 4) {
               runs <- runs + 1
               b <- NULL
             }
           }
           onbase <- c(onbase, 2)
            #print(onbase)
         } else if (num2 < slot(batter, 'single') + slot(batter, 'bb') + slot(batter, 'double') + slot(batter, 'triple')) {#If batter triples

           #move rest of batters on base
           for (b in onbase) {
             b <- b + 3
             if (b >= 4) {
               runs <- runs + 1
               b <- NULL
             }
           }
           onbase <- c(onbase, 3)
           #print(onbase)
         } else if (num2 < slot(batter, 'single') + slot(batter, 'bb') + slot(batter, 'double')+ slot(batter, 'triple')+ slot(batter, 'hr')) {#If batter homeruns
             runs <- runs + 1
           #move rest of batters on base
             for (b in onbase) {
               runs = runs + 1
             }

             #print(onbase)
          
        }} else { #batter is out
          outs <- outs + 1
        }
        
      if (outs == 3) {
        # for (b in onbase) {
        #   if (b >= 4) {
        #     runs <- runs + 4
        #   }
        # }
        onbase <- c()
        break
      }}
  inning <- inning + 1
  #print(paste("Inning: ",inning))
  #print(paste("runs: ", runs))
}
scores <- c(scores, runs)
}

hist(scores)
sum(scores)/1000
table(scores)

for (b in onbase) {
  b <- b + 1
  print(b)
}

if (num < slot(batter, 'obp')) {
  onbase <- c(onbase, batter)
  num2 <- runif(1, min = 0, max = 1)
  
  if (num2 < slot(batter, 'single')) {#get a single
    onbase <- c(onbase, batter)
    #move rest of batters on base
    for (b in onbase) {
      print(slot(b, 'base'))
      print(slot(b, 'name'))
      if (slot(b, 'base') >= 4) {
        runs <- runs + 1
        slot(b, 'base') <- -1000
      }
    }}} 






onbase <- c(onbase, batter)
onbase
for (b in onbase) {
  slot(b, 'base') <- slot(b, 'base') + 1
  print(slot(b, 'base'))
}
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

