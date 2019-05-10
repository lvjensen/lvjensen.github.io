setClass('batter', slots = list(name = 'character', average = 'numeric', obp = 'numeric', bb = 'numeric', single = 'numeric', double = 'numeric', triple = 'numeric', hr = 'numeric', base = 'numeric'))
setClass('pitcher', slots = list(name = 'character', obpa = 'numeric'))
b1 <- new('batter', name = 'Juan', average = .250, obp = .325, single = .45, double = .25, triple = .05, hr = .1, bb = .15, base = 0)
b2 <- new('batter', name = 'Juan', average = .203, obp = .325, single = .45, double = .25, triple = .05, hr = .1, bb = .15, base = 0)
b3 <- new('batter', name = 'Juan', average = .287, obp = .325, single = .45, double = .25, triple = .05, hr = .1, bb = .15, base = 0)
b4 <- new('batter', name = 'Juan', average = .303, obp = .325, single = .45, double = .25, triple = .05, hr = .1, bb = .15, base = 0)
b5 <- new('batter', name = 'Juan', average = .254, obp = .325, single = .45, double = .25, triple = .05, hr = .1, bb = .15, base = 0)
b6 <- new('batter', name = 'Juan', average = .233, obp = .325, single = .45, double = .25, triple = .05, hr = .1, bb = .15, base = 0)
b7 <- new('batter', name = 'Juan', average = .244, obp = .325, single = .45, double = .25, triple = .05, hr = .1, bb = .15, base = 0)
b8 <- new('batter', name = 'Juan', average = .332, obp = .325, single = .45, double = .25, triple = .05, hr = .1, bb = .15, base = 0)
b9 <- new('batter', name = 'Juan', average = .167, obp = .325, single = .45, double = .25, triple = .05, hr = .1, bb = .15, base = 0)
p1 <- new('pitcher', name = 'John', obpa =.260)

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
runs <- 0
while (inning < 10) {
  outs <- 0
  onbase <- c()
  while (outs < 3) {
    for (batter in lineup) {
      num <- runif(1, min = 0, max = 1)
      slot(batter, 'obp') <- (slot(batter, 'obp') + slot(p1, 'obpa'))/2
      
      #if they get on base
      if (num < slot(batter, 'obp')) {
        onbase <- c(onbase, batter)
        num2 <- runif(1, min = 0, max = 1)
        
        if (num2 < slot(batter, 'single')) {#get a single
          slot(batter, 'base') <- 1
          #move rest of batters on base
          for (batter in onbase) {
            slot(batter, 'base') <- slot(batter, 'base') + 1
            if (slot(batter, 'base') >= 4) {
              runs <- runs + 1
              slot(batter, 'base') <- -1000
            }
          }
        } else if (nums < slot(batter, 'single') + slot(batter, 'double')) {#If batter doubles
          slot(batter, 'base') <- 2
          #move rest of batters on base
          for (batter in onbase) {
            slot(batter, 'base') <- slot(batter, 'base') + 1
            if (slot(batter, 'base') >= 4) {
              runs <- runs + 1
              slot(batter, 'base') <- -1000
            }
          }
        } else if (nums < slot(batter, 'single') + slot(batter, 'double')) {#If batter doubles
          slot(batter, 'base') <- 2
          #move rest of batters on base
          for (batter in onbase) {
            slot(batter, 'base') <- slot(batter, 'base') + 1
            if (slot(batter, 'base') >= 4) {
              runs <- runs + 1
              slot(batter, 'base') <- -1000
            }
          }
        }
        
      } else { #batter is out
        outs <- outs + 1
      }
      if (outs = 3) {
        break
      }
    }
  inning <- inning + 1
  }
}


print(runs)


onbase <- c(b1, b2, b3)
for (batter in onbase) {
  
}

onbase

onbase[[-1,]]

find(b1, onbase)








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

