
library(tidyverse)
library(magrittr)

##question 1

  
n <- 1000

##selects a door and the door with the prize behind it
lab3_fun1 <- function(choice){
  doors <- as.character(1:3)
  picks <- sample(doors, size = 2, replace = TRUE)
  cont_pick <- unlist(picks)[1]
  print(paste0("Contestent picks door ", cont_pick))
  prize <- unlist(picks)[2]
  print(paste0("Prize door is door ", prize))
  ##correct needs work!
  #correct <- doors[!doors%in%c(prize)]
  correct <- sample(doors[!doors%in%c(cont_pick)], size = 1)
  incorrect <- doors[!doors%in%c(cont_pick, prize)]
  #incorrect <- sample(doors[!doors%in%c(prize)], size = 1)
  offer <- ifelse(cont_pick == prize, 
                  ##this is for 1.b.1
                  correct,
                  ##this is for 1.b.2
                  incorrect)
  print(paste0("Next door opened is ", offer))
  if (choice == 'switch') {
    cont_pick <- doors[!doors%in%c(cont_pick, offer)]
  }
  print(cont_pick)
  ifelse(cont_pick == prize, TRUE, FALSE)
}

simulation <- 

##gives 0
##switch = TRUE
switch <- replicate(n, lab3_fun1('switch'))
mean(switch)

##gives correct .32
##switch = FALSE
stick <- replicate(n, lab3_fun1('stick'))
mean(stick)



##messing with changing cont_pick if 'switch'
doors <- as.character(1:3)
picks <- sample(doors, size = 2, replace = TRUE)
cont_pick <- unlist(picks)[1]
print(paste0("Contestent picks door ", cont_pick))
prize <- unlist(picks)[2]
print(paste0("Prize door is door ", prize))
switch <- replace(cont_pick, 1, offer)
stick <- cont_pick



##outside the function practice to run line by line
doors <- as.character(1:3)
picks <- sample(doors, size = 2, replace = TRUE)
print(picks)
cont_pick <- unlist(picks)[1]
print(cont_pick)
prize <- unlist(picks)[2]
print(prize)
##correct needs work!
correct <- doors[!doors%in%c(prize)]
incorrect <- doors[!doors%in%c(cont_pick, prize)]
offer <- ifelse(cont_pick == prize, 
                ##this is for 1.b.1
                correct,
                ##this is for 1.b.2
                incorrect)
switch <- offer
switch <- rename(offer = cont_pick)
stick <- cont_pick
ifelse(choice == 'switch', switch, stick)
ifelse(cont_pick == prize, print(TRUE), print(FALSE))

#switch <- replace(cont_pick, 1, offer)
#stick <- cont_pick
## I think I have a problem here
#type <- ifelse(choice == 'switch', replace(cont_pick, 1, offer), stick)
print(cont_pick)
## need to edit based on if the offer is taken
ifelse(cont_pick == prize, yes, no)


##gives 0
##switch = TRUE
#switch <- replicate(n, lab3_fun1(switch = TRUE))
#mean(switch)
##gives correct .32
##switch = FALSE
#stick <- replicate(n, lab3_fun1(switch = FALSE))
#mean(stick)


##wrong approach
door <- sample(1:3, size = 2, replace = TRUE)
x = unlist(door)[1]
y = unlist(door)[2]

fun1_b <- function(x, y) {
  if_else ()
}

if_else (identical(1, 2), 
         "", 
         "Switch?")

