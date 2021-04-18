#-------------------------------------------------------------------------------------------------#
#### UNO CARD GAME SIMULATION ####
#    ------------------------
# 
# By Jan-Karl Knigge
# January 2021
#
# Uno-Rules according to https://en.wikipedia.org/wiki/Uno_%28card_game%29
#-------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------#
#### Preparation ####
#-------------------------------------------------------------------------------------------------#

#------------------------#
#### Cards parameters ####
#------------------------#
#parameter of the cards in the total deck

colors <- c("red", "yellow", "green", "blue")
numbers <- c(0:9)
specials <- c("Skip", "Reverse", "+2")
wilds <- c("wild", "+4")

#-----------------------#
#### Game parameters ####
#-----------------------#
#parameters of the game

num_player <- 4         #Num of players
init_draw <- 7          #Num of cards that are drawn initially

iterations <- 10000               #number of iterations (games simulated) with each setting
#allowMultPlus <- c(F,T)       #disable and enable accumulation of plus cards
allowMultPlus <- c(F, T)
showText <- 2                 #debugging: print text to console during simulations, 0: disable print, 1: print to console, 2: print to file
logFileDir <<- "output.txt"   #file for text output
displayIteration <- TRUE      #display current iteration?


set.seed(1988)          #set seed for random generated numbers

#--------------#
#### Inputs ####
#--------------#
#load external files & librarys
library(ggplot2)
library(mosaic)
source("Functions.R", encoding="UTF-8" )  #load functions

#-------------------------------------------------------------------------------------------------#
#### Game ####
#-------------------------------------------------------------------------------------------------#
maxCards <<- 0
textToPrint <<- NULL

gamesData <- NULL
for(a  in allowMultPlus) {
  for(i in 1:iterations) {
    if(displayIteration) {
      cat(paste("Iteration", i,"/", iterations,"(allowMultPlus=",a,")\n"))
    }
    gamesData <- playGame(gamesData, allowMultPlus = a, printText = showText)
  }
}

#print text to file
if(showText==2) {
  #cat(textToPrint)
  logFile <- file(logFileDir, open="w")
  writeLines(textToPrint, logFile)
  close(logFile)
}

#-------------------------------------------------------------------------------------------------#
#### Plots ####
#-------------------------------------------------------------------------------------------------#
gg_maxCardsHand <- plotHistogram(data=gamesData, variable = "maxCardsInHand", xlab = "Max. number of cards in hand", binSize = 1)
gg_maxCardsDrawn <- plotHistogram(data=gamesData, variable = "maxCardsDrawn", xlab = "Max. number of cards drawn by one player in a single turn", binSize=2)
gg_numRounds <- plotHistogram(data=gamesData, variable = "rounds", xlab = "Number of rounds per game", binSize = 10)
gg_maxCardsDrawnSingle <- plotHistogram(data=dplyr::filter(gamesData,typ!="Standard rules"), variable = "maxCardsDrawn", xlab = "Max. number of cards drawn by one player in a single turn", binSize=2, legendPos="None")
