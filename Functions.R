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
#### Functions ####
#-------------------------------------------------------------------------------------------------#

#---------------------#
#### plotHistogram ####
#---------------------#
#plots a histogram from the results
plotHistogram <- function(data, variable, xlab="", title="Uno Analysis", binSize=10, legendPos="bottom"){
  grp_mean <- mean(get(variable) ~  typ, data=data)
  
  gg <- ggplot(data=data, aes(x=get(variable), fill=typ)) + 
    geom_histogram(binwidth = binSize, position="identity", color="black", alpha=0.5) + 
    theme_minimal()+labs(x=xlab, title=title, y="Number of games") + 
    scale_fill_manual(values=c("#E9503E", "#5D85C3")) +  
    theme(legend.position=legendPos, legend.title = element_blank()) + 
    geom_vline(xintercept = grp_mean[2], color="#5D85C3", linetype="dashed", size=1.5) +
    geom_vline(xintercept = grp_mean[1], color="#E9503E", linetype="dashed", size=1.5)
  
  return(gg)
}

#----------------------#
#### playGame ####
#----------------------#
#plays one round of the game until a player wins or until the deck of card is empty

playGame <- function(gamesData, allowMultPlus=T, printText=0) {
  cards <<- createCardDeck(colors, numbers, specials, wilds) #create deck of cards
  cards <<- sample(cards)     #shuffle cards
  bin <<- NULL
  hands <- drawInitCards(init_draw, num_player)      #draw initial set of cards
  topCard <- drawCards(1)[[1]]                               #set the top card on the discard pile
  
  player <- 1
  cardsToDraw <- 0
  chosenColor <- colorRanking(hands[[player]])[1]
  
  numRounds <- 0                 #number of rounds
  maxCardsInHand <- 0         #max number of cards in hands of players
  maxCardsDrawn <- 0          #max number of cards that has been drawn in one round
  gameStoppedEarly <<- F          #has the game been stopped early due to missing cards
  gameEnded <<- F
  
  positiveDirection <- T      #increase or decrease player number each turn
  
  txt <- NULL                 #new Text to print to file or console
  
  txt <- textEdit(printHands(hands), modus = printText, addText = txt)
  
  while(!gameEnded) {
    numRounds <- numRounds +1
    maxCardsInHand <- countMaxNumberOfCardsForAll(maxCardsInHand, hands)
    
    txt <- textEdit(paste("Player:", player, "Top card:", cardToString(topCard), sep=" "), modus=printText, addText = txt)
    if(topCard$color=="black") {
      txt <- textEdit(paste("chosenColor: ", chosenColor, sep=" "), modus=printText, addText=txt)
    }
    
    hand <- hands[[player]]
    
    #choose next card or draw cards:
    pC <- possibleCards(topCard=topCard, playerCards = hand, chosenColor = chosenColor)
    txt <- textEdit(printHands(hands, player=player), modus=printText, addText = txt)
    txt <- textEdit(paste("Possible cards: ", pC, sep=""), modus=printText, addText=txt)
   
    if(isPlusCard(topCard)) {
      cardsToDraw <- cardsToDraw+addCardsToDraw(topCard)
      maxCardsDrawn <- checkMaxValue(cardsToDraw, maxCardsDrawn)
      if(allowMultPlus) {
        colRank <- colorRanking(hand)
        nextCard <- selectCardToPlay(potCards=pC, hand=hands[[player]], ranking=colRank, allowMultPlus = allowMultPlus)
        if(!is.null(nextCard) && !isPlusCard(hands[[player]][[nextCard]])) {
          hands[[player]] <- append(hands[[player]], drawCards(cardsToDraw))

          txt <- textEdit(paste("Player", player, "draws", cardsToDraw, "cards", sep=" "), modus=printText, addText = txt)
          txt <- textEdit(paste("Number of cards in deck: ", length(cards), sep=""), modus=printText, addText = txt)
          
          colRank <- colorRanking(hand)
          nextCard <- selectCardToPlay(potCards=pC, hand=hands[[player]], ranking=colRank, allowMultPlus = allowMultPlus)
          cardsToDraw <- 0
        }
      } else {
        hands[[player]] <- append(hands[[player]], drawCards(cardsToDraw))

        txt <- textEdit(paste("Player", player, "draws", cardsToDraw, "card", sep=" "), modus=printText, addText = txt)
        txt <- textEdit(paste("Number of cards in deck: ", length(cards), sep=""), modus=printText, addText = txt)
        
        colRank <- colorRanking(hand)
        nextCard <- selectCardToPlay(potCards=pC, hand=hands[[player]], ranking=colRank, allowMultPlus = allowMultPlus)
        cardsToDraw <- 0
      }
    } else {
      colRank <- colorRanking(hand)
      nextCard <- selectCardToPlay(potCards=pC, hand=hands[[player]], ranking=colRank, allowMultPlus = allowMultPlus)
    }
    
    #play card or draw card:
    if(!gameStoppedEarly) { 
      if(is.null(nextCard) || is.null(hands[[player]][[nextCard]])) {
        if(cardsToDraw<1) {
          cardsToDraw <- 1
        }
   
        draw <- drawCards(cardsToDraw)
        
        maxCardsDrawn <- checkMaxValue(1, maxCardsDrawn)
        hands[[player]] <- append(hands[[player]], draw)

        txt <- textEdit(paste("Player", player, "draws", cardsToDraw, "card(s):", cardToString(draw[[1]]), sep=" "), modus=printText, addText = txt)
        txt <- textEdit(paste("Number of cards in deck: ", length(cards), sep=""), modus=printText, addText = txt)
        
        cardsToDraw <- 0
        
      } else {
        bin <<-append(bin, list(topCard))
        topCard <- hands[[player]][[nextCard]]    #play card
        hands[[player]][[nextCard]] <- NULL       #remove card from hand
  
        txt <- textEdit(paste("Player", player,"plays", cardToString(topCard), sep=" "), modus=printText, addText = txt)
        txt <- textEdit(paste("Number of cards in hands:"), modus=printText, addText = txt)
        
        totalNumCards <- length(cards) +length(bin)
        if(printText!=0) {
          for(i in 1:num_player) {
            txt <- textEdit(paste("- Player ", i, ": ", length(hands[[i]]), sep=""), modus=printText, addText = txt)
            totalNumCards <- totalNumCards+length(hands[[i]])
          }

          txt <- textEdit(paste("--> Total number of cards in the game: ", totalNumCards, sep=""), modus=printText, addText = txt)
          
        }
        
        if(topCard$color=="black") {       #select color
          chosenColor <- colRank[1]
          if(chosenColor=="black") {
            chosenColor <- colRank[2]
          }
        }
        if(length(hands[[player]])==0) {    #check end of game
          gameEnded <<- TRUE

          txt <- textEdit(paste("Game ends! -  Player ", player, " wins!", sep=""), modus=printText, addText = txt)
          
        }
        if(topCard$typ=="Skip") {
          player <- nextPlayer(player=player, increase = positiveDirection, num_player = num_player)
        }
        if(topCard$typ=="Reverse") {
          positiveDirection <- !positiveDirection
        }
        player <- nextPlayer(player=player, increase=positiveDirection, num_player=num_player)
      }
    }
    
  }
  #cat(txt)
  if(allowMultPlus) {
    typ <- "Allow accumulation of plus cards"
  } else {
    typ <- "Standard rules"
  }
  if(gameStoppedEarly) {
    gamesData <- rbind(gamesData, data.frame(rounds=numRounds, maxCardsInHand=maxCardsInHand, maxCardsDrawn=maxCardsDrawn, endedEarly=T, typ=typ))
    
  } else {
    gamesData <- rbind(gamesData, data.frame(rounds=numRounds, maxCardsInHand=maxCardsInHand, maxCardsDrawn=maxCardsDrawn, endedEarly=F, typ=typ))
  }
  if(maxCardsInHand > maxCards) {
    maxCards <<- maxCardsInHand
    #cat(txt)
    textToPrint <<- txt
  }
  return(gamesData)
}

#----------------#
#### textEdit ####
#----------------#
#prints texts to console if modus==1 or saves text to a file if modus ==2
textEdit <- function(text, modus, addText="") {
  if(modus==1) {
    print(text)
    return(NULL)
  } 
  if(modus==2) {
    #writeLines(text, con=logFile)
    output <- paste(addText,text,"\n", sep="")
    #print(output)
    return(output)
  } else {
    return(NULL)
  }
}

#----------------------#
#### checkMaxNumber ####
#----------------------#
#returns new value if it is larger than the old value

checkMaxValue <- function(newValue, oldValue) {
  if(newValue > oldValue) {
    return(newValue)
  } else {
    return(oldValue)
  }
}

#-----------------------------#
#### countMaxNumberOfCards ####
#-----------------------------#
#checks if the max number of cards has increased
countMaxNumberOfCards <- function(maxNumber, hand) {
  if(length(hand)>maxNumber) {
    return(length(hand))
  } else {
    return(maxNumber)
  }
}

countMaxNumberOfCardsForAll <- function(maxNumber, hands) {
  for(h in hands) {
    maxNumber <- countMaxNumberOfCards(maxNumber, h)
  }
  return(maxNumber)
}

#-----------------#
#### drawCards ####
#-----------------#
#draws cards from deck and returns drawn cards
drawCards <- function(num=1) {
  output <- NULL
  for(i in 1:num) {
    output <- append(output, list(cards[[1]]))
    #bin <<- append(bin, list(cards[[1]]))
    cards[[1]] <<-NULL
    if(length(cards)==0) {          #check if deck is empty
      if(is.null(bin) || length(bin)==0) {
        gameStoppedEarly <<- TRUE
        gameEnded <<- TRUE
        return(NULL)
      } else {
        cards <<- sample(bin)
        bin <<-NULL
      }
    }
  }
  return(output)
}

#------------------#
#### nextPlayer ####
#------------------#
#increases (or decreases) the player number
nextPlayer <- function(player, increase=T, num_player) {
  if(increase) {
    player <- player+1
  } else {
    player <- player-1
  }
  if(player>num_player) {
    player <- 1
  }
  if(player<1) {
    player <- num_player
  }
  return(player)
} 

#--------------------#
#### cardToString ####
#--------------------#
#returns card details as string

cardToString <- function(card) {
  return(paste("(color:", card$color, "Typ:", card$typ,")", sep=" "))
}

#------------------#
#### isPlusCard ####
#------------------#
#return true if a given card is a +2 or +4 card
isPlusCard <- function(card) {
  if(is.null(card)) {
    return(FALSE)
  }
  if(card$typ=="+2") {
    return(TRUE)
  } else if(card$typ=="+4") {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#--------------------#
#### containsPlus ####
#--------------------#
#returns true if a stack of cards contains a +2 or +4 card
containsPlus <- function(cards) {
  for(c in cards) {
    if(c$typ=="+2") { 
      return(TRUE)
    } else if(c$typ=="+4") {
      return(TRUE)
    }
  }
  return(FALSE)
}

#----------------------#
#### addCardsToDraw ####
#----------------------#
#adds number of cards to draw
addCardsToDraw <- function(card) {
  if(card$typ=="+2") {
    return(2)
  } 
  if(card$typ=="+4") {
    return(4)
  }
  return(0)
}

#------------------------#
#### selectCardToPlay ####
#------------------------#
#selects the next card to play from the list of potential cards
selectCardToPlay <- function(potCards, hand, ranking, allowMultPlus=F) {
  if(is.null(potCards)) {
    return(NULL)
  }
  
  if(allowMultPlus) {               #prefer plus cards if multiple plus cards are allowed
    cardsToChoose <- selectOnlyPlus(potCards, hand)
    if(is.null(cardsToChoose)) {
      cardsToChoose <- potCards
    }
  } else {
    cardsToChoose <- potCards
  }
  
  if(length(cardsToChoose)>1) {
    chosenCards <- NULL
    rankNum <- 1
    while(is.null(chosenCards)) {
      chosenCards <- filterCardsByColor(cardsToChoose, ranking[[rankNum]], hand)
      rankNum <- rankNum+1
    }
    return(sample(chosenCards, 1)[[1]])
  } else {
    return(cardsToChoose[[1]])
  }
}

#--------------------------#
#### filterCardsByColor ####
#--------------------------#
#filter cards by their color
filterCardsByColor <- function(playerCards, color, hand) {
  output <- NULL
  for(i in playerCards) {
    c <- hand[[i]]
    if(c$color==color) {
      output <- append(output, list(i))
    }
  }
  return(output)
}

#----------------------#
#### selectOnlyPlus ####
#----------------------#
# only keeps +4 or +2 cards in the set of cards
selectOnlyPlus <- function(playerCardsNum, hand) {
  shouldTypes <- c("+4", "+2")
  output <- NULL
  for(i in playerCardsNum) {
    c <- hand[[i]]
    if(c$typ %in% shouldTypes) {
      output <- append(output, list(i))
    }
  }
  return(output)
}

#--------------------#
#### colorRanking ####
#--------------------#
# gives the order of colors in a player's hand, based on the number of cards with each color, i.e. the most frequent color is placed first etc.
colorRanking <- function(playerCards) {
  colors <- c("red", "yellow", "green", "blue", "black")
  numCol <- c(0,0,0,0,0)
  for(c in playerCards) {
    if(c$color==colors[1]) {
      numCol[1] <- numCol[1]+1
    } else if(c$color==colors[2]) {
      numCol[2] <- numCol[2]+1
    } else if(c$color==colors[3]) {
      numCol[3] <- numCol[3]+1
    } else if(c$color==colors[4]) {
      numCol[4] <- numCol[4]+1
    } else if(c$color==colors[5]) {
      numCol[5] <- numCol[5]+1
    }
  }
  ranking <- colors
  notCheck <- NULL
  for(i in 1:length(numCol)) {
    maxNum <- 0
    mostFreqCol <- 1
    for(j in 1:length(numCol)) {
      if(!(j %in% notCheck)) {
        if(numCol[j]>maxNum) {
          maxNum <- numCol[j]
          mostFreqCol <- j
        } else if(numCol[j]==maxNum) {      #Bei Gleichstand: Auswahl per Zufall
          if(sample(c(T,F), 1)) { 
            axNum <- numCol[j]
            mostFreqCol <- j
          }
        }
      }
    } 
    ranking[i] <- colors[mostFreqCol]
    notCheck <- append(notCheck, list(mostFreqCol))
  }
  return(ranking)
}

#---------------------#
#### possibleCards ####
#---------------------#
#returns a list of cards of a player's deck, that can be played on the current top cards
possibleCards <- function(topCard, playerCards, chosenColor=NULL, allowBlackOnBlack=T) {
  output <- NULL
  typ <- topCard$typ
  if(topCard$color=="black") {
    if(is.null(chosenColor)) {
      stop("Abort due to missing color preference after wild card.")
    }
    nextColor <- chosenColor
  } else {
    nextColor <- topCard$color
  }
  
  for(i in 1:length(playerCards)) {
    c <- playerCards[[i]]
    if(typ=="+4" || typ=="wild") {
      if(c$typ=="+4" || c$typ=="wild") {
        if(allowBlackOnBlack) {
          output <- append(output, list(i))
        }
      } else if(c$color==nextColor) {
        output <- append(output, list(i))
      }
    } else if(c$typ==typ) {
      output <- append(output, list(i))
    } else if(c$color==nextColor) {
      output <- append(output, list(i))
    } else if(c$color=="black") {
      output <- append(output, list(i))
    }
  }
  
 return(output)
}

#----------------------#
#### createCardDeck ####
#----------------------#
#creates the total deck of cards

createCardDeck <- function(colors, numbers, specials, wilds) {
  
  cards <- NULL
  for(col in colors) {
    cards <- append(cards, list(X=list(color=col, typ="0")))
    for(i in 1:9) {
      for(j in 1:2) {
        cards <- append(cards, list(X=list(color=col, typ=toString(i))))
      }
    }
    for(i in 1:2) {
      for(s in specials) {
        cards <- append(cards, list(X=list(color=col, typ=s)))
      }
    }
  }
  
  for(i in 1:4) {
    for(w in wilds){
      cards <- append(cards, list(X=list(color="black", typ=w)))
    }
  }
  names(cards) <- NULL
  return(cards)
}

#----------------------#
#### drawInitCards ####
#----------------------#
#draws the initial set of cards for each player

drawInitCards <- function(init_draw, num_player) {
  hands <- NULL
  for(i in 1:num_player) {
    hands <- append(hands, list(drawCards(init_draw)))
  }
  return(hands)
}

#----------------------#
#### printHands ####
#----------------------#
#prints the cards in each players hand
printHands <- function(hands, player=NULL) {
  txt<-""
  if(is.null(player)) {
    for(i in 1:length(hands)) {
      txt<-paste(txt," \n ", "--- Player ",i, " --- \n", sep="")
      j <- 0
      for(c in hands[[i]]) {
        txt <- paste(txt, "Card ", j, ": ", c$typ, "-", c$color,"\n")
        j <- j+1
      }
    }
  } else {
    for(c in hands[[player]]) {
      txt <- paste(txt, "Card ", j, ": ", c$typ, "-", c$color,"\n")
      j <- j+1
    }
  }
  return(txt)
}