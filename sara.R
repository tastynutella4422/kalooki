library(purrr)
library(dplyr)

## 2 Decks of Cards (2-3 person game)

suit.names = c('spades', 'clubs', 'hearts', 'diamonds')
suits = unlist(map(suit.names, rep, 26))

face.names = c('king', 'queen', 'jack', 'ten', 'nine', 'eight', 'seven', 'six', 'five', 'four', 'three', 'two', 'ace')
faces = rep(face.names, 8)

order = c(1:104)

deck = data.frame(faces,suits,order)
joker = c('joker')
faces = rep(joker,4)
joker.suits = c('NA')
suits = rep(joker.suits,4)
order = c(105:108)
total.jokers = data.frame(faces,suits,order)
deck = rbind(deck,total.jokers)

#Deal Function (num cards dealt as parameter)
deal = function(n, deck) {
  #Create empty data frames for player1 and player2
  player1_cards = data.frame()
  player2_cards = data.frame()
  
  # Set the number of cards to deal to each player
  num_cards = n
  
  # Simulate dealing the cards
  for (i in 1:(2 * num_cards)) {
    if (i %% 2 == 1) { # Odd row numbers go to player1
      player1_cards = rbind(player1_cards, deck[i,])
    } else { # Even row numbers go to player2
      player2_cards = rbind(player2_cards, deck[i,])
    }
  }
  
  # Remove the dealt cards from the original deck
  remaining.cards= deck[-(1:(2 * num_cards)), ]
  
  return(list(v1=player1_cards, v2=player2_cards, v3=remaining.cards))
}

#Beginning of each round setup - dealing & discard pile
setup = function(n, deck) {
  num.cards = n
  
  #shuffle the deck first
  shuffled.deck = deck[sample(nrow(deck), nrow(deck), replace = FALSE), ]
  
  resulting.hands = deal(num.cards, shuffled.deck)
  p1.hand = resulting.hands$v1
  p2.hand = resulting.hands$v2
  remaining = resulting.hands$v3
  
  top.discard.card = remaining[1,] #assigns the top value of the remaining cards
  
  stock.pile = remaining[-1, ]
  print(top.discard.card)
  return(list(v1=p1.hand, v2=p2.hand, v3=top.discard.card, v4=stock.pile))
}


#Round 1: Three 3s

init.cards = setup(12, deck)
p1.hand = init.cards$v1
p2.hand = init.cards$v2
top.discard = init.cards$v3
stock.pile = init.cards$v4
choose = runif(1) #decides which player goes first
if (choose <= .5) {
  cat("Player 1 goes first")
  first.player = p1.hand
  second.player = p2.hand
} else {
  cat("Player 2 goes first")
  first.player = p2.hand
  second.player = p1.hand
}
discard.stock = runif(1)
if (discard.stock <= .5) {
  first.player = rbind(first.player,top.discard)
  top.discard = NULL
} else {
  top.stock = stock.pile[1, ]
  first.player = rbind(first.player,top.stock)
  stock.pile = stock.pile[-1,]
}

#check for duplicates (mark 3s and sets getting close to 3)
#using face.names from above to keep track of the counts
p1.faces = first.player$faces
face.counts = rep(0,13)
for (i in 1:length(p1.faces)) {
  for (j in 1:length(p1.faces)) {
    if (face.names[i] == p1.faces[j]) {
      face.counts[i] = face.counts[i] + 1
    }
  }
}
for (k in 1:length(face.counts)) {
  if (face.counts[k] >= 3) {
    set.of.three = face.names[k]
  } 
  
}
