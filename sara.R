library(purrr)
library(dplyr)

## 2 Decks of Cards (2-3 person game)

laid.down.cards = data.frame()


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

face.vals <- c("two" = 2, "three" = 3, "four" = 4, "five" = 5, "six" = 6, "seven" = 7,
               "eight" = 8, "nine" = 9, "ten" = 10, "jack" = 11, "queen" = 12, "king" = 13, "joker" = 50)
deck$value = face.vals[deck$faces]
deck$value[deck$faces == "ace" & (deck$suits == "hearts" | deck$suits == "diamonds")] = 1
deck$value[deck$faces == "ace" & (deck$suits == "spades" | deck$suits == "clubs")] = 15


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

#function to determine which card in hand is of highest value (to be used when discarding during each player's turn)
highest.val.card = function(hand) {
  return(hand[which.max(hand$value), ])
}

#function to determine who goes first 
player.order = function(p1.hand,p2.hand) {
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
  return(list(v1=first.player, v2=second.player))
}

#function to determine if player will pick up from discard pile or stock pile
discard.or.stock = function(player) {
  discard.stock = runif(1)
  if (discard.stock <= .5) {
    player = rbind(player,top.discard)
    top.discard = NULL
  } else {
    top.stock = stock.pile[1, ]
    player = rbind(player,top.stock)
    stock.pile = stock.pile[-1,]
  }
  return(list(v1=player, v2=top.discard, v3=stock.pile))
}

#function that checks for 3s and partial 3s, lays down any 3s, and chooses card to discard (excluding sets of 2 since they have higher chance of becoming a 3)
three.threes.gameplay = function(player,total.threes,total.laid.down.cards) {
  set.of.three = c()
  set.of.two = c()
  faces.freq = table(player$faces) #make a table for how many times each face value occurs in the player's hand
  for (k in 1:length(faces.freq)) {
    if (faces.freq[k] == 2) {
      face.name = names(faces.freq[k])
      set.of.two = append(set.of.two,face.name) # vector of sets of 2 (close to a 3)
    }
    if (faces.freq[k] >= 3) {
      face.name = names(faces.freq[k])
      set.of.three = append(set.of.three,face.name) # vector of face vals that occur 3+ times
    } 
  }
  
  if (length(set.of.three > 0)) { #check to see if any cards can be laid down
    for (i in 1:length(set.of.three)) {
      dups = set.of.three[i] 
      laid.down.cards = filter(player, faces %in% dups) # print full card names in p1's hand that were marked as a 3
      total.laid.down.cards = rbind(total.laid.down.cards,laid.down.cards)
    }
    cat("Player laid down: \n")
    print(laid.down.cards)
    total.threes = total.threes + length(set.of.three)
    remaining.hand = anti_join(player,laid.down.cards, by="order")
  } else {
    remaining.hand = player
  }
  
  #convert set.of.two to dataframe so that it can be removed from consideration when discarding cards
  for (i in 1:length(set.of.two)) {
    partial = set.of.two[i]
    partial.sets = filter(remaining.hand, faces %in% partial)
  }
  
  #from the remaining hand, discard the highest value card (elias's function)
  to.discard = anti_join(remaining.hand,partial.sets, by="order")
  player.discard = highest.val.card(to.discard)
  end.of.turn.hand = anti_join(remaining.hand,player.discard, by="order")
  top.discard = player.discard #reassign top of the discard pile
  
  return(list(v1=end.of.turn.hand, v2=total.threes, v3=top.discard, v4=total.laid.down.cards)) 
}

#Round 1: Three 3s

p1.total.threes = 0
p2.total.threes = 0
total.laid.down.cards = data.frame()

init.cards = setup(12, deck)
p1.hand = init.cards$v1
p2.hand = init.cards$v2
top.discard = init.cards$v3
stock.pile = init.cards$v4

players = player.order(p1.hand,p2.hand)
p1.hand = players$v1
p2.hand = players$v2

while ((p1.total.threes < 3) | (p2.total.threes < 3)) {
  picked.up.card = discard.or.stock(p1.hand)
  p1.hand = picked.up.card$v1
  top.discard = picked.up.card$v2
  stock.pile = picked.up.card$v3
  
  p1.gameplay = three.threes.gameplay(p1.hand,p1.total.threes,total.laid.down.cards)
  p1.hand = p1.gameplay$v1
  p1.total.threes = p1.gameplay$v2
  top.discard = p1.gameplay$v3
  total.laid.down.cards = p1.gameplay$v4
  
  picked.up.card = discard.or.stock(p2.hand)
  p2.hand = picked.up.card$v1
  top.discard = picked.up.card$v2
  stock.pile = picked.up.card$v3
  
  p2.gameplay = three.threes.gameplay(p2.hand,p2.total.threes,total.laid.down.cards)
  p2.hand = p2.gameplay$v1
  p2.total.threes = p2.gameplay$v2
  top.discard = p2.gameplay$v3
  total.laid.down.cards = p2.gameplay$v4
}

print("done")




