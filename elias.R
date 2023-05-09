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
discard.stock = runif(1) #decides whether the player decides to choose from the stock pile or the discard pile
if (discard.stock <= .5) {
  first.player = rbind(first.player,top.discard) # if the player chooses from the discard pile, combine the card with the players hand
  top.discard = NULL
  print("Player picks it up from the discard pile")
} else {
  top.stock = stock.pile[1, ] # picks the top card of the stock pile
  first.player = rbind(first.player,top.stock)# if the player chooses from the stock pile, combine the card with the players hand
  stock.pile = stock.pile[-1,] #removes the top card from the stock pile after the player adds it to his hands
  print("ok")
}

three_threes_gameplay = function(player, total.threes, total.laid.down.cards, player_number) {
  set.of.three = c()
  set.of.two = c()
  won = F
  faces.freq = table(player$faces)
  for (k in 1:length(faces.freq)) {
    if (faces.freq[k] >= 3) {
      face.name = names(faces.freq[k])
      set.of.three = append(set.of.three, face.name)
    }
  }
  
  if (length(set.of.three > 0)) {
    for (i in 1:length(set.of.three)) {
      dups = set.of.three[i]
      laid.down.cards = filter(player, faces %in% dups)
      total.laid.down.cards = rbind(total.laid.down.cards, laid.down.cards)
    }
    cat("\n--------------------------------------\n")
    cat(sprintf("Player %d laid down the following cards:\n", player_number))
    print(laid.down.cards)
    cat("--------------------------------------\n")
    total.threes = total.threes + length(set.of.three)
    remaining.hand = anti_join(player, laid.down.cards, by = "order")
  } else {
    remaining.hand = player
  }
  
  if (length(remaining.hand$order) == 0) {
    won = T
    print(sprintf("Player %d has won", player_number))
    return(list(v1 = data.frame(), v2 = total.threes, v3 = top.discard, v4 = total.laid.down.cards, v5 = won))
  }
  
  if (total.threes >= 3) {
    cur.faces = remaining.hand$faces
    n = length(cur.faces)
    for (i in 1:n) {
      tack.on = data.frame()
      to.check = cur.faces[i]
      if (to.check %in% total.laid.down.cards$faces) {
        tack.on = filter(remaining.hand, faces %in% to.check)
        total.laid.down.cards = rbind(total.laid.down.cards, tack.on)
        remaining.hand = anti_join(remaining.hand, tack.on, by = "order")
      }
    }
    if (length(remaining.hand$order) == 0) {
      won = T
      print(sprintf("Player %d has won", player_number))
      return(list(v1 = data.frame(), v2 = total.threes, v3 = top.discard, v4 = total.laid.down.cards, v5 = won))
    }
  }
  
  faces.freq = table(remaining.hand$faces)
  for (k in 1:length(faces.freq)) {
    if (faces.freq[k] == 2) {
      face.name = names(faces.freq[k])
      set.of.two = append(set.of.two, face.name)
    }
  }
  for (i in 1:length(set.of.two)) {
    partial = set.of.two[i]
    partial.sets = filter(remaining.hand, faces %in% partial)
  }
  
  to.discard = anti_join(remaining.hand, partial.sets, by = "order")
  player.discard = highest.val.card(to.discard)
  end.of.turn.hand = anti_join(remaining.hand, player.discard, by = "order")
  top.discard = player.discard
  
  if (length(remaining.hand$order) == 0) {
    won = T
    print(sprintf("Player %d has won", player_number))
    return(list(v1 = data.frame(), v2 = total.threes, v3 = top.discard, v4 = total.laid.down.cards, v5 = won))
  }
}

faces.freq = table(remaining.hand$faces)
for (k in 1:length(faces.freq)) {
  if (faces.freq[k] == 2) {
    face.name = names(faces.freq[k])
    set.of.two = append(set.of.two, face.name)
  }
}
for (i in 1:length(set.of.two)) {
  partial = set.of.two[i]
  partial.sets = filter(remaining.hand, faces %in% partial)
}

to.discard = anti_join(remaining.hand, partial.sets, by = "order")
player.discard = highest.val.card(to.discard)
end.of.turn.hand = anti_join(remaining.hand, player.discard, by = "order")
top.discard = player.discard

if (length(remaining.hand$order) == 0) {
  won = T
  print(sprintf("Player %d has won", player_number))
  return(list(v1 = data.frame(), v2 = total.threes, v3 = top.discard,


#Round 1: Three 3s

p1.total.threes = 0
p2.total.threes = 0
total.laid.down.cards = data.frame()
won = F

init.cards = setup(12, deck)
p1.hand = init.cards$v1
p2.hand = init.cards$v2
top.discard = init.cards$v3
stock.pile = init.cards$v4

players = player.order(p1.hand,p2.hand)
p1.hand = players$v1
p2.hand = players$v2

while (won == F) {
  picked.up.card = discard.or.stock(p1.hand)
  p1.hand = picked.up.card$v1
  top.discard = picked.up.card$v2
  stock.pile = picked.up.card$v3
  
  if (p1.total.threes >= 3) {
    p1.gameplay = three.threes.with.tackon.gameplay(p1.hand,p1.total.threes,total.laid.down.cards)
    p1.hand = p1.gameplay$v1
    p1.total.threes = p1.gameplay$v2
    top.discard = p1.gameplay$v3
    total.laid.down.cards = p1.gameplay$v4 
    won = p1.gameplay$v5
  } else {
    p1.gameplay = three.threes.gameplay(p1.hand,p1.total.threes,total.laid.down.cards)
    p1.hand = p1.gameplay$v1
    p1.total.threes = p1.gameplay$v2
    top.discard = p1.gameplay$v3
    total.laid.down.cards = p1.gameplay$v4
    won = p1.gameplay$v5
  }
  
  picked.up.card = discard.or.stock(p2.hand)
  p2.hand = picked.up.card$v1
  top.discard = picked.up.card$v2
  stock.pile = picked.up.card$v3
  
  if (p2.total.threes >= 3) {
    p2.gameplay = three.threes.with.tackon.gameplay(p2.hand,p2.total.threes,total.laid.down.cards)
    p2.hand = p2.gameplay$v1
    p2.total.threes = p2.gameplay$v2
    top.discard = p2.gameplay$v3
    total.laid.down.cards = p2.gameplay$v4
    won = p2.gameplay$v5
    
  } else {
    p2.gameplay = three.threes.gameplay(p2.hand,p2.total.threes,total.laid.down.cards)
    p2.hand = p2.gameplay$v1
    p2.total.threes = p2.gameplay$v2
    top.discard = p2.gameplay$v3
    total.laid.down.cards = p2.gameplay$v4
    won = p2.gameplay$v5
  }
}






