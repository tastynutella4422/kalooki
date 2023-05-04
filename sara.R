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
  dealt.cards = deck[sample(nrow(deck), n),]
  remaining.cards = anti_join(deck,dealt.cards, by="order")
  return(list(v1=dealt.cards, v2=remaining.cards))
}

#Beginning of each round setup - dealing & discard pile
setup = function(n, deck) {
  num.cards = n
  p1 = deal(num.cards, deck)
  p1.hand = p1$v1
  remaining = p1$v2
  p2 = deal(num.cards, remaining)
  p2.hand = p2$v1
  remaining = p2$v2
  top.card = remaining[sample(nrow(remaining), 1),]
  stock.pile = anti_join(remaining,top.card, by="order")
  print(top.card)
  return(list(v1=p1.hand, v2=p2.hand, v3=top.card, v4=stock.pile))
}


#Round 1: Three 3s

init.cards = setup(12, deck)
p1.hand = init.cards$v1
p2.hand = init.cards$v2
top.discard = init.cards$v3
stock.pile = init.cards$v4
choose = runif(1)
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
  first.player = rbind(first.player,)
}

