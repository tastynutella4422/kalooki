library(purrr)
library(dplyr)

## 2 Decks of Cards (2-3 person game)

suit.names = c('spades', 'clubs', 'hearts', 'diamonds')
suits = unlist(map(suit.names, rep, 26))

face.names = c('king', 'queen', 'jack', 'ten', 'nine', 'eight', 'seven', 'six', 'five', 'four', 'three', 'two', 'ace')
faces = rep(face.names, 8)

deck = data.frame(faces,suits)
joker = c('joker')
faces = rep(joker,4)
joker.suits = c('NA')
suits = rep(joker.suits,4)
total.jokers = data.frame(faces,suits)
deck = rbind(deck,total.jokers)

#Deal Function (num cards dealt as parameter)

deal = function(n) {
  sample_n(deck,n,replace=F)
}
