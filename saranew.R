

library(purrr)
library(dplyr)
library(plyr)

test.player = data.frame(faces=c("ace","king","jack","four","five","queen"), suits=c("hearts","hearts","hearts","hearts","hearts","hearts"), 
                         order=c(1,2,3,4,5,6), value=c(15,13,11,4,5,12), name=c("Player 1","Player 1","Player 1","Player 1","Player 1","Player 1"))

p1.calls = 0
p2.calls = 0
p3.calls = 0
p4.calls = 0

# Metrics to Track
total.rounds = c()
p1.scores = c()
p2.scores = c()
p3.scores = c()
p4.scores = c()

p1.tacked = 0
p2.tacked = 0
p3.tacked = 0
p4.tacked = 0

## 2 Decks of Cards

p1.score = 0
p2.score = 0
p3.score = 0
p4.score = 0

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
deck$name = "NA"

discard.pile = data.frame()

#Deal Function (num cards dealt as parameter)
deal = function(n, deck) {
  #Create empty data frames for player1 and player2
  player1_cards = data.frame()
  player2_cards = data.frame()
  player3_cards = data.frame()
  player4_cards = data.frame()
  
  # Set the number of cards to deal to each player
  num_cards = n
  
  # Simulate dealing the cards
  for (i in 1:(4 * num_cards)) {
    if (i %% 4 == 1) { # First cards go to player1
      player1_cards = rbind(player1_cards, deck[i,])
    } else if (i %% 4 == 2) { # Second cards go to player2
      player2_cards = rbind(player2_cards, deck[i,])
    } else if (i %% 4 == 3) { # Third cards go to player3
      player3_cards = rbind(player3_cards, deck[i,])
    } else { # Fourth cards go to player4
      player4_cards = rbind(player4_cards, deck[i,])
    }
  }
  
  # Remove the dealt cards from the original deck
  remaining.cards= deck[-(1:(4 * num_cards)), ]
  
  return(list(v1=player1_cards, v2=player2_cards, v3=player3_cards, v4=player4_cards, v5=remaining.cards))
}

#Beginning of each round setup - dealing & discard pile
setup = function(n, deck) {
  num.cards = n
  
  #shuffle the deck first
  shuffled.deck = deck[sample(nrow(deck), nrow(deck), replace = FALSE), ]
  
  resulting.hands = deal(num.cards, shuffled.deck)
  p1.hand = resulting.hands$v1
  p2.hand = resulting.hands$v2
  p3.hand = resulting.hands$v3
  p4.hand = resulting.hands$v4
  remaining = resulting.hands$v5
  
  top.discard.card = remaining[1,] #assigns the top value of the remaining cards
  discard.pile = rbind(discard.pile, top.discard.card)
  
  stock.pile = remaining[-1, ]
  print(top.discard.card)
  return(list(v1=p1.hand, v2=p2.hand, v3=p3.hand, v4 = p4.hand, v5=top.discard.card, v6=stock.pile, v7=discard.pile))
}

#function to determine which card in hand is of highest value (to be used when discarding during each player's turn)
highest.val.card = function(hand) {
  return(hand[which.max(hand$value), ])
}

#function to determine who goes first 
player.order = function(p1.hand,p2.hand, p3.hand, p4.hand) {
  choose = runif(1) #decides which player goes first
  if (choose <= .25) {
    cat("Player 1 goes first")
    first.player = p1.hand
    second.player = p2.hand
    third.player = p3.hand
    fourth.player = p4.hand
  } else if (choose <= .5)  {
    cat("Player 2 goes first")
    first.player = p2.hand
    second.player = p1.hand
    third.player = p4.hand
    fourth.player = p3.hand 
  }
  else if (choose <= .75) {
    first.player = p3.hand
    second.player = p4.hand
    third.player = p1.hand
    fourth.player = p2.hand 
  }
  else {
    first.player = p4.hand
    second.player = p3.hand
    third.player = p2.hand
    fourth.player = p1.hand 
  }
  first.player$name = "Player 1"
  second.player$name = "Player 2"
  third.player$name = "Player 3"
  fourth.player$name = "Player 4"
  return(list(v1=first.player, v2=second.player, v3 = third.player, v4 = fourth.player))
}

#function to determine if player will pick up from discard pile or stock pile
discard.or.stock = function(player) {
  discard.stock = runif(1)
  if (discard.stock <= .5) {
    player = rbind(player,top.discard)
    discard.pile = anti_join(discard.pile,top.discard,by="order")
    if(length(discard.pile)>0) {
      top.discard = discard.pile[length(discard.pile$faces),]
    }
    else {
      top.discard = NULL
    }
  } else {
    top.stock = stock.pile[1, ]
    player = rbind(player,top.stock)
    stock.pile = anti_join(stock.pile, top.stock, by="order")
  }
  return(list(v1=player, v2=top.discard, v3=stock.pile, v4=discard.pile))
}

#function that checks for 3s and partial 3s, lays down any 3s, and chooses card to discard (excluding sets of 2 since they have higher chance of becoming a 3)
finding.threes = function(player, total.threes, total.laid.down.cards, tack_on, all=T) {
  player.tacked = 0
  won = F
  
  # Find sets in the player's hand
  sets = find_sets(player)
  set_of_two = sets$v2
  set_of_three = sets$v1
  total.threes = total.threes + length(sets$v1)
  joker.cards = filter(player, faces %in% "joker")
  
  # Convert set_of_two to partial_sets dataframe
  partial_sets = data.frame()
  for (i in 1:length(set_of_two)) {
    partial = set_of_two[i]
    partial_set = filter(player, faces %in% partial)
    partial_sets = rbind(partial_sets, partial_set)
  }

  joker.set = F
  
  if(all == F) {
    if ((length(set_of_two) > 0) & (length(joker.cards$order) > 0)) {
      double.to.triple = set_of_two[1]
      jokers.to.add = joker.cards %>% slice(1)
      to.add = filter(player, faces %in% double.to.triple)
      doubles.added = rbind(jokers.to.add, to.add)
      if (length(doubles.added$order) > 0) {
        player = anti_join(player,doubles.added, by="order")
        partial_sets = anti_join(partial_sets,doubles.added,by="order")
      }
      total.laid.down.cards = rbind(total.laid.down.cards,doubles.added)
      if (length(doubles.added$order) > 0) {
        cat(player$name[1],"laid down a joker in combination with two cards: \n")
        print(doubles.added)
      }
      total.threes = total.threes + 1
      joker.set = T
    }
    if(joker.set == F){
      lay_down_result = lay_down_threes(player, set_of_three, total.laid.down.cards, all)
      player = lay_down_result$v1
      total.laid.down.cards = lay_down_result$v2
    }
  }
  else {
    if ((length(set_of_two) > 0) & (length(joker.cards$order) > 0)) {
      lengths = c(length(set_of_two), length(joker.cards$order))
      min.val = min(lengths)
      for (y in 1:min.val) {
        double.to.triple = set_of_two[y]
        jokers.to.add = joker.cards %>% slice(y)
        to.add = filter(player, faces %in% double.to.triple)
        doubles.added = rbind(jokers.to.add, to.add)
        if (length(doubles.added$order) > 0) {
          player = anti_join(player,doubles.added, by="order")
          partial_sets = anti_join(partial_sets,doubles.added,by="order")
        }
        total.laid.down.cards = rbind(total.laid.down.cards,doubles.added)
        if (length(doubles.added$order) > 0) {
          cat(player$name[1],"laid down a joker in combination with two cards: \n")
          print(doubles.added)
        }
      }
      total.threes = total.threes + min.val
    }
    
    # Lay down threes
    lay_down_result = lay_down_threes(player, set_of_three, total.laid.down.cards, all)
    player = lay_down_result$v1
    total.laid.down.cards = lay_down_result$v2
  }
  
  # Check if the player has won
  if (length(player$order) == 0) {
    won = T
    return(list(v1=player, v2=total.threes, v3=top.discard, v4=total.laid.down.cards, v5=won, v6=partial_sets, v7=player.tacked))
  }
  
  # Tack on cards
  if (tack_on == T) {
    tack_on_result = tack_on_cards(player, total.laid.down.cards)
    player = tack_on_result$v1
    total.laid.down.cards = tack_on_result$v2
    player.tacked = player.tacked + tack_on_result$v3
  }
  
  # Check if the player has won
  if (length(player$order) == 0) {
    won = T
    return(list(v1=player, v2=total.threes, v3=top.discard, v4=total.laid.down.cards, v5=won, v6=partial_sets, v7=player.tacked))
  }
  
  return(list(v1=player, v2=total.threes, v3=top.discard, v4=total.laid.down.cards, v5=won, v6=partial_sets, v7=player.tacked))
}


find_sets = function(player) {
  set.of.three = c()
  set.of.two = c()
  
  joker.cards = filter(player, faces %in% "joker")
  find.sets = anti_join(player, joker.cards, by="order")
  faces.freq = table(find.sets$faces)
  faces.freq
  for (k in 1:length(faces.freq)) {
    if (faces.freq[k] == 2) {
      face.name = names(faces.freq[k])
      set.of.two = append(set.of.two, face.name)
    }
    if (faces.freq[k] >= 3) {
      face.name = names(faces.freq[k])
      set.of.three = append(set.of.three, face.name)
    }
  }
  
  return(list(v1 = set.of.three, v2 = set.of.two))
}

lay_down_threes = function(player, set.of.three, total.laid.down.cards, all= T) {
  if(all == TRUE) {
    if (length(set.of.three) > 0) {
      for (i in 1:length(set.of.three)) {
        face_value = set.of.three[i]
        laid_down_cards = filter(player, faces %in% face_value)
        cat(player$name[1],"laid down a set: \n")
        print(laid_down_cards)
        player = anti_join(player, laid_down_cards, by="order")
        total.laid.down.cards = rbind(total.laid.down.cards, laid_down_cards)
      }
    }
  }
  else {
    if (length(set.of.three) > 0) {
      face_value = set.of.three[1]
      laid_down_cards = filter(player, faces %in% face_value)
      cat(player$name[1],"laid down a set: \n")
      print(laid_down_cards)
      player = anti_join(player, laid_down_cards, by="order")
      total.laid.down.cards = rbind(total.laid.down.cards, laid_down_cards)
      }
    }
  return(list(v1 = player, v2 = total.laid.down.cards))
}

tack_on_cards = function(player, total.laid.down.cards) {
  cur_faces = player$faces
  n = length(cur_faces)
  num.tack.on = 0
  
  for (i in 1:n) {
    to_check = cur_faces[i]
    if (to_check %in% total.laid.down.cards$faces) {
      tack_on = filter(player, faces %in% to_check)
      if (length(tack_on$order) > 0) {
        total.laid.down.cards = rbind(total.laid.down.cards, tack_on)
        player = anti_join(player, tack_on, by="order")
        cat("Player tacked on: \n")
        print(tack_on)
        num.tack.on = num.tack.on + 1
      }
    }
  }
  
  return(list(v1 = player, v2 = total.laid.down.cards, v3 = num.tack.on))
}

discard_card = function(player, partial_sets, partial_runs,option="both") {
  no_discard = data.frame()
  jokers = filter(player, faces %in% "joker")
  if (option == "both") {
    no_discard = rbind(partial_sets, partial_runs)
    if (length(no_discard$order) == length(player$order)) {
      player_discard = highest.val.card(partial_sets)
    } else {
      no_discard = rbind(no_discard, jokers)
      to_discard = anti_join(player, no_discard, by="order")
      player_discard = highest.val.card(to_discard)
    }
  } else if (option == "sets") {
    no_discard = rbind(no_discard,partial_sets)
    if (length(no_discard$order) == length(player$order)) {
      player_discard = highest.val.card(partial_sets)
    } else {
      no_discard = rbind(no_discard, jokers)
      to_discard = anti_join(player, no_discard, by="order")
      player_discard = highest.val.card(to_discard)
    }
  } else if (option == "runs") {
    no_discard = rbind(no_discard,partial_runs)
    if (length(no_discard$order) == length(player$order)) {
      player_discard = highest.val.card(partial_runs)
    } else {
      no_discard = rbind(no_discard, jokers)
      to_discard = anti_join(player, no_discard, by="order")
      player_discard = highest.val.card(to_discard)
    }
  }
  
  # From the remaining hand, discard the highest value card
  player = anti_join(player, player_discard, by="order")
  top_discard = player_discard # Reassign top of the discard pile
  discard.pile = rbind(discard.pile, player_discard)
  
  return(list(v1=player, v2=top_discard, v3=discard.pile))
}

#function to keep track of the score after each round
tally.score = function(p1.hand,p1.score,p2.hand,p2.score,p3.hand,p3.score,p4.hand,p4.score) {
  if ((length(p1.hand$order)) == 0) {
    p2.score = p2.score + (sum(p2.hand$value))
    p3.score = p3.score + (sum(p3.hand$value))
    p4.score = p4.score + (sum(p4.hand$value))
  } else if ((length(p2.hand$order)) == 0) {
    p1.score = p1.score + (sum(p1.hand$value))
    p3.score = p3.score + (sum(p3.hand$value))
    p4.score = p4.score + (sum(p4.hand$value))
  } else if ((length(p3.hand$order)) == 0) {
    p1.score = p1.score + (sum(p1.hand$value))
    p2.score = p2.score + (sum(p2.hand$value))
    p4.score = p4.score + (sum(p4.hand$value))
  } else {
    p1.score = p1.score + (sum(p1.hand$value))
    p2.score = p2.score + (sum(p2.hand$value))
    p3.score = p3.score + (sum(p3.hand$value))
  }
  return(list(v1=p1.score,v2=p2.score,v3=p3.score,v4=p4.score ))
}

find.runs = function(player) {
  num.runs = 0
  player.runs = data.frame()
  partial.runs = data.frame()
  run = data.frame()
  joker.cards = filter(player, faces %in% "joker")
  aces = filter(player, faces %in% "ace")
  aces.suits = aces$suits
  aces.suits = unique(aces.suits)
  to.ignore = rbind(joker.cards,aces)
  to.find = anti_join(player, to.ignore, by="order")
  new.beginning = data.frame()
  
  # Sort the player's hand by suit and face value
  sorted.hand = to.find[order(to.find$suits,to.find$value), ]
  hand.suits = sorted.hand$suits
  hand.suits = unique(hand.suits)
  
  # Loop through each unique suit in the sorted hand
  for (i in 1:length(hand.suits)) {
    # Get all cards with the current suit
    cards.of.suit = filter(sorted.hand, suits %in% hand.suits[i])
    
    run = data.frame()
    new.beginning = data.frame()
    # Initialize variables to keep track of consecutive cards and the current run
    # consecutive.cards = 1
    
    # Loop through the remaining cards in the suit_cards dataframe
    num.cards = length(cards.of.suit$order)
    
    if (num.cards >= 3) {
      for (j in 2:num.cards) {
        # If the current card's face value is 1 more than the previous card's face value,
        # increment the consecutive_cards counter and add the current card to the run
        if ((cards.of.suit$value[j]) == ((cards.of.suit$value[j - 1]) + 1)) {
          #  consecutive.cards = consecutive.cards + 1
          first.card = cards.of.suit %>% slice(1)
          if ((nrow(merge(first.card,run)) == 0) & (length(new.beginning$order) == 0)) {
            run = rbind(run,first.card)
          }
          to.add = cards.of.suit %>% slice(j)
          if ((nrow(merge(to.add,run)) == 0)) {
            run = rbind(run,to.add)
          }
        } else {
          # If not, reset the consecutive_cards counter and start a new run with the current card
          # consecutive.cards = 1
          new.beginning = cards.of.suit %>% slice(j)
          run = data.frame()
          run = rbind(run,new.beginning)
        }
      }
    }
    
    if (length(run$order) >= 4 & (length(aces$order) == 0)) {
      player.runs = rbind(player.runs,run)
      num.runs = num.runs + 1
    }
    
    if ((length(run$order) >= 3) & (length(aces$order) > 0)) {
      first = run %>% slice(1)
      last = run %>% slice(length(run$order))
      for (k in 1:length(aces.suits)) {
        if ((aces.suits[k] == first$suits) & (first$faces == "two")) {
          add.ace = aces %>% slice(k)
          new.four = rbind(add.ace,run)
          aces = anti_join(add.ace,aces,by="order")
          player.runs = rbind(player.runs,new.four)
          num.runs = num.runs + 1
          
        } else if ((aces.suits[k] == last$suits) & (last$faces == "king")) {
          add.ace = aces %>% slice(k)
          new.four = rbind(run,add.ace)
          aces = anti_join(add.ace,aces,by="order")
          player.runs = rbind(player.runs,new.four)
          num.runs = num.runs + 1
          
        } else {
          partial.runs = rbind(partial.runs,run)
          
        }
      }
    }
    
    #check if there are any jokers to add to partial run
    if ((length(run$order) >= 3) & (length(joker.cards$order) > 0)) {
      #lengths = c(length(partial.runs$order), length(joker.cards$order))
      #min.val = min(lengths)
      #for (y in 1:min.val) {
      jokers.to.add = joker.cards %>% slice(1)
      new.run = rbind(jokers.to.add, run)
      player.runs = rbind(player.runs,new.run)
      #}
      num.runs = num.runs + 1
    } else {
      partial.runs = rbind(partial.runs,run)
    }
  }
  # Return the list of runs found in the player's hand
  return(list(v1=player,v2=player.runs,v3=partial.runs,v4=num.runs))
}

lay_down_fours = function(player, player.runs, total.runs) {
  if (length(player.runs$order) > 0) {
    cat("Player laid down: \n")
    print(player.runs)
    player = anti_join(player, player.runs, by="order")
    add.to.order = add.to.order + 1
    player.runs$add.order = add.to.order 
    total.runs = rbind(total.runs, player.runs)
  }
  return(list(v1 = player, v2 = total.runs, v3 = add.to.order))
}

tack_on_run = function(player, total.runs) {
  num.runs = length(unique(total.runs$add.order))
  num.tack.on = 0
  for (i in 1:num.runs) {
    joker.cards = filter(player, faces %in% "joker")
    player.no.jokers = anti_join(player,joker.cards,by="order")
    cur.run = filter(total.runs, add.order %in% i)
    suit.cur.run = unique(cur.run$suits)
    first.card = cur.run %>% slice(1)
    last.card = cur.run %>% slice(length(cur.run$order))
    cards.of.suit = filter(player.no.jokers, suits %in% suit.cur.run)
    cards.of.suit.values = unique(cards.of.suit$values)
    if (length(cards.of.suit.values) > 0) {
      for (j in 1:length(cards.of.suit.values)) {
        if (cards.of.suit.values[j] == (first.card$value - 1)) {
          total.runs = anti_join(total.runs,cur.run, by="order")
          card.to.add = filter(cards.of.suit, value %in% cards.of.suit.values[j])
          card.to.add = card.to.add %>% slice(1)
          card.to.add$add.order = i
          cur.run = rbind(card.to.add,cur.run)
          total.runs = rbind(cur.run,total.runs)
          player = anti_join(player,card.to.add, by="order")
          num.tack.on = num.tack.on + 1
        } else if (cards.of.suit.values[j] == (last.card$value + 1)) {
          total.runs = anti_join(total.runs,cur.run, by="order")
          card.to.add = filter(cards.of.suit, value %in% cards.of.suit.values[j])
          card.to.add = card.to.add %>% slice(1)
          card.to.add$add.order = i
          cur.run = rbind(cur.run,card.to.add)
          total.runs = rbind(cur.run,total.runs)
          player = anti_join(player,card.to.add, by="order")
          num.tack.on = num.tack.on + 1
        }
      }
      if (length(joker.cards$order) > 0) {
        if ((first.card$faces != "joker") & (last.card$faces != "joker")) {
          total.runs = anti_join(total.runs,cur.run,by="order")
          card.to.add = filter(joker.cards, face %in% "joker")
          card.to.add = card.to.add %>% slice(1)
          card.to.add$add.order = i
          cur.run = rbind(cur.run, card.to.add)
          total.runs = rbind(cur.run,total.runs)
          player = anti_join(player,card.to.add,by="order")
          num.tack.on = num.tack.on + 1
          joker.cards = anti_join(joker.cards,card.to.add)
        } else if ((first.card$faces == "joker") & (last.card$faces != "joker")) {
          total.runs = anti_join(total.runs,cur.run,by="order")
          card.to.add = filter(joker.cards, face %in% "joker")
          card.to.add = card.to.add %>% slice(1)
          card.to.add$add.order = i
          cur.run = rbind(cur.run, card.to.add)
          total.runs = rbind(cur.run,total.runs)
          player = anti_join(player,card.to.add,by="order")
          num.tack.on = num.tack.on + 1
          joker.cards = anti_join(joker.cards,card.to.add)
        } else if ((first.card$faces != "joker") & (last.card$faces == "joker")) {
          total.runs = anti_join(total.runs,cur.run,by="order")
          card.to.add = filter(joker.cards, face %in% "joker")
          card.to.add = card.to.add %>% slice(1)
          card.to.add$add.order = i
          cur.run = rbind(card.to.add,cur.run)
          total.runs = rbind(cur.run,total.runs)
          player = anti_join(player,card.to.add,by="order")
          num.tack.on = num.tack.on + 1
          joker.cards = anti_join(joker.cards,card.to.add)
        } else {
          
        }
      }
    }
  }
  return(list(v1 = player, v2 = total.runs, v3 = num.tack.on))
}



finding.fours = function(player, total.fours, num.fours, tack_on) {
  player.tacked = 0
  won = F
  cur.runs = find.runs(player)
  player = cur.runs$v1
  player.runs = cur.runs$v2
  partial.runs = cur.runs$v3
  num.fours = num.fours + cur.runs$v4

  # Lay down fours
  laying.down = lay_down_fours(player,player.runs,total.runs) 
  player = laying.down$v1
  total.fours = laying.down$v2
  add.to.order = laying.down$v3
    
  # Check if the player has won
  if (length(player$order) == 0) {
    won = T
    return(list(v1=player, v2=num.fours, v3=top.discard, v4=total.fours, v5=won, v6=partial.runs, v7 = add.to.order, v8=player.tacked))
  }
  
  if (tack_on == T) {
    tack_on_result = tack_on_run(player, total.fours)
    player = tack_on_result$v1
    total.fours = tack_on_result$v2
    player.tacked = player.tacked + tack_on_result$v3
  }
  
  # Check if the player has won
  if (length(player$order) == 0) {
    won = T
    return(list(v1=player, v2=total.threes, v3=top.discard, v4=total.fours, v5=won, v6=partial_sets, v7=add.to.order, v8=player.tacked))
  }
  
  
  return(list(v1=player,v2=num.fours,v3=top.discard,v4=total.fours,v5=won,v6=partial.runs, v7 = add.to.order, v8=player.tacked))

}

#function to determine if anyone is "calling" the discard card (not person who has next turn)
#if call is successful, pick up discard card + 1 from stock pile, can't lay down any cards or tack on
#limit of 3 calls per game, once you lay down cards you can't call anymore
calling = function(type="both",other.p1,other.p2,p1.calls,p2.calls) {
  sim.p1.hand = rbind(other.p1,top.discard)
  sim.p2.hand = rbind(other.p2,top.discard)
  help.p1 = F
  help.p2 = F
  caller = data.frame()
  call = F
  if (type == "set") {
    if (p1.calls < 3) {
      discard.face = top.discard$faces
      p1.sets = find_sets(sim.p1.hand)
      threes = p1.sets$v1
      partial.sets = p1.sets$v2
      if (length(discard.face) > 0) {
        if (((discard.face %in% threes) | (discard.face %in% partial.sets))) {
          help.p1 = T
        }
      }
    } else {
      return(list(v1=other.p1, v2=other.p2, v3=call, v4=discard.pile, v5=top.discard, v6=stock.pile, v7=p1.calls, v8=p2.calls))
    }
    if (p2.calls < 3) {
      p2.sets = find_sets(sim.p2.hand)
      threes = p2.sets$v1
      partial.sets = p2.sets$v2
      if ((length(discard.face) > 0)) {
        if (((discard.face %in% threes) | (discard.face %in% partial.sets))) {
          help.p2 = T
        }
      }
    } else {
      return(list(v1=other.p1, v2=other.p2, v3=call, v4=discard.pile, v5=top.discard, v6=stock.pile, v7=p1.calls, v8=p2.calls))
    }
  } else if (type == "run") {
    discard.order = top.discard$order
    if (p1.calls < 3) {
      p1.runs = find.runs(sim.p1.hand)
      fours = p1.runs$v2
      partial.runs = p1.runs$v3
      fours.orders = fours$order
      partial.orders = partial.runs$order
      if ((length(discard.order) > 0)) {
        if (((discard.order %in% fours.orders) | (discard.order %in% partial.orders))) {
          help.p1 = T
        }
      }
    } else {
      return(list(v1=other.p1, v2=other.p2, v3=call, v4=discard.pile, v5=top.discard, v6=stock.pile, v7=p1.calls, v8=p2.calls))
    }
    if (p2.calls < 3) {
      p2.runs = find.runs(sim.p2.hand)
      fours = p2.runs$v2
      partial.runs = p2.runs$v3
      fours.orders = fours$order
      partial.orders = partial.runs$order
      if ((length(discard.order) > 0)) {
        if (((discard.order %in% fours.orders) | (discard.order %in% partial.orders))) {
          help.p2 = T
        }
      }
    } else {
      return(list(v1=other.p1, v2=other.p2, v3=call, v4=discard.pile, v5=top.discard, v6=stock.pile, v7=p1.calls, v8=p2.calls))
    }
  } else if (type == "both") {
    discard.face = top.discard$faces
    discard.order = top.discard$order
    if (p1.calls < 3) {
      p1.sets = find_sets(sim.p1.hand)
      threes = p1.sets$v1
      partial.sets = p1.sets$v2
      if ((length(discard.face) > 0)) {
        if (((discard.face %in% threes) | (discard.face %in% partial.sets))) {
          help.p1 = T
        }
      }
      p1.runs = find.runs(sim.p1.hand)
      fours = p1.runs$v2
      partial.runs = p1.runs$v3
      fours.orders = fours$order
      partial.orders = partial.runs$order
      if ((length(discard.order) > 0)) {
        if (((discard.order %in% fours.orders) | (discard.order %in% partial.orders))) {
          help.p1 = T
        }
      }
    } else {
      return(list(v1=other.p1, v2=other.p2, v3=call, v4=discard.pile, v5=top.discard, v6=stock.pile, v7=p1.calls, v8=p2.calls))
    }

    if (p2.calls < 3) {
      p2.sets = find_sets(sim.p2.hand)
      threes = p2.sets$v1
      partial.sets = p2.sets$v2
      if ((length(discard.face) > 0)) {
        if (((discard.face %in% threes) | (discard.face %in% partial.sets))) {
          help.p2 = T
        }
      }
      p2.runs = find.runs(sim.p2.hand)
      fours = p2.runs$v2
      partial.runs = p2.runs$v3
      fours.orders = fours$order
      partial.orders = partial.runs$order
      if ((length(discard.order) > 0)) {
        if (((discard.order %in% fours.orders) | (discard.order %in% partial.orders))) {
          help.p2 = T
        }
      }
    } else {
      return(list(v1=other.p1, v2=other.p2, v3=call, v4=discard.pile, v5=top.discard, v6=stock.pile, v7=p1.calls, v8=p2.calls))
    }
  }
  
  if ((help.p1 == T) & (help.p2 == T)) {
    choose = runif(1)
    if (choose >= .5) {
      caller = other.p1
      name = "other.p1"
    } else {
      caller = other.p2
      name = "other.p2"
    }
  } else if ((help.p1 == T) & (help.p2 == F)) {
    caller = other.p1
    name = "other.p1"
  } else if ((help.p1 == F) & (help.p2 == T)) {
    caller = other.p2
    name = "other.p2"
  } else {
    return(list(v1=other.p1, v2=other.p2, v3=call, v4=discard.pile, v5=top.discard, v6=stock.pile, v7=p1.calls, v8=p2.calls))
  }
  
  call.allowed = runif(1)
  if (call.allowed >= .5) {
    caller.name = caller$name[1]
    cat(caller.name, " successfully called a card \n")
    call = T
    top.stock = stock.pile[1, ]
    stock.pile = anti_join(stock.pile,top.stock,by="order")
    to.add = rbind(top.discard,top.stock)
    caller = rbind(caller,to.add)
    discard.pile = anti_join(discard.pile,top.discard,by="order")
    top.discard = data.frame()
  } else {
    print("Call was denied")
    return(list(v1=other.p1, v2=other.p2, v3=call, v4=discard.pile, v5=top.discard, v6=stock.pile, v7=p1.calls, v8=p2.calls))
  }
  
  if (name == "other.p1") {
    other.p1 = data.frame()
    other.p1 = caller
    if (call == T) {
      p1.calls = p1.calls + 1
    }
  } else if (name == "other.p2") {
    other.p2 = data.frame()
    other.p2 = caller
    if (call == T) {
      p2.calls = p2.calls + 1
    }
  } else {
    return(list(v1=other.p1, v2=other.p2, v3=call, v4=discard.pile, v5=top.discard, v6=stock.pile, v7=p1.calls, v8=p2.calls))
  }
  
  return(list(v1=other.p1, v2=other.p2, v3=call, v4=discard.pile, v5=top.discard, v6=stock.pile, v7=p1.calls, v8=p2.calls))
}

#------------------------------------------------------------------------------#

for (z in 1:50) {
  #Round 1: Two 3s
  
  subround.total = 0
  p1.total.threes = 0
  p2.total.threes = 0
  p3.total.threes = 0
  p4.total.threes = 0
  total.threes = data.frame()
  won = F
  
  init.cards = setup(12, deck)
  p1.hand = init.cards$v1
  p2.hand = init.cards$v2
  p3.hand = init.cards$v3
  p4.hand = init.cards$v4
  top.discard = init.cards$v5
  stock.pile = init.cards$v6
  discard.pile = init.cards$v7
  
  players = player.order(p1.hand,p2.hand,p3.hand,p4.hand)
  p1.hand = players$v1
  p2.hand = players$v2
  p3.hand = players$v3
  p4.hand = players$v4
  
  subround = 0
  

  success = F
  
  while (won == F) {
    subround = subround + 1
    
    if ((p2.total.threes == 0) & (p3.total.threes == 0)) {
      call = calling("set",p2.hand,p3.hand,p2.calls,p3.calls)
      p2.hand = call$v1
      p3.hand = call$v2
      success = call$v3
      discard.pile = call$v4
      top.discard = call$v5
      stock.pile = call$v6
      p2.calls = call$v7
      p3.calls = call$v8
    } else if ((p2.total.threes == 0) & (p3.total.threes > 0)) {
      dummy.hand = data.frame()
      call = calling("set",p2.hand,dummy.hand,p2.calls,0)
      p2.hand = call$v1
      success = call$v3
      discard.pile = call$v4
      top.discard = call$v5
      stock.pile = call$v6
      p2.calls = call$v7
    } else if ((p2.total.threes > 0) & (p3.total.threes == 0)) {
      dummy.hand = data.frame()
      call = calling("set",dummy.hand,p3.hand,0,p3.calls)
      p3.hand = call$v2
      success = call$v3
      discard.pile = call$v4
      top.discard = call$v5
      stock.pile = call$v6
      p3.calls = call$v8
    }
    
    cat("------------it is player's 1 turn--------", "\n")
    
    if (length(stock.pile$order) == 0) {
      stock.pile = discard.pile[sample(nrow(discard.pile), nrow(discard.pile), replace = FALSE), ]
      top.discard.card = stock.pile[1,] #assigns the top value of the remaining cards
      discard.pile = data.frame()
      discard.pile = rbind(discard.pile,top.discard.card)
    }
    
    if (success == F) {
      picked.up.card = discard.or.stock(p1.hand)
      p1.hand = picked.up.card$v1
      p1.hand$name = p1.hand$name[1]
      top.discard = picked.up.card$v2
      stock.pile = picked.up.card$v3
      discard.pile = picked.up.card$v4
    } else {
      top.stock = stock.pile[1, ]
      p1.hand = rbind(p1.hand,top.stock)
      stock.pile = anti_join(stock.pile, top.stock, by="order")
    }
    
    if (p1.total.threes >= 2) {
      tack.on = T
    } else {
      tack.on = F
    }
    p1.gameplay = finding.threes(p1.hand,p1.total.threes,total.threes,tack.on)
    p1.hand = p1.gameplay$v1
    p1.total.threes = p1.gameplay$v2
    top.discard = p1.gameplay$v3
    total.threes = p1.gameplay$v4
    won = p1.gameplay$v5
    partial.sets = p1.gameplay$v6
    p1.tacked = p1.tacked + p1.gameplay$v7
    partial.runs = data.frame()
    
    discard_result = discard_card(p1.hand, partial.sets, partial.runs, "sets")
    p1.hand = discard_result$v1
    top.discard = discard_result$v2
    discard.pile = discard_result$v3
    
    
    if (length(p1.hand$faces) == 0) {
      print("Player 1 wins this round")
      break
    }

    if ((p3.total.threes == 0) & (p4.total.threes == 0)) {
      call = calling("set",p3.hand,p4.hand,p3.calls,p4.calls)
      p3.hand = call$v1
      p4.hand = call$v2
      success = call$v3
      discard.pile = call$v4
      top.discard = call$v5
      stock.pile = call$v6
      p3.calls = call$v7
      p4.calls = call$v8
    } else if ((p3.total.threes == 0) & (p4.total.threes > 0)) {
      dummy.hand = data.frame()
      call = calling("set",p3.hand,dummy.hand,p3.calls,0)
      p3.hand = call$v1
      success = call$v3
      discard.pile = call$v4
      top.discard = call$v5
      stock.pile = call$v6
      p3.calls = call$v7
    } else if ((p3.total.threes > 0) & (p4.total.threes == 0)) {
      dummy.hand = data.frame()
      call = calling("set",dummy.hand,p4.hand,0,p4.calls)
      p4.hand = call$v2
      success = call$v3
      discard.pile = call$v4
      top.discard = call$v5
      stock.pile = call$v6
      p4.calls = call$v8
    }
    
    cat("------------it is player's 2 turn--------", "\n")
    
    if (length(stock.pile$order) == 0) {
      stock.pile = discard.pile[sample(nrow(discard.pile), nrow(discard.pile), replace = FALSE), ]
      top.discard.card = stock.pile[1,] #assigns the top value of the remaining cards
      discard.pile = data.frame()
      discard.pile = rbind(discard.pile,top.discard.card)
    }
    
    if (success == F) {
      picked.up.card = discard.or.stock(p2.hand)
      p2.hand = picked.up.card$v1
      p2.hand$name = p2.hand$name[1]
      top.discard = picked.up.card$v2
      stock.pile = picked.up.card$v3
      discard.pile = picked.up.card$v4
    } else {
      top.stock = stock.pile[1, ]
      p2.hand = rbind(p2.hand,top.stock)
      stock.pile = anti_join(stock.pile, top.stock, by="order")
    }
    
    if (p2.total.threes >= 2) {
      tack.on = T
    } else {
      tack.on = F
    }
    p2.gameplay = finding.threes(p2.hand,p2.total.threes,total.threes,tack.on)
    p2.hand = p2.gameplay$v1
    p2.total.threes = p2.gameplay$v2
    top.discard = p2.gameplay$v3
    total.threes = p2.gameplay$v4
    won = p2.gameplay$v5
    partial.sets = p2.gameplay$v6
    p2.tacked = p2.tacked + p2.gameplay$v7
    partial.runs = data.frame()
    
    discard_result = discard_card(p2.hand, partial.sets, partial.runs, "sets")
    p2.hand = discard_result$v1
    top.discard = discard_result$v2
    discard.pile = discard_result$v3
    
    
    if (length(p2.hand$faces) == 0) {
      print("Player 2 wins this round")
      break
    }

    if ((p4.total.threes == 0) & (p1.total.threes == 0)) {
      call = calling("set",p4.hand,p1.hand,p4.calls,p1.calls)
      p4.hand = call$v1
      p1.hand = call$v2
      success = call$v3
      discard.pile = call$v4
      top.discard = call$v5
      stock.pile = call$v6
      p4.calls = call$v7
      p1.calls = call$v8
    } else if ((p4.total.threes == 0) & (p1.total.threes > 0)) {
      dummy.hand = data.frame()
      call = calling("set",p4.hand,dummy.hand,p4.calls,0)
      p4.hand = call$v1
      success = call$v3
      discard.pile = call$v4
      top.discard = call$v5
      stock.pile = call$v6
      p4.calls = call$v7
    } else if ((p4.total.threes > 0) & (p1.total.threes == 0)) {
      dummy.hand = data.frame()
      call = calling("set",dummy.hand,p1.hand,0,p1.calls)
      p1.hand = call$v2
      success = call$v3
      discard.pile = call$v4
      top.discard = call$v5
      stock.pile = call$v6
      p1.calls = call$v8
    }
    
    
    cat("------------it is player's 3 turn--------", "\n")
    
    if (length(stock.pile$order) == 0) {
      stock.pile = discard.pile[sample(nrow(discard.pile), nrow(discard.pile), replace = FALSE), ]
      top.discard.card = stock.pile[1,] #assigns the top value of the remaining cards
      discard.pile = data.frame()
      discard.pile = rbind(discard.pile,top.discard.card)
    }
    
    if (success == F) {
      picked.up.card = discard.or.stock(p3.hand)
      p3.hand = picked.up.card$v1
      p3.hand$name = p3.hand$name[1]
      top.discard = picked.up.card$v2
      stock.pile = picked.up.card$v3
      discard.pile = picked.up.card$v4
    } else {
      top.stock = stock.pile[1, ]
      p3.hand = rbind(p3.hand,top.stock)
      stock.pile = anti_join(stock.pile, top.stock, by="order")
    }
    
    if (p3.total.threes >= 2) {
      tack.on = T
    } else {
      tack.on = F
    }
    p3.gameplay = finding.threes(p3.hand,p3.total.threes,total.threes,tack.on)
    p3.hand = p3.gameplay$v1
    p3.total.threes = p3.gameplay$v2
    top.discard = p3.gameplay$v3
    total.threes = p3.gameplay$v4
    won = p3.gameplay$v5
    partial.sets = p3.gameplay$v6
    p3.tacked = p3.tacked + p3.gameplay$v7
    partial.runs = data.frame()
    
    discard_result = discard_card(p3.hand, partial.sets, partial.runs, "sets")
    p3.hand = discard_result$v1
    top.discard = discard_result$v2
    discard.pile = discard_result$v3
    
    
    if (length(p3.hand$faces) == 0) {
      print("Player 3 wins this round")
      break
    }

    if ((p1.total.threes == 0) & (p2.total.threes == 0)) {
      call = calling("set",p1.hand,p2.hand,p1.calls,p2.calls)
      p1.hand = call$v1
      p2.hand = call$v2
      success = call$v3
      discard.pile = call$v4
      top.discard = call$v5
      stock.pile = call$v6
      p1.calls = call$v7
      p2.calls = call$v8
    } else if ((p1.total.threes == 0) & (p2.total.threes > 0)) {
      dummy.hand = data.frame()
      call = calling("set",p1.hand,dummy.hand,p1.calls,0)
      p1.hand = call$v1
      success = call$v3
      discard.pile = call$v4
      top.discard = call$v5
      stock.pile = call$v6
      p1.calls = call$v7
    } else if ((p1.total.threes > 0) & (p2.total.threes == 0)) {
      dummy.hand = data.frame()
      call = calling("set",dummy.hand,p2.hand,0,p2.calls)
      p2.hand = call$v2
      success = call$v3
      discard.pile = call$v4
      top.discard = call$v5
      stock.pile = call$v6
      p2.calls = call$v8
    }
    
    cat("------------it is player's 4 turn--------", "\n")
    
    if (length(stock.pile$order) == 0) {
      stock.pile = discard.pile[sample(nrow(discard.pile), nrow(discard.pile), replace = FALSE), ]
      top.discard.card = stock.pile[1,] #assigns the top value of the remaining cards
      discard.pile = data.frame()
      discard.pile = rbind(discard.pile,top.discard.card)
    }
    
    if (success == F) {
      picked.up.card = discard.or.stock(p4.hand)
      p4.hand = picked.up.card$v1
      p4.hand$name = p4.hand$name[1]
      top.discard = picked.up.card$v2
      stock.pile = picked.up.card$v3
      discard.pile = picked.up.card$v4
    } else {
      top.stock = stock.pile[1, ]
      p4.hand = rbind(p4.hand,top.stock)
      stock.pile = anti_join(stock.pile, top.stock, by="order")
    }
    
    if (p4.total.threes >= 2) {
      tack.on = T
    } else {
      tack.on = F
    }
    p4.gameplay = finding.threes(p4.hand,p4.total.threes,total.threes,tack.on)
    p4.hand = p4.gameplay$v1
    p4.total.threes = p4.gameplay$v2
    top.discard = p4.gameplay$v3
    total.threes = p4.gameplay$v4
    won = p4.gameplay$v5
    partial.sets = p4.gameplay$v6
    p4.tacked = p4.tacked + p4.gameplay$v7
    partial.runs = data.frame()
    
    discard_result = discard_card(p4.hand, partial.sets, partial.runs, "sets")
    p4.hand = discard_result$v1
    top.discard = discard_result$v2
    discard.pile = discard_result$v3
    
    
    if (length(p4.hand$faces) == 0) {
      print("Player 4 wins this round")
      break
    }
  }
  cat("this game ended in the",subround,"th round \n")
  subround.total = subround.total + subround
  
  all.scores = tally.score(p1.hand,p1.score,p2.hand,p2.score,p3.hand,p3.score,p4.hand,p4.score)
  p1.score = all.scores$v1
  p2.score = all.scores$v2
  p3.score = all.scores$v3
  p4.score = all.scores$v4
  
  tot1 = length(discard.pile$faces) +length(p1.hand$faces) +length(p2.hand$faces) +length(p3.hand$faces)
  tot2 = length(p4.hand$faces) +length(stock.pile$faces) + length(total.threes$faces)
  tot1 + tot2
  
  #--------------------------------------------------------------------------------------------#
  
  #Round 2: One three, One four (34)
  
  subround = 0
  p1.total.threes = 0
  p2.total.threes = 0
  p3.total.threes = 0
  p4.total.threes = 0
  
  p1.total.fours = 0
  p2.total.fours = 0
  p3.total.fours = 0
  p4.total.fours = 0
  
  total.runs = data.frame()
  total.sets = data.frame()
  total.laid.down.cards = data.frame()
  won = F
  
  init.cards = setup(12, deck)
  p1.hand = init.cards$v1
  p2.hand = init.cards$v2
  p3.hand = init.cards$v3
  p4.hand = init.cards$v4
  top.discard = init.cards$v5
  stock.pile = init.cards$v6
  discard.pile = init.cards$v7
  
  players = player.order(p1.hand,p2.hand,p3.hand,p4.hand)
  p1.hand = players$v1
  p2.hand = players$v2
  p3.hand = players$v3
  p4.hand = players$v4
  
  success = F
  
  add.to.order = 0
  
  while (won == F) {
    tack.on = F 
    subround = subround + 1
    if (((p2.total.threes + p2.total.fours) == 0) & ((p3.total.threes + p3.total.fours) == 0)) {
      call = calling("both",p2.hand,p3.hand,p2.calls,p3.calls)
      p2.hand = call$v1
      p3.hand = call$v2
      success = call$v3
      discard.pile = call$v4
      top.discard = call$v5
      stock.pile = call$v6
      p2.calls = call$v7
      p3.calls = call$v8
    } else if (((p2.total.threes + p2.total.fours) == 0) & ((p3.total.threes + p3.total.fours) > 0)) {
      dummy.hand = data.frame()
      call = calling("both",p2.hand,dummy.hand,p2.calls,0)
      p2.hand = call$v1
      success = call$v3
      discard.pile = call$v4
      top.discard = call$v5
      stock.pile = call$v6
      p2.calls = call$v7
    } else if (((p2.total.threes + p2.total.fours) > 0) & ((p3.total.threes + p3.total.fours) == 0)) {
      dummy.hand = data.frame()
      call = calling("both",dummy.hand,p3.hand,0,p3.calls)
      p3.hand = call$v2
      success = call$v3
      discard.pile = call$v4
      top.discard = call$v5
      stock.pile = call$v6
      p3.calls = call$v8
    }
    
    cat("------------it is player's 1 turn--------", "\n")
    
    if (length(stock.pile$order) == 0) {
      stock.pile = discard.pile[sample(nrow(discard.pile), nrow(discard.pile), replace = FALSE), ]
      top.discard.card = stock.pile[1,] #assigns the top value of the remaining cards
      discard.pile = data.frame()
      discard.pile = rbind(discard.pile,top.discard.card)
    }
    
    if (success == F) {
      picked.up.card = discard.or.stock(p1.hand)
      p1.hand = picked.up.card$v1
      p1.hand$name = p1.hand$name[1]
      top.discard = picked.up.card$v2
      stock.pile = picked.up.card$v3
      discard.pile = picked.up.card$v4
    } else {
      top.stock = stock.pile[1, ]
      p1.hand = rbind(p1.hand,top.stock)
      stock.pile = anti_join(stock.pile, top.stock, by="order")
    }
    
    if ((p1.total.threes >= 1) & (p1.total.fours == 0)) {
      p1.gameplay.fours = finding.fours(p1.hand,total.runs,p1.total.fours,F)
      p1.hand = p1.gameplay.fours$v1
      p1.total.fours = p1.gameplay.fours$v2
      top.discard = p1.gameplay.fours$v3
      total.runs = p1.gameplay.fours$v4
      won = p1.gameplay.fours$v5
      partial.runs = p1.gameplay.fours$v6
      add.to.order = p1.gameplay.fours$v7
      p1.tacked = p1.tacked + p1.gameplay.fours$v8
      partial.sets = data.frame()
      option = "runs"
    } else if ((p1.total.threes == 0) & (p1.total.fours >= 1)) {
      p1.gameplay.threes = finding.threes(p1.hand,p1.total.threes,total.sets,F)
      p1.hand = p1.gameplay.threes$v1
      p1.total.threes = p1.gameplay.threes$v2
      top.discard = p1.gameplay.threes$v3
      total.sets = p1.gameplay.threes$v4 
      won = p1.gameplay.threes$v5
      partial.sets = p1.gameplay.threes$v6
      p1.tacked = p1.tacked + p1.gameplay.threes$v7
      
      partial.runs = data.frame()
      option = "sets"
    } else if ((p1.total.threes == 0) & (p1.total.fours == 0))  {
      p1.gameplay.fours = finding.fours(p1.hand,total.runs,p1.total.fours,F)
      p1.hand = p1.gameplay.fours$v1
      p1.total.fours = p1.gameplay.fours$v2
      top.discard = p1.gameplay.fours$v3
      total.runs = p1.gameplay.fours$v4
      won = p1.gameplay.fours$v5
      partial.runs = p1.gameplay.fours$v6
      add.to.order = p1.gameplay.fours$v7
      p1.tacked = p1.tacked + p1.gameplay.fours$v8
      
      p1.gameplay.threes = finding.threes(p1.hand,p1.total.threes,total.sets,F,all=F)
      p1.hand = p1.gameplay.threes$v1
      p1.total.threes = p1.gameplay.threes$v2
      top.discard = p1.gameplay.threes$v3
      total.sets = p1.gameplay.threes$v4 
      won = p1.gameplay.threes$v5
      partial.sets = p1.gameplay.threes$v6
      p1.tacked = p1.tacked + p1.gameplay.threes$v7
      option = "both"
    } else {
      p1.gameplay.fours = finding.fours(p1.hand,total.runs,p1.total.fours,T)
      p1.hand = p1.gameplay.fours$v1
      p1.total.fours = p1.gameplay.fours$v2
      top.discard = p1.gameplay.fours$v3
      total.runs = p1.gameplay.fours$v4
      won = p1.gameplay.fours$v5
      partial.runs = p1.gameplay.fours$v6
      add.to.order = p1.gameplay.fours$v7
      p1.tacked = p1.tacked + p1.gameplay.fours$v8
      
      p1.gameplay.threes = finding.threes(p1.hand,p1.total.threes,total.sets,T)
      p1.hand = p1.gameplay.threes$v1
      p1.total.threes = p1.gameplay.threes$v2
      top.discard = p1.gameplay.threes$v3
      total.sets = p1.gameplay.threes$v4 
      won = p1.gameplay.threes$v5
      partial.sets = p1.gameplay.threes$v6
      p1.tacked = p1.tacked + p1.gameplay.threes$v7
      option = "both"
    }
    
    discard_result = discard_card(p1.hand, partial.sets, partial.runs, option)
    p1.hand = discard_result$v1
    top.discard = discard_result$v2
    discard.pile = discard_result$v3
    
    if (length(p1.hand$faces) == 0) {
      print("Player 1 wins this round")
      break
    }
    tot1 = length(discard.pile$faces) +length(p1.hand$faces) +length(p2.hand$faces) +length(p3.hand$faces)
    tot2 = length(p4.hand$faces) +length(stock.pile$faces) + length(total.runs$faces) + length(total.sets$faces)
    print(tot1 + tot2)
    
    if (((p3.total.threes + p3.total.fours) == 0) & ((p4.total.threes + p4.total.fours) == 0)) {
      call = calling("both",p3.hand,p4.hand,p3.calls,p4.calls)
      p3.hand = call$v1
      p4.hand = call$v2
      success = call$v3
      discard.pile = call$v4
      top.discard = call$v5
      stock.pile = call$v6
      p3.calls = call$v7
      p4.calls = call$v8
    } else if (((p3.total.threes + p3.total.fours) == 0) & ((p4.total.threes + p4.total.fours) > 0)) {
      dummy.hand = data.frame()
      call = calling("both",p3.hand,dummy.hand,p3.calls,0)
      p3.hand = call$v1
      success = call$v3
      discard.pile = call$v4
      top.discard = call$v5
      stock.pile = call$v6
      p3.calls = call$v7
    } else if (((p3.total.threes + p3.total.fours) > 0) & ((p4.total.threes + p4.total.fours) == 0)) {
      dummy.hand = data.frame()
      call = calling("both",dummy.hand,p4.hand,0,p4.calls)
      p4.hand = call$v2
      success = call$v3
      discard.pile = call$v4
      top.discard = call$v5
      stock.pile = call$v6
      p4.calls = call$v8
    }
    
    
    cat("------------it is player's 2 turn--------", "\n")
    
    if (length(stock.pile$order) == 0) {
      stock.pile = discard.pile[sample(nrow(discard.pile), nrow(discard.pile), replace = FALSE), ]
      top.discard.card = stock.pile[1,] #assigns the top value of the remaining cards
      discard.pile = data.frame()
      discard.pile = rbind(discard.pile,top.discard.card)
    }
    
    if (success == F) {
      picked.up.card = discard.or.stock(p2.hand)
      p2.hand = picked.up.card$v1
      p2.hand$name = p2.hand$name[1]
      top.discard = picked.up.card$v2
      stock.pile = picked.up.card$v3
      discard.pile = picked.up.card$v4
    } else {
      top.stock = stock.pile[1, ]
      p2.hand = rbind(p2.hand,top.stock)
      stock.pile = anti_join(stock.pile, top.stock, by="order")
    }
    
    if ((p2.total.threes >= 1) & (p2.total.fours == 0)) {
      p2.gameplay.fours = finding.fours(p2.hand,total.runs,p2.total.fours,F)
      p2.hand = p2.gameplay.fours$v1
      p2.total.fours = p2.gameplay.fours$v2
      top.discard = p2.gameplay.fours$v3
      total.runs = p2.gameplay.fours$v4
      won = p2.gameplay.fours$v5
      partial.runs = p2.gameplay.fours$v6
      add.to.order = p2.gameplay.fours$v7
      p2.tacked = p2.gameplay.fours$v8
      partial.sets = data.frame()
      option = "runs"
    } else if ((p2.total.threes == 0) & (p2.total.fours >= 1)) {
      p2.gameplay.threes = finding.threes(p2.hand,p2.total.threes,total.sets,F)
      p2.hand = p2.gameplay.threes$v1
      p2.total.threes = p2.gameplay.threes$v2
      top.discard = p2.gameplay.threes$v3
      total.sets = p2.gameplay.threes$v4 
      won = p2.gameplay.threes$v5
      partial.sets = p2.gameplay.threes$v6
      p2.tacked = p2.tacked + p2.gameplay.threes$v7
      partial.runs = data.frame()
      option = "sets"
    } else if ((p2.total.threes == 0) & (p2.total.fours == 0)) {
      p2.gameplay.fours = finding.fours(p2.hand,total.runs,p2.total.fours,F)
      p2.hand = p2.gameplay.fours$v1
      p2.total.fours = p2.gameplay.fours$v2
      top.discard = p2.gameplay.fours$v3
      total.runs = p2.gameplay.fours$v4
      won = p2.gameplay.fours$v5
      partial.runs = p2.gameplay.fours$v6
      add.to.order = p2.gameplay.fours$v7
      p2.tacked = p2.gameplay.fours$v8
      
      p2.gameplay.threes = finding.threes(p2.hand,p2.total.threes,total.sets,F, all = F)
      p2.hand = p2.gameplay.threes$v1
      p2.total.threes = p2.gameplay.threes$v2
      top.discard = p2.gameplay.threes$v3
      total.sets = p2.gameplay.threes$v4 
      won = p2.gameplay.threes$v5
      partial.sets = p2.gameplay.threes$v6
      p2.tacked = p2.tacked + p2.gameplay.threes$v7
      option = "both"
    } else {
      p2.gameplay.fours = finding.fours(p2.hand,total.runs,p2.total.fours,T)
      p2.hand = p2.gameplay.fours$v1
      p2.total.fours = p2.gameplay.fours$v2
      top.discard = p2.gameplay.fours$v3
      total.runs = p2.gameplay.fours$v4
      won = p2.gameplay.fours$v5
      partial.runs = p2.gameplay.fours$v6
      add.to.order = p2.gameplay.fours$v7
      p2.tacked = p2.gameplay.fours$v8
      
      p2.gameplay.threes = finding.threes(p2.hand,p2.total.threes,total.sets,T)
      p2.hand = p2.gameplay.threes$v1
      p2.total.threes = p2.gameplay.threes$v2
      top.discard = p2.gameplay.threes$v3
      total.sets = p2.gameplay.threes$v4 
      won = p2.gameplay.threes$v5
      partial.sets = p2.gameplay.threes$v6
      p2.tacked = p2.tacked + p2.gameplay.threes$v7
      option = "both"
    }
    
    discard_result = discard_card(p2.hand, partial.sets, partial.runs, option)
    p2.hand = discard_result$v1
    top.discard = discard_result$v2
    discard.pile = discard_result$v3
    
    
    if (length(p2.hand$faces) == 0) {
      print("Player 2 wins this round")
      break
    }
    tot1 = length(discard.pile$faces) +length(p1.hand$faces) +length(p2.hand$faces) +length(p3.hand$faces)
    tot2 = length(p4.hand$faces) +length(stock.pile$faces) + length(total.runs$faces) + length(total.sets$faces)
    print(tot1 + tot2)
    
    if (((p4.total.threes + p4.total.fours) == 0) & ((p1.total.threes + p1.total.fours) == 0)) {
      call = calling("both",p4.hand,p1.hand,p4.calls,p1.calls)
      p4.hand = call$v1
      p1.hand = call$v2
      success = call$v3
      discard.pile = call$v4
      top.discard = call$v5
      stock.pile = call$v6
      p4.calls = call$v7
      p1.calls = call$v8
    } else if (((p4.total.threes + p4.total.fours) == 0) & ((p1.total.threes + p1.total.fours) > 0)) {
      dummy.hand = data.frame()
      call = calling("both",p4.hand,dummy.hand,p4.calls,0)
      p4.hand = call$v1
      success = call$v3
      discard.pile = call$v4
      top.discard = call$v5
      stock.pile = call$v6
      p4.calls = call$v7
    } else if (((p4.total.threes + p4.total.fours) > 0) & ((p1.total.threes + p1.total.fours) == 0)) {
      dummy.hand = data.frame()
      call = calling("both",dummy.hand,p1.hand,0,p1.calls)
      p1.hand = call$v2
      success = call$v3
      discard.pile = call$v4
      top.discard = call$v5
      stock.pile = call$v6
      p1.calls = call$v8
    }
    
    cat("------------it is player's 3 turn--------", "\n")
    
    if (length(stock.pile$order) == 0) {
      stock.pile = discard.pile[sample(nrow(discard.pile), nrow(discard.pile), replace = FALSE), ]
      top.discard.card = stock.pile[1,] #assigns the top value of the remaining cards
      discard.pile = data.frame()
      discard.pile = rbind(discard.pile,top.discard.card)
    }
    
    if (success == F) {
      picked.up.card = discard.or.stock(p3.hand)
      p3.hand = picked.up.card$v1
      p3.hand$name = p3.hand$name[1]
      top.discard = picked.up.card$v2
      stock.pile = picked.up.card$v3
      discard.pile = picked.up.card$v4
    } else {
      top.stock = stock.pile[1, ]
      p3.hand = rbind(p3.hand,top.stock)
      stock.pile = anti_join(stock.pile, top.stock, by="order")
    }
    
    if ((p3.total.threes >= 1) & (p3.total.fours == 0)) {
      p3.gameplay.fours = finding.fours(p3.hand,total.runs,p3.total.fours,F)
      p3.hand = p3.gameplay.fours$v1
      p3.total.fours = p3.gameplay.fours$v2
      top.discard = p3.gameplay.fours$v3
      total.runs = p3.gameplay.fours$v4
      won = p3.gameplay.fours$v5
      partial.runs = p3.gameplay.fours$v6
      add.to.order = p3.gameplay.fours$v7
      p3.tacked = p3.tacked + p3.gameplay.fours$v8
      partial.sets = data.frame()
      option = "runs"
    } else if ((p3.total.threes == 0) & (p3.total.fours >= 0)) {
      p3.gameplay.threes = finding.threes(p3.hand,p3.total.threes,total.sets,F)
      p3.hand = p3.gameplay.threes$v1
      p3.total.threes = p3.gameplay.threes$v2
      top.discard = p3.gameplay.threes$v3
      total.sets = p3.gameplay.threes$v4 
      won = p3.gameplay.threes$v5
      partial.sets = p3.gameplay.threes$v6
      p3.tacked = p3.tacked + p3.gameplay.threes$v7
      partial.runs = data.frame()
      option = "sets"
    } else if ((p3.total.threes == 0) & (p3.total.fours == 0)) {
      p3.gameplay.fours = finding.fours(p3.hand,total.runs,p3.total.fours,F)
      p3.hand = p3.gameplay.fours$v1
      p3.total.fours = p3.gameplay.fours$v2
      top.discard = p3.gameplay.fours$v3
      total.runs = p3.gameplay.fours$v4
      won = p3.gameplay.fours$v5
      partial.runs = p3.gameplay.fours$v6
      add.to.order = p3.gameplay.fours$v7
      p3.tacked = p3.tacked + p3.gameplay.fours$v8
      
      p3.gameplay.threes = finding.threes(p3.hand,p3.total.threes,total.sets,F, all = F)
      p3.hand = p3.gameplay.threes$v1
      p3.total.threes = p3.gameplay.threes$v2
      top.discard = p3.gameplay.threes$v3
      total.sets = p3.gameplay.threes$v4 
      won = p3.gameplay.threes$v5
      partial.sets = p3.gameplay.threes$v6
      p3.tacked = p3.tacked + p3.gameplay.threes$v7
      option = "both"
    } else {
      p3.gameplay.fours = finding.fours(p3.hand,total.runs,p3.total.fours,T)
      p3.hand = p3.gameplay.fours$v1
      p3.total.fours = p3.gameplay.fours$v2
      top.discard = p3.gameplay.fours$v3
      total.runs = p3.gameplay.fours$v4
      won = p3.gameplay.fours$v5
      partial.runs = p3.gameplay.fours$v6
      add.to.order = p3.gameplay.fours$v7
      p3.tacked = p3.tacked + p3.gameplay.fours$v8
      
      p3.gameplay.threes = finding.threes(p3.hand,p3.total.threes,total.sets,T)
      p3.hand = p3.gameplay.threes$v1
      p3.total.threes = p3.gameplay.threes$v2
      top.discard = p3.gameplay.threes$v3
      total.sets = p3.gameplay.threes$v4 
      won = p3.gameplay.threes$v5
      partial.sets = p3.gameplay.threes$v6
      p3.tacked = p3.tacked + p3.gameplay.threes$v7
      option = "both"
    }
    
    discard_result = discard_card(p3.hand, partial.sets, partial.runs, option)
    p3.hand = discard_result$v1
    top.discard = discard_result$v2
    discard.pile = discard_result$v3
    
    if (length(p3.hand$faces) == 0) {
      print("Player 3 wins this round")
      break
    }
    tot1 = length(discard.pile$faces) +length(p1.hand$faces) +length(p2.hand$faces) +length(p3.hand$faces)
    tot2 = length(p4.hand$faces) +length(stock.pile$faces) + length(total.runs$faces) + length(total.sets$faces)
    print(tot1 + tot2)
    
    if (((p1.total.threes + p1.total.fours) == 0) & ((p2.total.threes + p2.total.fours) == 0)) {
      call = calling("both",p1.hand,p2.hand,p1.calls,p2.calls)
      p1.hand = call$v1
      p2.hand = call$v2
      success = call$v3
      discard.pile = call$v4
      top.discard = call$v5
      stock.pile = call$v6
      p1.calls = call$v7
      p2.calls = call$v8
    } else if (((p1.total.threes + p1.total.fours) == 0) & ((p2.total.threes + p2.total.fours) > 0)) {
      dummy.hand = data.frame()
      call = calling("both",p1.hand,dummy.hand,p1.calls,0)
      p1.hand = call$v1
      success = call$v3
      discard.pile = call$v4
      top.discard = call$v5
      stock.pile = call$v6
      p1.calls = call$v7
    } else if (((p1.total.threes + p1.total.fours) > 0) & ((p2.total.threes + p2.total.fours) == 0)) {
      dummy.hand = data.frame()
      call = calling("both",dummy.hand,p2.hand,0,p2.calls)
      p2.hand = call$v2
      success = call$v3
      discard.pile = call$v4
      top.discard = call$v5
      stock.pile = call$v6
      p2.calls = call$v8
    }
    
    cat("------------it is player's 4 turn--------", "\n")
    
    if (length(stock.pile$order) == 0) {
      stock.pile = discard.pile[sample(nrow(discard.pile), nrow(discard.pile), replace = FALSE), ]
      top.discard.card = stock.pile[1,] #assigns the top value of the remaining cards
      discard.pile = data.frame()
      discard.pile = rbind(discard.pile,top.discard.card)
    }
    
    if (success == F) {
      picked.up.card = discard.or.stock(p4.hand)
      p4.hand = picked.up.card$v1
      p4.hand$name = p4.hand$name[1]
      top.discard = picked.up.card$v2
      stock.pile = picked.up.card$v3
      discard.pile = picked.up.card$v4
    } else {
      top.stock = stock.pile[1, ]
      p4.hand = rbind(p4.hand,top.stock)
      stock.pile = anti_join(stock.pile, top.stock, by="order")
    }
    

    if ((p4.total.threes >= 1) & (p4.total.fours == 0)) {
      p4.gameplay.fours = finding.fours(p4.hand,total.runs,p4.total.fours,F)
      p4.hand = p4.gameplay.fours$v1
      p4.total.fours =p4.gameplay.fours$v2
      top.discard = p4.gameplay.fours$v3
      total.runs = p4.gameplay.fours$v4
      won = p4.gameplay.fours$v5
      partial.runs = p4.gameplay.fours$v6
      add.to.order = p4.gameplay.fours$v7
      p4.tacked = p4.gameplay.fours$v8
      partial.sets = data.frame()
      option = "runs"
    } else if ((p4.total.threes == 0) & (p4.total.fours >= 1)) {
      p4.gameplay.threes = finding.threes(p4.hand,p4.total.threes,total.sets,F)
      p4.hand = p4.gameplay.threes$v1
      p4.total.threes = p4.gameplay.threes$v2
      top.discard = p4.gameplay.threes$v3
      total.sets = p4.gameplay.threes$v4 
      won = p4.gameplay.threes$v5
      partial.sets = p4.gameplay.threes$v6
      p4.tacked = p4.tacked + p4.gameplay.threes$v7
      partial.runs = data.frame()
      option = "sets"
    } else if ((p4.total.threes == 0) & (p4.total.fours == 0)) {
      p4.gameplay.fours = finding.fours(p4.hand,total.runs,p4.total.fours,F)
      p4.hand = p4.gameplay.fours$v1
      p4.total.fours = p4.gameplay.fours$v2
      top.discard = p4.gameplay.fours$v3
      total.runs = p4.gameplay.fours$v4
      won = p4.gameplay.fours$v5
      partial.runs = p4.gameplay.fours$v6
      add.to.order = p4.gameplay.fours$v7
      p4.tacked = p4.gameplay.fours$v8
      
      p4.gameplay.threes = finding.threes(p4.hand,p4.total.threes,total.sets,F, all = F)
      p4.hand = p4.gameplay.threes$v1
      p4.total.threes = p4.gameplay.threes$v2
      top.discard = p4.gameplay.threes$v3
      total.sets = p4.gameplay.threes$v4 
      won = p4.gameplay.threes$v5
      partial.sets = p4.gameplay.threes$v6
      p4.tacked = p4.tacked + p4.gameplay.threes$v7
      option = "both"
    } else {
      p4.gameplay.fours = finding.fours(p4.hand,total.runs,p4.total.fours,T)
      p4.hand = p4.gameplay.fours$v1
      p4.total.fours = p4.gameplay.fours$v2
      top.discard = p4.gameplay.fours$v3
      total.runs = p4.gameplay.fours$v4
      won = p4.gameplay.fours$v5
      partial.runs = p4.gameplay.fours$v6
      add.to.order = p4.gameplay.fours$v7
      p4.tacked = p4.gameplay.fours$v8
      
      p4.gameplay.threes = finding.threes(p4.hand,p4.total.threes,total.sets,T)
      p4.hand = p4.gameplay.threes$v1
      p4.total.threes = p4.gameplay.threes$v2
      top.discard = p4.gameplay.threes$v3
      total.sets = p4.gameplay.threes$v4 
      won = p4.gameplay.threes$v5
      partial.sets = p4.gameplay.threes$v6
      p4.tacked = p4.tacked + p4.gameplay.threes$v7
      option = "both"
    }
    
    discard_result = discard_card(p4.hand, partial.sets, partial.runs, option)
    p4.hand = discard_result$v1
    top.discard = discard_result$v2
    discard.pile = discard_result$v3
    
    
    if (length(p4.hand$faces) == 0) {
      print("Player 4 wins this round")
      break
    }
  }
  cat("this game ended in the",subround,"th round \n")
  subround.total = subround.total + subround
  
  all.scores = tally.score(p1.hand,p1.score,p2.hand,p2.score,p3.hand,p3.score,p4.hand,p4.score)
  p1.score = all.scores$v1
  p2.score = all.scores$v2
  p3.score = all.scores$v3
  p4.score = all.scores$v4

  
  #------------------------------------------------------------------------------#
  
  #Round 3: Two 4s
  
  # p1.total.fours = 0
  # p2.total.fours = 0
  # p3.total.fours = 0
  # p4.total.fours = 0
  # total.runs = data.frame()
  # won = F
  # 
  # init.cards = setup(12, deck)
  # p1.hand = init.cards$v1
  # p2.hand = init.cards$v2
  # p3.hand = init.cards$v3
  # p4.hand = init.cards$v4
  # top.discard = init.cards$v5
  # stock.pile = init.cards$v6
  # discard.pile = init.cards$v7
  # 
  # players = player.order(p1.hand,p2.hand,p3.hand,p4.hand)
  # p1.hand = players$v1
  # p2.hand = players$v2
  # p3.hand = players$v3
  # p4.hand = players$v4
  # 
  # p1.calls = 0
  # p2.calls = 0
  # p3.calls = 0
  # p4.calls = 0
  # success = F
  # 
  # add.to.order = 0
  # 
  # subround = 0
  # 
  # while (won == F) {
  #   subround = subround + 1
  #   
  #   if ((p2.total.fours == 0) & (p3.total.fours == 0)) {
  #     call = calling("run",p2.hand,p3.hand,p2.calls,p3.calls)
  #     p2.hand = call$v1
  #     p3.hand = call$v2
  #     success = call$v3
  #     discard.pile = call$v4
  #     top.discard = call$v5
  #     stock.pile = call$v6
  #     p2.calls = call$v7
  #     p3.calls = call$v8
  #   } else if ((p2.total.fours == 0) & (p3.total.fours > 0)) {
  #     dummy.hand = data.frame()
  #     call = calling("run",p2.hand,dummy.hand,p2.calls,0)
  #     p2.hand = call$v1
  #     success = call$v3
  #     discard.pile = call$v4
  #     top.discard = call$v5
  #     stock.pile = call$v6
  #     p2.calls = call$v7
  #   } else if ((p2.total.fours > 0) & (p3.total.fours == 0)) {
  #     dummy.hand = data.frame()
  #     call = calling("run",dummy.hand,p3.hand,0,p3.calls)
  #     p3.hand = call$v2
  #     success = call$v3
  #     discard.pile = call$v4
  #     top.discard = call$v5
  #     stock.pile = call$v6
  #     p3.calls = call$v8
  #   }
  #   
  #   cat("------------it is player's 1 turn--------", "\n")
  #   
  #   if (length(stock.pile$order) == 0) {
  #     stock.pile = discard.pile[sample(nrow(discard.pile), nrow(discard.pile), replace = FALSE), ]
  #     top.discard.card = stock.pile[1,] #assigns the top value of the remaining cards
  #     discard.pile = data.frame()
  #     discard.pile = rbind(discard.pile,top.discard.card)
  #   }
  #   
  #   if (success == F) {
  #     picked.up.card = discard.or.stock(p1.hand)
  #     p1.hand = picked.up.card$v1
  #     p1.hand$name = p1.hand$name[1]
  #     top.discard = picked.up.card$v2
  #     stock.pile = picked.up.card$v3
  #     discard.pile = picked.up.card$v4
  #   } else {
  #     top.stock = stock.pile[1, ]
  #     p1.hand = rbind(p1.hand,top.stock)
  #     stock.pile = anti_join(stock.pile, top.stock, by="order")
  #   }
  # 
  #   if (p1.total.fours >= 2) {
  #     tack.on = T
  #   } else {
  #     tack.on = F
  #   }
  #   p1.gameplay.fours = finding.fours(p1.hand,total.runs,p1.total.fours,tack.on)
  #   p1.hand = p1.gameplay.fours$v1
  #   p1.total.fours = p1.gameplay.fours$v2
  #   top.discard = p1.gameplay.fours$v3
  #   # total.runs = p1.gameplay.fours$v4
  #   # if (length(total.runs$order)>0) {
  #   #   p1.suits = unique(total.runs$suits)
  #   #   p1.suits = p1.suits[!p1.suits == "NA"]
  #   # }
  #   won = p1.gameplay.fours$v5
  #   partial.runs = p1.gameplay.fours$v6
  #   add.to.order = p1.gameplay.fours$v7
  #   p1.tacked = p1.gameplay.fours$v8
  #   partial.sets = data.frame()
  # 
  #   discard_result = discard_card(p1.hand, partial.sets, partial.runs, "runs")
  #   p1.hand = discard_result$v1
  #   top.discard = discard_result$v2
  #   discard.pile = discard_result$v3
  # 
  # 
  #   if (length(p1.hand$faces) == 0) {
  #     print("Player 1 wins this round")
  #     break
  #   }
  #   # tot1 = length(discard.pile$faces) +length(p1.hand$faces) +length(p2.hand$faces) +length(p3.hand$faces)
  #   # tot2 = length(p4.hand$faces) +length(stock.pile$faces) + length(total.laid.down.cards$faces)
  #   # print(tot1 + tot2)
  #   
  #   if ((p3.total.fours == 0) & (p4.total.fours == 0)) {
  #     call = calling("run",p3.hand,p4.hand,p3.calls,p4.calls)
  #     p3.hand = call$v1
  #     p4.hand = call$v2
  #     success = call$v3
  #     discard.pile = call$v4
  #     top.discard = call$v5
  #     stock.pile = call$v6
  #     p3.calls = call$v7
  #     p4.calls = call$v8
  #   } else if ((p3.total.fours == 0) & (p4.total.fours > 0)) {
  #     dummy.hand = data.frame()
  #     call = calling("run",p3.hand,dummy.hand,p3.calls,0)
  #     p3.hand = call$v1
  #     success = call$v3
  #     discard.pile = call$v4
  #     top.discard = call$v5
  #     stock.pile = call$v6
  #     p3.calls = call$v7
  #   } else if ((p3.total.fours > 0) & (p4.total.fours == 0)) {
  #     dummy.hand = data.frame()
  #     call = calling("run",dummy.hand,p4.hand,0,p4.calls)
  #     p4.hand = call$v2
  #     success = call$v3
  #     discard.pile = call$v4
  #     top.discard = call$v5
  #     stock.pile = call$v6
  #     p4.calls = call$v8
  #   }
  # 
  #   cat("------------it is player's 2 turn--------", "\n")
  # 
  #   if (length(stock.pile$order) == 0) {
  #     stock.pile = discard.pile[sample(nrow(discard.pile), nrow(discard.pile), replace = FALSE), ]
  #     top.discard.card = stock.pile[1,] #assigns the top value of the remaining cards
  #     discard.pile = data.frame()
  #     discard.pile = rbind(discard.pile,top.discard.card)
  #   }
  #   
  #   if (success == F) {
  #     picked.up.card = discard.or.stock(p2.hand)
  #     p2.hand = picked.up.card$v1
  #     p2.hand$name = p2.hand$name[1]
  #     top.discard = picked.up.card$v2
  #     stock.pile = picked.up.card$v3
  #     discard.pile = picked.up.card$v4
  #   } else {
  #     top.stock = stock.pile[1, ]
  #     p2.hand = rbind(p2.hand,top.stock)
  #     stock.pile = anti_join(stock.pile, top.stock, by="order")
  #   }
  # 
  #   if (p2.total.fours >= 2) {
  #     tack.on = T
  #   } else {
  #     tack.on = F
  #   }
  #   p2.gameplay.fours = finding.fours(p2.hand,total.runs,p2.total.fours,tack.on)
  #   p2.hand = p2.gameplay.fours$v1
  #   p2.total.fours = p2.gameplay.fours$v2
  #   top.discard = p2.gameplay.fours$v3
  #   # total.runs = p2.gameplay.fours$v4
  #   # if (length(total.runs$order)>0) {
  #   #   p2.suits = unique(total.runs$suits)
  #   #   p2.suits = p2.suits[!p2.suits == "NA"]
  #   # }
  #   won = p2.gameplay.fours$v5
  #   partial.runs = p2.gameplay.fours$v6
  #   add.to.order = p2.gameplay.fours$v7
  #   p2.tacked = p2.gameplay.fours$v8
  #   partial.sets = data.frame()
  #   
  #   discard_result = discard_card(p2.hand, partial.sets, partial.runs, "runs")
  #   p2.hand = discard_result$v1
  #   top.discard = discard_result$v2
  #   discard.pile = discard_result$v3
  # 
  # 
  #   if (length(p2.hand$faces) == 0) {
  #     print("Player 2 wins this round")
  #     break
  #   }
  #   # tot1 = length(discard.pile$faces) +length(p1.hand$faces) +length(p2.hand$faces) +length(p3.hand$faces)
  #   # tot2 = length(p4.hand$faces) +length(stock.pile$faces) + length(total.laid.down.cards$faces)
  #   # print(tot1 + tot2)
  #   
  #   if ((p4.total.fours == 0) & (p1.total.fours == 0)) {
  #     call = calling("run",p4.hand,p1.hand,p4.calls,p1.calls)
  #     p4.hand = call$v1
  #     p1.hand = call$v2
  #     success = call$v3
  #     discard.pile = call$v4
  #     top.discard = call$v5
  #     stock.pile = call$v6
  #     p4.calls = call$v7
  #     p1.calls = call$v8
  #   } else if ((p4.total.fours == 0) & (p1.total.fours > 0)) {
  #     dummy.hand = data.frame()
  #     call = calling("run",p4.hand,dummy.hand,p4.calls,0)
  #     p4.hand = call$v1
  #     success = call$v3
  #     discard.pile = call$v4
  #     top.discard = call$v5
  #     stock.pile = call$v6
  #     p4.calls = call$v7
  #   } else if ((p4.total.fours > 0) & (p1.total.fours == 0)) {
  #     dummy.hand = data.frame()
  #     call = calling("run",dummy.hand,p1.hand,0,p1.calls)
  #     p1.hand = call$v2
  #     success = call$v3
  #     discard.pile = call$v4
  #     top.discard = call$v5
  #     stock.pile = call$v6
  #     p1.calls = call$v8
  #   }
  # 
  #   cat("------------it is player's 3 turn--------", "\n")
  # 
  #   if (length(stock.pile$order) == 0) {
  #     stock.pile = discard.pile[sample(nrow(discard.pile), nrow(discard.pile), replace = FALSE), ]
  #     top.discard.card = stock.pile[1,] #assigns the top value of the remaining cards
  #     discard.pile = data.frame()
  #     discard.pile = rbind(discard.pile,top.discard.card)
  #   }
  #   
  #   if (success == F) {
  #     picked.up.card = discard.or.stock(p3.hand)
  #     p3.hand = picked.up.card$v1
  #     p3.hand$name = p3.hand$name[1]
  #     top.discard = picked.up.card$v2
  #     stock.pile = picked.up.card$v3
  #     discard.pile = picked.up.card$v4
  #   } else {
  #     top.stock = stock.pile[1, ]
  #     p3.hand = rbind(p3.hand,top.stock)
  #     stock.pile = anti_join(stock.pile, top.stock, by="order")
  #   }
  # 
  #   if (p3.total.fours >= 2) {
  #     tack.on = T
  #   } else {
  #     tack.on = F
  #   }
  #   p3.gameplay.fours = finding.fours(p3.hand,total.runs,p3.total.fours,tack.on)
  #   p3.hand = p3.gameplay.fours$v1
  #   p3.total.fours = p3.gameplay.fours$v2
  #   top.discard = p3.gameplay.fours$v3
  #   # total.runs = p3.gameplay.fours$v4
  #   # if (length(total.runs$order)>0) {
  #   #   p3.suits = unique(total.runs$suits)
  #   #   p3.suits = p3.suits[!p3.suits == "NA"]
  #   # }
  #   won = p3.gameplay.fours$v5
  #   partial.runs = p3.gameplay.fours$v6
  #   add.to.order = p3.gameplay.fours$v7
  #   p3.tacked = p3.gameplay.fours$v8
  #   partial.sets = data.frame()
  #   
  #   discard_result = discard_card(p3.hand, partial.sets, partial.runs, "runs")
  #   p3.hand = discard_result$v1
  #   top.discard = discard_result$v2
  #   discard.pile = discard_result$v3
  #   
  # 
  #   if (length(p3.hand$faces) == 0) {
  #     print("Player 3 wins this round")
  #     break
  #   }
  #   # tot1 = length(discard.pile$faces) +length(p1.hand$faces) +length(p2.hand$faces) +length(p3.hand$faces)
  #   # tot2 = length(p4.hand$faces) +length(stock.pile$faces) + length(total.laid.down.cards$faces)
  #   # print(tot1 + tot2)
  #   
  #   if ((p1.total.fours == 0) & (p2.total.fours == 0)) {
  #     call = calling("run",p1.hand,p2.hand,p1.calls,p2.calls)
  #     p1.hand = call$v1
  #     p2.hand = call$v2
  #     success = call$v3
  #     discard.pile = call$v4
  #     top.discard = call$v5
  #     stock.pile = call$v6
  #     p1.calls = call$v7
  #     p2.calls = call$v8
  #   } else if ((p1.total.fours == 0) & (p2.total.fours > 0)) {
  #     dummy.hand = data.frame()
  #     call = calling("run",p1.hand,dummy.hand,p1.calls,0)
  #     p1.hand = call$v1
  #     success = call$v3
  #     discard.pile = call$v4
  #     top.discard = call$v5
  #     stock.pile = call$v6
  #     p1.calls = call$v7
  #   } else if ((p1.total.fours > 0) & (p2.total.fours == 0)) {
  #     dummy.hand = data.frame()
  #     call = calling("run",dummy.hand,p2.hand,0,p2.calls)
  #     p2.hand = call$v2
  #     success = call$v3
  #     discard.pile = call$v4
  #     top.discard = call$v5
  #     stock.pile = call$v6
  #     p2.calls = call$v8
  #   }
  # 
  #   cat("------------it is player's 4 turn--------", "\n")
  # 
  #   if (length(stock.pile$order) == 0) {
  #     stock.pile = discard.pile[sample(nrow(discard.pile), nrow(discard.pile), replace = FALSE), ]
  #     top.discard.card = stock.pile[1,] #assigns the top value of the remaining cards
  #     discard.pile = data.frame()
  #     discard.pile = rbind(discard.pile,top.discard.card)
  #   }
  #   
  #   if (success == F) {
  #     picked.up.card = discard.or.stock(p4.hand)
  #     p4.hand = picked.up.card$v1
  #     p4.hand$name = p4.hand$name[1]
  #     top.discard = picked.up.card$v2
  #     stock.pile = picked.up.card$v3
  #     discard.pile = picked.up.card$v4
  #   } else {
  #     top.stock = stock.pile[1, ]
  #     p4.hand = rbind(p4.hand,top.stock)
  #     stock.pile = anti_join(stock.pile, top.stock, by="order")
  #   }
  # 
  #   if (p4.total.fours >= 2) {
  #     tack.on = T
  #   } else {
  #     tack.on = F
  #   }
  #   p4.gameplay.fours = finding.fours(p4.hand,total.runs,p4.total.fours,tack.on)
  #   p4.hand = p4.gameplay.fours$v1
  #   p4.total.fours = p4.gameplay.fours$v2
  #   top.discard = p4.gameplay.fours$v3
  #   total.runs = p4.gameplay.fours$v4
  #   # if (length(total.runs$order)>0) {
  #   #   p4.suits = unique(total.runs$suits)
  #   #   p4.suits = p4.suits[!p4.suits == "NA"]
  #   # }
  #   won = p4.gameplay.fours$v5
  #   partial.runs = p4.gameplay.fours$v6
  #   add.to.order = p4.gameplay.fours$v7
  #   p4.tacked = p4.gameplay.fours$v8
  #   partial.sets = data.frame()
  #   
  #   discard_result = discard_card(p4.hand, partial.sets, partial.runs, "runs")
  #   p4.hand = discard_result$v1
  #   top.discard = discard_result$v2
  #   discard.pile = discard_result$v3
  #   
  # 
  #   if (length(p4.hand$faces) == 0) {
  #     print("Player 4 wins this round")
  #     break
  #   }
  #   # tot1 = length(discard.pile$faces) +length(p1.hand$faces) +length(p2.hand$faces) +length(p3.hand$faces)
  #   # tot2 = length(p4.hand$faces) +length(stock.pile$faces) + length(total.laid.down.cards$faces)
  #   # print(tot1 + tot2)
  # }
  # cat("this game ended in the",subround,"th round \n")
  # subround.total = subround.total + subround
  
  all.scores = tally.score(p1.hand,p1.score,p2.hand,p2.score,p3.hand,p3.score,p4.hand,p4.score)
  p1.score = all.scores$v1
  p2.score = all.scores$v2
  p3.score = all.scores$v3
  p4.score = all.scores$v4
  
  p1.scores = append(p1.scores,p1.score)
  p2.scores = append(p2.scores,p2.score)
  p3.scores = append(p3.scores,p3.score)
  p4.scores = append(p4.scores,p4.score)
  
  p1.wins = 0
  p2.wins = 0
  p3.wins = 0
  p4.wins = 0
  
  win.with.one.call = 0
  win.with.five.plus.calls = 0
  
  winner.tacked = c()
  max.score.tacked = c()
  
  winner = min(p1.score,p2.score,p3.score,p4.score)
  if (winner == p1.score) {
    print("Player 1 wins the game")
    p1.wins = p1.wins + 1
    if (p1.calls >= 1) {
      win.with.one.call = win.with.one.call + 1
    }
    if (p1.calls >= 5) {
      win.with.five.plus.calls = win.with.five.plus.calls + 1
    }
    winner.tacked = append(winner.tacked,p1.tacked)
  } else if (winner == p2.score) {
    print("Player 2 wins the game")
    p2.wins = p2.wins + 1
    if (p2.calls >= 1) {
      win.with.one.call = win.with.one.call + 1
    }
    if (p2.calls >= 5) {
      win.with.five.plus.calls = win.with.five.plus.calls + 1
    }
    winner.tacked = append(winner.tacked,p2.tacked)
  } else if (winner == p3.score) {
    print("Player 3 wins the game")
    p3.wins = p3.wins + 1
    if (p3.calls >= 1) {
      win.with.one.call = win.with.one.call + 1
    }
    if (p3.calls >= 5) {
      win.with.five.plus.calls = win.with.five.plus.calls + 1
    }
    winner.tacked = append(winner.tacked,p3.tacked)
  } else {
    print("Player 4 wins the game")
    p4.wins = p4.wins + 1
    if (p4.calls >= 1) {
      win.with.one.call = win.with.one.call + 1
    }
    if (p4.calls >= 5) {
      win.with.five.plus.calls = win.with.five.plus.calls + 1
    }
    winner.tacked = append(winner.tacked,p4.tacked)
  }
  
  max.score = max(p1.score,p2.score,p3.score,p4.score)
  if (max.score == p1.score) {
    max.score.tacked = append(max.score.tacked,p1.tacked)
  } else if (max.score == p2.score) {
    max.score.tacked = append(max.score.tacked,p2.hand)
  } else if (max.score == p3.score) {
    max.score.tacked = append(max.score.tacked, p3.hand)
  } else {
    max.score.tacked = append(max.score.tacked, p4.hand)
  }
  

  total.rounds = append(total.rounds,subround.total)
}


#-------------------------------------------------------------------------------#

# Game Analysis

# Histogram of num subrounds for each game
hist(total.rounds)

# Average number of subrounds it takes to complete a game
mean(total.rounds)


# Proportion of players who had at least one successful call that went on to win the game
print(win.with.one.call / 100)


# Average number of tacked on cards for player who won the game 
mean(winner.tacked)

p1.winner = 0
p2.winner = 0
p3.winner = 0
p4.winner = 0



p1.rounds = c()
p1.rounds[1] = p1.scores[1]
for (i in 2:50) {
  p1.rounds[i] = p1.scores[i] - p1.scores[i-1]
}

p2.rounds = c() 
p2.rounds[1] = p2.scores[1]
for (i in 2:50) {
  p2.rounds[i] = p2.scores[i] - p2.scores[i-1]
}

p3.rounds = c() 
p3.rounds[1] = p3.scores[1]
for (i in 2:50) {
  p3.rounds[i] = p3.scores[i] - p3.scores[i-1]
}

p4.rounds = c() 
p4.rounds[1] = p4.scores[1]
for (i in 2:50) {
  p4.rounds[i] = p4.scores[i] - p4.scores[i-1]
}

for (i in 1:50) {
  if (p1.rounds[i] == min(p1.rounds[i],p2.rounds[i], p3.rounds[i], p4.rounds[i])) {
    p1.winner = p1.winner + 1
  } else if (p2.rounds[i] == min(p1.rounds[i],p2.rounds[i], p3.rounds[i], p4.rounds[i])) {
    p2.winner = p2.winner + 1
  } else if (p3.rounds[i] == min(p1.rounds[i],p2.rounds[i], p3.rounds[i], p4.rounds[i])) {
    p3.winner = p3.winner + 1
  } else {
    p4.winner = p4.winner + 1
  }
}

#Probability that each player wins the game
print(p1.winner/50)
print(p2.winner/50)
print(p3.winner/50)
print(p4.winner/50)

# Histogram of each player's score distribution
hist(p1.rounds)
hist(p2.rounds)
hist(p3.rounds)
hist(p4.rounds)


avg.p1 = mean(p1.rounds)
min.p1 = min(p1.rounds)
max.p1 = max(p1.rounds)

avg.p2 = mean(p2.rounds)
min.p2 = min(p2.rounds)
max.p2 = max(p2.rounds)

avg.p3 = mean(p3.rounds)
min.p3 = min(p3.rounds)
max.p3 = max(p3.rounds)

avg.p4 = mean(p4.rounds)
min.p4 = min(p4.rounds)
max.p4 = max(p4.rounds)

cat("The average score for Player 1 was ", avg.p1, ", the minimum score was ", min.p1, " and the maximum score was ", max.p1, "\n
The average score for Player 2 was ", avg.p2, ", the minimum score was ", min.p2, " and the maximum score was ", max.p2, "\n
The average score for Player 3 was ", avg.p3, ", the minimum score was ", min.p3, " and the maximum score was ", max.p3, "\n
The average score for Player 4 was ", avg.p4, ", the minimum score was ", min.p4, " and the maximum score was ", max.p4, "\n")

