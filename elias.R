library(purrr)
library(dplyr)

## 2 Decks of Cards (2-3 person game)

total.laid.down.cards = data.frame()
p1.score = 0
p2.score = 0

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

find_sets <- function(player) {
  faces_freq = table(player$faces) # Count the frequency of each face value in the player's hand
  set_of_two = c() # Initialize an empty vector to store sets of two
  set_of_three = c() # Initialize an empty vector to store sets of three or more
  set_cards = data.frame()
  
  # Iterate through the face frequencies
  for (k in 1:length(faces_freq)) {
    # If the face value occurs twice in the player's hand, add it to the set_of_two vector
    if (faces_freq[k] == 2) {
      face_name = names(faces_freq[k])
      set_of_two = append(set_of_two, face_name)
    }
    # If the face value occurs three or more times in the player's hand, add it to the set_of_three vector
    if (faces_freq[k] >= 3) {
      face_name = names(faces_freq[k])
      set_of_three = append(set_of_three, face_name)
      set_cards = rbind(set_cards, player[player$faces == face_name, ])
    }
  }
  
  num_sets = length(set_of_three)
  return(list(set_of_two = set_of_two, num_sets = num_sets, set_cards = set_cards))
}


# This function lays down cards in the player's hand that belong to a set of three or more.
lay_down_cards = function(player, set_of_three, total_laid_down_cards) {
  # Iterate through the sets of three or more
  for (i in 1:length(set_of_three)) {
    dups = set_of_three[i] # Get the current set of three or more
    # Filter the player's hand to find cards belonging to the current set of three or more
    laid_down_cards = filter(player, faces %in% dups)
    # Add the laid down cards to the total_laid_down_cards data frame
    total_laid_down_cards = rbind(total_laid_down_cards, laid_down_cards)
  }
  
  # Remove the laid down cards from the player's hand
  player = anti_join(player, laid_down_cards, by="order")
  
  return(list(player=player, total_laid_down_cards=total_laid_down_cards))
}

# This function discards the highest value card in the player's hand that is not part of a set of two.
discard_card = function(player, set_of_two, top_discard) {
  
  # Iterate through the sets of two
  for (i in 1:length(set_of_two)) {
    partial = set_of_two[i] # Get the current set of two
    # Filter the player's hand to find cards belonging to the current set of two
    partial_sets = filter(player, faces %in% partial)
  }
  
  # Exclude the cards in sets of two from the discard decision
  to_discard = anti_join(player, partial_sets, by="order")
  
  # Find the highest value card in the remaining cards
  player_discard = highest.val.card(to_discard)
  
  # Remove the discarded card from the player's hand
  player = anti_join(player, player_discard, by="order")
  
  # Update the top_discard with the discarded card
  top_discard = player_discard
  
  return(list(player=player, top_discard=top_discard))
}

#function to keep track of the score after each round
tally.score = function(p1.hand,p1.score,p2.hand,p2.score) {
  if ((length(p1.hand$order)) == 0) {
    p2.score = p2.score + (sum(p2.hand$value))
  } else {
    p1.score = p1.score + (sum(p1.hand$value))
  }
  return(list(v1=p1.score,v2=p2.score))
}

three.threes.gameplay = function(player, total.threes, total.laid.down.cards, tack.on) {
  won = F
  if (length(player$order) == 0) {
    won = T
    return(list(v1=player, v2=total.threes, v3=top.discard, v4=total.laid.down.cards, v5=won))
  }
  
  # Call check_sets() function to find sets of two and sets of three or more in the player's hand
  sets = check_sets(player)
  set_of_two = sets$set_of_two
  set_of_three = sets$set_of_three
  
  # Call lay_down_cards() function to lay down sets of three or more and update the player's hand
  if (length(set_of_three) > 0) {
    lay_down_result = lay_down_cards(player, set_of_three, total.laid.down.cards)
    player = lay_down_result$player
    total.laid.down.cards = lay_down_result$total_laid_down_cards
    total.threes = total.threes + length(set_of_three)
  }
  
  if (length(player$order) == 0) {
    won = T
    return(list(v1=player, v2=total.threes, v3=top.discard, v4=total.laid.down.cards, v5=won))
  }
  
  # Call discard_card() function to discard the highest value card not part of a set of two
  discard_result <- discard_card(player, set_of_two, top.discard)
  player <- discard_result$player
  top.discard <- discard_result$top_discard
  
  if (length(player$order) == 0) {
    won = T
    return(list(v1=data.frame(), v2=total.threes, v3=top.discard, v4=total.laid.down.cards, v5=won))
  }
  
  return(list(v1=player, v2=total.threes, v3=top.discard, v4=total.laid.down.cards, v5=won))
}


round2_gameplay <- function(player, total_sets, total_runs, total.laid.down.cards, tack.on) {
  # Check for sets (threes) and runs (fours) in the player's hand
  # Lay down any sets or runs found, and update the total.laid.down.cards
  # Check if the player has laid down the required number of sets and runs, then allow them to tack on
  # Discard a card from the player's hand
  # Return the updated values
}

#this function finds runs 
find_runs = function(player_hand) {
  # Initialize an empty list to store the found runs
  runs = list()
  
  # Sort the player's hand by suit and face value
  sorted_hand = player_hand[order(player_hand$suit, player_hand$face_value), ]
  
  # Loop through each unique suit in the sorted hand
  for (current_suit in unique(sorted_hand$suit)) {
    # Get all cards with the current suit
    suit_cards = sorted_hand[sorted_hand$suit == current_suit, ]
    
    # Initialize variables to keep track of consecutive cards and the current run
    consecutive_cards = 1
    run = list(suit_cards[1, ])
    
    # Loop through the remaining cards in the suit_cards dataframe
    for (i in 2:nrow(suit_cards)) {
      # If the current card's face value is 1 more than the previous card's face value,
      # increment the consecutive_cards counter and add the current card to the run
      if (suit_cards$face_value[i] == suit_cards$face_value[i - 1] + 1) {
        consecutive_cards = consecutive_cards + 1
        run = append(run, list(suit_cards[i, ]))
      } else {
        # If not, reset the consecutive_cards counter and start a new run with the current card
        consecutive_cards = 1
        run = list(suit_cards[i, ])
      }
      
      # If there are at least 4 consecutive cards, add the run to the list of runs
      if (consecutive_cards >= 4) {
        runs = append(runs, list(run))
      }
    }
  }
  
  # Return the list of runs found in the player's hand
  return(runs)
}

check_sets <- function(player) {
  faces_freq <- table(player$faces) # Count the frequency of each face value in the player's hand
  set_of_two <- c() # Initialize an empty vector to store sets of two
  set_of_three <- c() # Initialize an empty vector to store sets of three or more
  
  # Iterate through the face frequencies
  for (k in 1:length(faces_freq)) {
    # If the face value occurs twice in the player's hand and is not already part of a set, add it to the set_of_two vector
    if (faces_freq[k] == 2 && !names(faces_freq[k]) %in% set_of_three) {
      face_name <- names(faces_freq[k])
      set_of_two <- append(set_of_two, face_name)
    }
    # If the face value occurs three or more times in the player's hand and is not already part of a set, add it to the set_of_three vector
    if (faces_freq[k] >= 3 && !names(faces_freq[k]) %in% set_of_two && !names(faces_freq[k]) %in% set_of_three) {
      face_name <- names(faces_freq[k])
      set_of_three <- append(set_of_three, face_name)
    }
  }
  
  # Identify unique sets of three or more cards
  unique_sets <- list()
  for (s in set_of_three) {
    # Check if the set is unique
    if (sum(player$faces == s) >= 3 && !(s %in% unique_sets)) {
      # Add the set to the list of unique sets
      unique_sets <- append(unique_sets, s)
    }
  }
  
  return(list(set_of_two=set_of_two, set_of_three=unique_sets))
}




generate_card_combinations = function(player) {
  c
}


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
    tack.on = T
  } else {
    tack.on = F
  }
  p1.gameplay = three.threes.gameplay(p1.hand,p1.total.threes,total.laid.down.cards,tack.on)
  p1.hand = p1.gameplay$v1
  p1.total.threes = p1.gameplay$v2
  top.discard = p1.gameplay$v3
  total.laid.down.cards = p1.gameplay$v4
  won = p1.gameplay$v5
  
  if (length(p1.hand) == 0) {
    print("Player 1 wins")
    break
  }
  
  picked.up.card = discard.or.stock(p2.hand)
  p2.hand = picked.up.card$v1
  top.discard = picked.up.card$v2
  stock.pile = picked.up.card$v3
  
  if (p2.total.threes >= 3) {
    tack.on = T
  } else {
    tack.on = F
  }
  p2.gameplay = three.threes.gameplay(p2.hand,p2.total.threes,total.laid.down.cards,tack.on)
  p2.hand = p2.gameplay$v1
  p2.total.threes = p2.gameplay$v2
  top.discard = p2.gameplay$v3
  total.laid.down.cards = p2.gameplay$v4
  won = p2.gameplay$v5
  
  if (length(p2.hand) == 0) {
    print("Player 2 wins")
    break
  }
}

all.scores = tally.score(p1.hand,p1.score,p2.hand,p2.score)
p1.score = all.scores$v1
p2.score = all.scores$v2


#Round 2: Two 3s and One 4

p1.total.threes = 0
p2.total.threes = 0
p1.total.fours = 0
p2.total.fours = 0
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


p1.hand <- data.frame(
  faces = c("2", "2", "2", "2", "3", "4", "5", "4", "Q", "Q", "Q", "9"),
  suit = c("D", "D", "D", "D", "D", "D", "D", "H", "H", "H", "H", "H"),
  order = c(21,31,42,23,87,25,73,52,65,98,24,34),
  value = c(2,2,2,2,3,4,5,4,12,12,12,9)
)




#maybe we should make finding a 3 a function so that we can implement in the different rounds
#potentially need function if stock pile runs out - would then need to have actually kept track of discard pile rather than only storing the top card
#metrics to track across rounds