# Sample player's hand
player <- data.frame(
  faces = c("2", "2", "2", "2", "3", "4", "5", "J", "Q", "K", "4", "9"),
  suit = c("D", "D", "D", "D", "D", "D", "D", "H", "H", "H", "H", "H")
)

# Generate card combinations
card_combinations <- generate_card_combinations(player)

# Check the result
print(card_combinations)



generate_card_combinations <- function(player) {
  n_cards <- length(player$faces)
  card_combinations <- list()
  
  for (i in 2:n_cards) {
    combos <- combn(n_cards, i, simplify = FALSE)
    for (combo in combos) {
      card_combinations <- append(card_combinations, list(player[combo, ]))
    }
  }
  
  return(card_combinations)
}

check_sets_runs = function(player){
  has_set(hand)
}

has_set <- function(hand) {
  # Get the frequency of each face value in the hand
  faces_freq <- table(hand$faces)
  
  # Check if any face value occurs three or more times
  for (i in 1:length(faces_freq)) {
    if (faces_freq[i] >= 3) {
      return(TRUE)
    }
  }
  
  # If no set is found, return FALSE
  return(FALSE)
}



round2_gameplay <- function(player, total_sets, total_runs, total_laid_down_cards, tack_on) {
  
  # Check for sets (threes) and runs (fours) in the player's hand
  sets_runs = check_sets_runs(player)
  
  # If there are no sets or runs in the player's hand, discard the highest value card
  if (length(sets_runs$sets) == 0 && length(sets_runs$runs) == 0) {
    card_to_discard <- discard_highest_value_card(player)
    player <- player[player$order != card_to_discard$order,]
    return(list(player = player, total_sets = total_sets, total_runs = total_runs, total_laid_down_cards = total_laid_down_cards, tack_on = tack_on))
  }
  
  # Check if the player has laid down the required number of sets and runs, then allow them to tack on
  if (total_sets >= 2 && total_runs >= 1) {
    tack_on <- TRUE
  }
  
  # Generate all possible combinations of cards in the player's hand
  card_combinations <- generate_card_combinations(player)
  
  # Iterate through the card combinations
  for (i in 1:length(card_combinations)) {
    combination <- card_combinations[[i]]
    
    # Check if the combination contains a set or a run
    if (check_for_set_run(combination)) {
      
      # Check if the set/run is already laid down
      if (!check_already_laid_down(combination, total_laid_down_cards)) {
        
        # Add the set/run to the total laid down cards
        total_laid_down_cards <- rbind(total_laid_down_cards, combination)
        
        # Update the player's hand
        player <- player[!(player$order %in% combination$order),]
        
        # Update the total sets and runs
        if (is_set(combination)) {
          total_sets <- total_sets + 1
        } else {
          total_runs <- total_runs + 1
        }
        
        # Check if the player has laid down the required number of sets and runs, then allow them to tack on
        if (total_sets >= 2 && total_runs >= 1) {
          tack_on <- TRUE
        }
        
        # Check if the player has laid down the required sets and runs and tacked on
        if (total_sets >= 2 && total_runs >= 1 && tack_on) {
          # Remove any cards that don't belong to a set or run
          player <- remove_unmatched_cards(player, total_laid_down_cards)
          
          # Discard the highest value card
          card_to_discard <- discard_highest_value_card(player)
          player <- player[player$order != card_to_discard$order,]
          
          return(list(player = player, total_sets = total_sets, total_runs = total_runs, total_laid_down_cards = total_laid_down_cards, tack_on = tack_on))
        }
      }
    }
  }
  
  # If the player hasn't laid down the required sets and runs, or can't lay down any sets or runs,
  # discard the highest value card
  card_to_discard <- discard_highest_value_card(player)
  player <- player[player$order != card_to_discard$order,]
  
  return(list(player = player, total_sets = total_sets, total_runs = total_runs, total_laid_down_cards = total_laid_down_cards, tack_on = tack_on))
}


# Sample player's hand
p1.hand <- data.frame(
  faces = c("2", "2", "2", "2", "3", "4", "5", "J", "Q", "K", "4", "9"),
  suit = c("D", "D", "D", "D", "D", "D", "D", "H", "H", "H", "H", "H"),
  order = c(21,31,42,23,87,25,73,52,65,98,24,34),
  value = c(2,2,2,2,3,4,5,11,12,13,4,9)
)



