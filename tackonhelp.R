faces = c("nine", "joker", "king", "king", "three", "ace", "seven", "four", "five", "four", "two", "ace", "four")
suits = c("hearts", "NA", "hearts", "clubs", "diamonds", "spades", "clubs", "hearts", "hearts", "clubs", "spades", "diamonds", "hearts")
order = c(57, 85, 53, 27, 102, 13, 46, 10, 61, 36, 12, 91, 14)
value = c(9, 50, 13, 13, 3, 15, 7, 4, 5, 4, 2, 1, 4)
name = c("Player 1", "Player 1", "Player 1", "Player 1", "Player 1", "Player 1", "Player 1", "Player 1", "Player 1", "Player 1", "Player 1", "Player 1", "Player 1")

p1.hand = data.frame(faces, suits, order, value, name)

total.runer = data.frame()
add.order = 0

faces = c("five", "six", "seven",  "joker")
suits = c("hearts","hearts","hearts","hearts")
order = c(57, 85 ,53 , 27)
value = c( 5,  6, 7, 8)
name = c("Player 2","Player 2", "Player 2","Player 2")

add.order = add.order + 1
new.cards = data.frame(faces, suits, order, value, name, add.order)

total.runer = rbind(total.runer, new.cards)

faces = c( "nine", "ten", "jack", "queen")
suits = c("diamonds","diamonds","diamonds","diamonds")
order = c(102,  13,  46,  10)
value = c( 9, 10,  11,  12)
name = c("Player 2","Player 2", "Player 2","Player 2")

add.order = add.order + 1

new.cards = data.frame(faces, suits, order, value, name, add.order)

total.runer = rbind(total.runer, new.cards)

tack_on_cards <- function(player, total_runs) {
  can_tack_on <- function(card, run) {
    same_suit <- card$suits == run$suits[1]
    next_higher <- card$value == max(run$value) + 1
    next_lower <- card$value == min(run$value) - 1
    is_joker <- card$faces == "joker"
    adjacent_jokers <- any(diff(run$value[run$faces == "joker"]) == 1)
    
    if (is_joker && !adjacent_jokers) {
      return(TRUE)
    }
    
    if (same_suit) {
      if (next_higher && !any(run$faces == "Ace")) {
        return(TRUE)
      }
      if (next_lower && !any(run$faces == "Ace")) {
        return(TRUE)
      }
    }
    
    joker_in_run = any(run$faces == "joker")
    if (joker_in_run) {
      joker_idx = which(run$faces == "joker")
      joker_as_lower = run$value[joker_idx - 1] + 1
      joker_as_upper = run$value[joker_idx + 1] - 1
      joker_as_higher = run$value[joker_idx - 1] + 2
      
      if ((joker_idx > 1) && (joker_idx < nrow(run))) {
        if (card$value == joker_as_lower || card$value == joker_as_upper) {
          return(TRUE)
        }
      }
      
      if ((card$value == joker_as_higher) && (card$suits == run$suits[1])) {
        print("brother312")
        return(TRUE)
      }
      else {
        print("brother 10")
      }
    }
    return(FALSE)
  }
  
  update_run_with_joker <- function(card, run) {
    real_card_value <- run$value[run$faces == "Joker"]
    run$value[run$faces == "joker"] <- card$value
    run$faces[run$faces == "joker"] <- card$faces
    run$suits[run$faces == card$faces] <- card$suits
    run <- rbind(run, data.frame(faces = "joker", suits = "", order = NA, value = real_card_value, name = card$name, add.order = run$add.order[1]))
    run <- run[order(run$value), ]
    return(run)
  }
  
  i = 1
  while (i <=nrow(player)) {
    card = player[i, ]
    for (j in unique(total_runs$add.order)) {
      cat("i is",i,"\n")
      cat("j is",j,"\n")
      run = total_runs[total_runs$add.order == j, ]
      if (can_tack_on(card, run)) {
        front.joker = run$faces[1]=="joker"
        end.joker = run$faces[length(run$order)]=="joker"
        if (any(run$faces == "joker") && (card$value == max(run$value) + 2 || card$value == min(run$value) - 2)) {
          card$suits = run$suits[1]
          card$value = run$value[1]-1
          #run <- update_run_with_joker(card, run)
          #total_runs[total_runs$add.order == j, ] <- run
          total_runs <- rbind(total_runs, card)
          total_runs <- total_runs[order(total_runs$add.order, total_runs$value), ]
        } else if((card$faces=="joker")&((front.joker) | (end.joker))) {
          print("this is happening elias berhane")
          card$suits = run$suits[1]
          if (!front.joker & !end.joker) {
            card$value = run$value[length(run$order)] + 1
          } else if (front.joker & !end.joker) {
            card$value = run$value[length(run$order)] + 1
          } else {
            card$value = run$value[1]-1
          }
          print(total_runs)
          print(card)
          card$add.order <- j
          total_runs <- rbind(total_runs, card)
          total_runs <- total_runs[order(total_runs$add.order, total_runs$value), ]
          
        }else {
          card$add.order <- j
          total_runs <- rbind(total_runs, card)
          total_runs <- total_runs[order(total_runs$add.order, total_runs$value), ]
        }
        player <- player[-i, ]
        break 
      } else {
        i = i + 1
      }
    }
  }
  
  return(list(v1=player, v2=total_runs))
}

x = tack_on_cards(p1.hand, total.runer)
p1.hand = x$v2
total.runer = x$v2

