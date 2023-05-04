suits <- c("Clubs", "Diamonds", "Hearts", "Spades")
ranks <- c(2:10, "Jack", "Queen", "King", "Ace")
deck <- expand.grid(Suit = suits, Rank = ranks)
deck <- rbind(deck, deck) # Add a second copy of the deck
jokers <- data.frame(Suit = c("Joker", "Joker"), Rank = c("Joker", "Joker"))
deck <- rbind(deck, jokers) # Add the jokers
