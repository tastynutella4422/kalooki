
p1.hand = data.frame(faces=c("ace","king","jack","four","five","queen","four","four","three","eight","jack","seven"), suits=c("hearts","hearts","hearts","hearts","hearts","hearts","clubs","spades","spades","diamonds","diamonds","hearts"), 
                         order=c(1,2,3,4,5,6,7,8,9,10,11,12), value=c(15,13,11,4,5,12,4,4,3,8,11,7), name=c("Player 1","Player 1","Player 1","Player 1","Player 1","Player 1","Player 1","Player 1","Player 1","Player 1","Player 1","Player 1"))

init.cards = setup(12, deck)
# p1.hand = init.cards$v1
# p2.hand = init.cards$v2
# p3.hand = init.cards$v3
# p4.hand = init.cards$v4
top.discard = init.cards$v5
stock.pile = init.cards$v6
discard.pile = init.cards$v7
tack.on=F

picked.up.card = discard.or.stock(p2.hand)
p2.hand = picked.up.card$v1
p2.hand$name = p2.hand$name[1]
top.discard = picked.up.card$v2
stock.pile = picked.up.card$v3
discard.pile = picked.up.card$v4

p2.gameplay.fours = finding.fours(p2.hand,total.runs,p2.total.fours,tack.on)
p2.hand = p2.gameplay.fours$v1
p2.total.fours = p2.total.fours + p2.gameplay.fours$v2
top.discard = p2.gameplay.fours$v3
total.runs = rbind(total.runs, p2.gameplay.fours$v4)
won = p2.gameplay.fours$v5
partial.runs = p2.gameplay.fours$v6

p2.gameplay.threes = finding.threes(p2.hand,p2.total.threes,total.sets,tack.on)
p2.hand = p2.gameplay.threes$v1
p2.total.threes = p2.gameplay.threes$v2
top.discard = p2.gameplay.threes$v3
total.sets = p2.gameplay.threes$v4 
won = p2.gameplay.threes$v5
partial.sets = p2.gameplay.threes$v6
option = "both"

discard_result = discard_card(p2.hand, partial.sets, partial.runs, option)
p2.hand = discard_result$v1
top.discard = discard_result$v2
discard.pile = discard_result$v3



test.player = data.frame(faces=c("four","two","two","joker","four","three","three","three","four"), 
                         suits=c("spades","hearts","clubs","NA","diamonds","clubs","diamonds","hearts","hearts"), 
                         order=c(1,2,3,4,5,6,7,8,9), value=c(4,2,2,50,4,3,3,3,4), 
                         name=c("Player 1","Player 1","Player 1","Player 1","Player 1","Player 1","Player 1","Player 1","Player 1"))
find.runs(test.player)

top.discard = data.frame(faces=c("two"),suits=c("hearts"),order=c(20),value=c(3),name=c("Player 1"))
calls.1 = 0
calls.2 = 0

calling("set",test.player,p2.hand,calls.1,calls.2)
