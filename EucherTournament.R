# Eucher tournametn

players <- c("Nick", "Dan", "Ellie", "Samantha", "Nancy", "Bert", "Katie")

lenp <- length(players)

records <- array(NA, c(lenp, lenp, lenp, lenp), 
                 dimnames=list(player=players, partner=players,
                               vs1=players, vs2=players))
games <- rep(0, lenp)

enterRecord <- function(winner1, winner2, loser1, loser2, differential)
{
  winner1ind <- which(players==winner1)
  winner2ind <- which(players==winner2)
  loser1ind <- which(players==loser1)
  loser2ind <- which(players==loser2)
  if (length(winner1ind) != 1) stop("Winner1 is wrong")
  if (length(winner2ind) != 1) stop("winner2 is wrong")
  if (length(loser1ind) != 1) stop("loser1 is wrong")
  if (length(loser2ind) != 1) stop("loser2 is wrong")
  records[winner1ind, winner2ind, loser1ind, loser2ind] <<- differential
  records[winner2ind, winner1ind, loser1ind, loser2ind] <<- differential
  records[loser1ind, loser2ind, winner1ind, winner2ind] <<- -1 * differential
  records[loser2ind, loser1ind, winner1ind, winner2ind] <<- -1 * differential
  games[winner1ind] <<- games[winner1ind] + 1
  games[winner2ind] <<- games[winner2ind] + 1
  games[loser1ind] <<- games[loser1ind] + 1
  games[loser2ind] <<- games[loser2ind] + 1
}

#overallScore <- function(player)
#{
#  playerind <- which(players==player)
#  
#}


## Test
enterRecord("Nick", "Dan", "Nancy", "Samantha", 3)
enterRecord("Nancy", "Nick", "Dan", "Samantha", 7)
enterRecord("Nancy", "Dan", "Nick", "Samantha", 7)

apply(records, c(1), sum, na.rm=TRUE)
which(!is.na(records), arr.ind=TRUE)



## real records
records <- array(NA, c(lenp, lenp, lenp, lenp), 
                 dimnames=list(player=players, partner=players,
                               vs1=players, vs2=players))

games <- rep(0, lenp)

enterRecord("Nancy", "Dan", "Nick", "Samantha", 5)
enterRecord("Nancy", "Nick", "Dan", "Samantha", 1)
enterRecord("Nancy", "Samantha", "Nick", "Dan", 2)
enterRecord("Nick", "Bert", "Dan", "Katie", 7)

apply(records, c(1), sum, na.rm=TRUE) / games

records2 <- records
records2[which(records2>0,arr.ind=TRUE)] <- 1
records2[which(records2<0, arr.ind=TRUE)] <- -1

apply(records2, c(1,2), sum, na.rm=TRUE)
