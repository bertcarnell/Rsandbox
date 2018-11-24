## PingPongTournament utilities

whichLastNotNA <- function(x)
{
  ind <- which(!is.na(x))
  if (length(ind) == 0)
    return(length(x))
  # else length > 0
    return(ind[length(ind)])
}

#whichLastNotNA(c(1,2,3)) == 3
#whichLastNotNA(c(1,NA,3)) == 3
#whichLastNotNA(c(NA,1,NA)) == 2
#whichLastNotNA(c(1,NA,NA)) == 1

lastNotNA <- function(x)
{
  return(x[whichLastNotNA(x)])
}

#lastNotNA(c(1,2,3)) == 3
#lastNotNA(c(1,NA,3)) == 3
#lastNotNA(c(NA,1,NA)) == 1
#lastNotNA(c(1,NA,NA)) == 1
#lastNotNA(c(5)) == 5

errorRate <- function(x)
{
  return(1-sum(x)/length(x))
}

errorRateProb <- function(x)
{
  y <- ifelse(x>0.5, 1, 0)
  return(1-sum(y, na.rm=TRUE)/length(which(!is.na(y))))
}

NickSystem <- function(player1win, player2loss, player1score, player2score,
                       bestOf, startingScore, k)
{
  #player1win <- X$Player1.Win
  #player2loss <- X$Player2.Loss
  #player1score <- X$Player1.Score
  #player2score <- X$Player2.Score
  #bestOf <- X$Best.of
  #startingScore <- 1000
  #k <- 7
  
  players <- unique(c(player1win, player2loss))
  nplayers <- length(players)
  len <- length(player1win)
  stopifnot(length(player2loss)==len)
  stopifnot(length(player1score)==len)
  stopifnot(length(player2score)==len)
  stopifnot(length(bestOf)==len)
  
  scores <- matrix(NA, nrow=len+1, ncol=nplayers)
  scores[1,] <- startingScore
  
  validation <- numeric(len)
    
  for (i in 1:len)
  {
    indWin <- which(players == player1win[i])
    indLoss <- which(players == player2loss[i])
    startScoreWin <- lastNotNA(scores[1:i,indWin])
    startScoreLoss <- lastNotNA(scores[1:i,indLoss])
    
    E1 <- 1/(1+10^((startScoreLoss-startScoreWin)/400))
    E2 <- 1/(1+10^((startScoreWin-startScoreLoss)/400))
    validation[i] <- ifelse(E1 >= E2, 1, 0)
    
    endScoreWin <- startScoreWin + k*(1-E1)*bestOf[i]
    endScoreLoss <- startScoreLoss + k*(0-E2)*bestOf[i]

    scores[i+1, indWin] <- endScoreWin
    scores[i+1, indLoss] <- endScoreLoss
  }
  chart <- data.frame(players=players, scores=apply(scores, 2, lastNotNA))
  return(list(scores=scores, chart=chart, validation=validation))
}

plotSystem <- function(res, ylim, cols, pchs, Main)
{
  #res <- list(scores=scores, chart=chart)
  #ylim <- c(600,1400)
  #cols <- 1:5
  #pchs <- 1:5

  plot(1:nrow(res$scores), res$scores[,1], ylim=ylim, xlab="Match", ylab="Score", main=Main,
       col=cols[1], pch=pchs[1])
  nc <- ncol(res$scores)
  if (nc >=2)
  {
    for (i in 2:nc)
    {
      points(1:nrow(res$scores), res$scores[,i], col=cols[i], pch=pchs[i])
    }
  }
  legend("bottom", horiz=TRUE, 
         legend=paste(res$chart$players, round(res$chart$scores, 0), sep="-"),
         pch=pchs, col=cols)
}

ChessSystem <- function(player1win, player2loss, player1score, player2score,
                       bestOf, startingScore, k)
{
  players <- unique(c(player1win, player2loss))
  nplayers <- length(players)
  len <- length(player1win)
  stopifnot(length(player2loss)==len)
  stopifnot(length(player1score)==len)
  stopifnot(length(player2score)==len)
  stopifnot(length(bestOf)==len)
  
  scores <- matrix(NA, nrow=len+1, ncol=nplayers)
  scores[1,] <- startingScore
  
  gamesPlayed <- player1score + player2score
  validation <- numeric(len)
  
  for (i in 1:len)
  {
    indWin <- which(players == player1win[i])
    indLoss <- which(players == player2loss[i])
    startScoreWin <- lastNotNA(scores[1:i,indWin])
    startScoreLoss <- lastNotNA(scores[1:i,indLoss])
    
    E1 <- min(1/(1+10^((startScoreLoss-startScoreWin)/400))*gamesPlayed[i], 
              ceiling(bestOf[i]/2))
    E2 <- min(1/(1+10^((startScoreWin-startScoreLoss)/400))*gamesPlayed[i], 
              ceiling(bestOf[i]/2))
    validation[i] <- ifelse(E1 >= E2, 1, 0)
    endScoreWin <- startScoreWin + k*(player1score[i]-E1)
    endScoreLoss <- startScoreLoss + k*(player2score[i]-E2)
    
    scores[i+1, indWin] <- endScoreWin
    scores[i+1, indLoss] <- endScoreLoss
  }
  chart <- data.frame(players=players, scores=apply(scores, 2, lastNotNA))
  return(list(scores=scores, chart=chart, validation=validation))
}

