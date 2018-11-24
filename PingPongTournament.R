require(PlayerRatings)
require(RCurl)

connectString <- "https://docs.google.com/spreadsheet/pub?key=0Am6z8RkbEVRCdG9HUWJVNTM3SEpUVWltV2toWGlPdnc&single=true&gid=0&output=csv"
curl <- getCurlHandle()
junk <- curlSetOpt(ssl.verifypeer=FALSE, ssl.verifyhost=FALSE, curl=curl)
connectUrl <- getURL(connectString, curl=curl)
X <- read.csv(textConnection(connectUrl), stringsAsFactors=FALSE)

default <- file.path("S:")
source(file.path(default, "PingPongTournamentUtilities.R"))

playerGraph <- c("Bert","Nick","Dan","Mom","Ellie","Ryan")
nPlayers <- length(playerGraph)
X$Date <- strptime(X$Date, "%m/%d/%Y")

Y <- data.frame(date=1:nrow(X),#date=as.numeric(X$Date), 
           player1=as.character(X$Player1.Win),
           player2=as.character(X$Player2.Loss), 
           matchScore=1, stringsAsFactors=FALSE)

#Y2 <- data.frame(date=as.numeric(X$Date), 
#                 player1=as.character(X$Player1.Win),
#                 player2=as.character(X$Player2.Loss), 
#                 matchScore=(X$Player1.Score-X$Player2.Score)/X$Best.of, 
#                 stringsAsFactors=FALSE)

Dates <- numeric(0)
player1 <- character(0)
player2 <- character(0)
matchScore <- character(0)
count <- 1

for (i in 1:nrow(X))
{
  for (j in 1:X[i,]$Player1.Score)
  {
    #Dates[count] <- as.numeric(X[i,]$Date)
    Dates[count] <- as.numeric(Y[i,]$date)
    player1[count] <- X[i,]$Player1.Win
    player2[count] <- X[i,]$Player2.Loss
    matchScore[count] <- 1
    count <- count + 1
  }
  if (X[i,]$Player1.Score > 0)
  {
    for (j in 1:X[i,]$Player2.Score)
    {
      #Dates[count] <- as.numeric(X[i,]$Date)
      Dates[count] <- as.numeric(Y[i,]$date)
      player1[count] <- X[i,]$Player1.Win
      player2[count] <- X[i,]$Player2.Loss
      matchScore[count] <- 0
      count <- count + 1
    }
  }
}


Y3 <- data.frame(date=Dates, 
                 player1=player1,
                 player2=player2, 
                 matchScore=as.numeric(matchScore), 
                 stringsAsFactors=FALSE)

##################################################################
# Match = 1 point #

elo.rankings <- elo(Y, history=TRUE, gamma=0)
fide.rankings <- fide(Y, history=TRUE)
glicko.rankings <- glicko(Y, history=TRUE, gamma=0)
steph.rankings <- steph(Y, history=TRUE, gamma=0)

errorRateProb(predict(elo.rankings, Y[-(1:30),]))
errorRateProb(predict(fide.rankings,Y[-(1:30),]))
errorRateProb(predict(glicko.rankings, Y[-(1:30),]))
errorRateProb(predict(steph.rankings, Y[-(1:30),]))

windows()
par(mfrow=c(2,2))
par(mar=c(5,4,4,2)-1)
plot(elo.rankings, main="Elo - Match", xlab="", ylab="", players=playerGraph)
legend("bottomleft", legend=playerGraph, col=1:length(playerGraph), 
       lty=1:length(playerGraph), bg="white", ncol=2)
plot(fide.rankings, main="FIDE", xlab="", ylab="", players=playerGraph)
plot(glicko.rankings, main="Glicko", xlab="", ylab="", players=playerGraph)
plot(steph.rankings, main="Stephanson", xlab="", ylab="", players=playerGraph)

######################################
# Match = proportion #

#elo.rankings <- elo(Y2, history=TRUE, gamma=0)
#fide.rankings <- fide(Y2, history=TRUE)
#glicko.rankings <- glicko(Y2, history=TRUE, gamma=0)
#steph.rankings <- steph(Y2, history=TRUE, gamma=0)

#windows()
#par(mfrow=c(2,2))
#par(mar=c(5,4,4,2)-1)
#plot(elo.rankings, main="Elo - Match Ratio", xlab="", ylab="", players=playerGraph)
#legend("bottomleft", legend=playerGraph, col=1:length(playerGraph), 
#       lty=1:length(playerGraph), bg="white", ncol=2)
#plot(fide.rankings, main="FIDE", xlab="", ylab="", players=playerGraph)
#plot(glicko.rankings, main="Glicko", xlab="", ylab="", players=playerGraph)
#plot(steph.rankings, main="Stephanson", xlab="", ylab="", players=playerGraph)

#######

elo.rankings <- elo(Y3, history=TRUE, gamma=0)
fide.rankings <- fide(Y3, history=TRUE)
glicko.rankings <- glicko(Y3, history=TRUE, gamma=0)
steph.rankings <- steph(Y3, history=TRUE, gamma=0)

windows()
par(mfrow=c(2,2))
par(mar=c(5,4,4,2)-1)
plot(elo.rankings, main="Elo - Games", xlab="", ylab="", players=playerGraph)
legend("bottomleft", legend=playerGraph, col=1:length(playerGraph), 
       lty=1:length(playerGraph), bg="white", ncol=2)
plot(fide.rankings, main="FIDE", xlab="", ylab="", players=playerGraph)
plot(glicko.rankings, main="Glicko", xlab="", ylab="", players=playerGraph)
plot(steph.rankings, main="Stephanson", xlab="", ylab="", players=playerGraph)

#############################################################3
# Nick System #

N <- 50
error.rankings <- numeric(N)

for (k in 1:N)
{
  nick.rankings <- NickSystem(X$Player1.Win, X$Player2.Loss,
                              X$Player1.Score, X$Player2.Score,
                              X$Best.of, 1000, k)
  
  error.rankings[k] <- errorRate(nick.rankings$validation[-(1:30)])
  print(paste(k, " ", errorRate(nick.rankings$validation[-(1:30)])))
}

nick.rankings <- NickSystem(X$Player1.Win, X$Player2.Loss,
                            X$Player1.Score, X$Player2.Score,
                            X$Best.of, 1000, which.min(error.rankings))

errorRate(nick.rankings$validation[-(1:30)])


windows(width=7, height=7)
plotSystem(nick.rankings, c(600,1400), 1:nPlayers, 1:nPlayers, "Nick System")

################################################################3
# Chess Rankings #

for (k in 1:N)
{
  chess.rankings <- ChessSystem(X$Player1.Win, X$Player2.Loss,
                                X$Player1.Score, X$Player2.Score,
                                X$Best.of, 1200, k)
  error.rankings[k] <- errorRate(chess.rankings$validation[-(1:30)])
  print(paste(k, " ", errorRate(chess.rankings$validation[-(1:30)])))
}

chess.rankings <- ChessSystem(X$Player1.Win, X$Player2.Loss,
                            X$Player1.Score, X$Player2.Score,
                            X$Best.of, 1200, which.min(error.rankings))

errorRate(chess.rankings$validation[-(1:30)])

windows(width=7, height=7)
plotSystem(chess.rankings, c(900,1500), 1:nPlayers, 1:nPlayers, "Chess System")

######################################################3

A <- matrix(0, nrow=nPlayers, ncol=nPlayers)
dimnames(A) <- list(playerGraph, playerGraph)
B <- matrix(0, nrow=nPlayers, ncol=nPlayers)
dimnames(B) <- list(playerGraph, playerGraph)


for (iwin in 1:nPlayers)
{
  for (iloss in 1:nPlayers)
  {
    if (iwin == iloss)
    {
      A[iwin, iloss] <- 0
      B[iwin, iloss] <- 0
    }
    else
    {
      ind <- which(X$Player1.Win==playerGraph[iwin] & X$Player2.Loss==playerGraph[iloss])
      if (length(ind) == 0)
      {
        A[iwin, iloss] <- 0
        B[iwin, iloss] <- 0
      }
      else
      {
        A[iwin, iloss] <- length(ind)
        B[iwin, iloss] <- sum(X$Player1.Score[ind])
      }
    }
  }
}

A <- A[1:4,1:4]
B <- B[1:4,1:4]

A2 <- t(A)
B2 <- t(B)

diag(A2) <- rowSums(A)
diag(B2) <- rowSums(B)

A2 <- apply(A2, 2, "/", rowSums(A2))
B2 <- apply(B2, 2, "/", rowSums(B2))

rho <- 0.95

idiag <- diag(1, ncol(A), ncol(A))
A3 <- idiag - rho*A2
B3 <- idiag - rho*B2

rank(sort(1/diag(solve(A3)), decreasing=TRUE))
rank(sort(1/diag(solve(B3)), decreasing=TRUE))

#######################################################33

require(sna)

windows(h=7, w=7)
gplot(A, label=playerGraph[1:4])

windows(h=7, w=7)
gplot(A, label=playerGraph[1:4], edge.lwd=A)




