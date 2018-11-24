require(PlayerRatings)
require(RCurl)

connectString <- "https://docs.google.com/spreadsheet/pub?key=0Am6z8RkbEVRCdG9HUWJVNTM3SEpUVWltV2toWGlPdnc&single=true&gid=1&output=csv"
curl <- getCurlHandle()
junk <- curlSetOpt(ssl.verifypeer=FALSE, ssl.verifyhost=FALSE, curl=curl)
connectUrl <- getURL(connectString, curl=curl)
X <- read.csv(textConnection(connectUrl), stringsAsFactors=FALSE)

default <- file.path("S:")
source(file.path(default, "PingPongTournamentUtilities.R"))

playerGraph <- unique(c(X$Player2.Loss, X$Player1.Win))
nPlayers <- length(playerGraph)
#X$Date <- strptime(rep("12/29/2012",length(X$Player1.Win)), "%m/%d/%Y")
X$Date <- X$X


Y <- data.frame(date=as.numeric(X$Date), 
                player1=as.character(X$Player1.Win),
                player2=as.character(X$Player2.Loss), 
                matchScore=1, stringsAsFactors=FALSE)

Y2 <- data.frame(date=as.numeric(X$Date), 
                 player1=as.character(X$Player1.Win),
                 player2=as.character(X$Player2.Loss), 
                 matchScore=(X$Player1.Score-X$Player2.Score)/6, 
                 stringsAsFactors=FALSE)

Y3 <- data.frame(date=as.numeric(X$Date), 
                 player1=as.character(X$Player1.Win),
                 player2=as.character(X$Player2.Loss), 
                 matchScore=(X$Player1.MatchScore-X$Player2.MatchScore)/20, 
                 stringsAsFactors=FALSE)

##################################################################

elo.rankings <- elo(Y, history=TRUE, gamma=0)
fide.rankings <- fide(Y, history=TRUE)
glicko.rankings <- glicko(Y, history=TRUE, gamma=0)
steph.rankings <- steph(Y, history=TRUE, gamma=0)

windows()
par(mfrow=c(2,2))
par(mar=c(5,4,4,2)-1)
plot(elo.rankings, main="Elo - Match", xlab="", ylab="", players=playerGraph)
legend("bottomleft", legend=playerGraph, col=1:length(playerGraph), 
       lty=1:length(playerGraph), bg="white", ncol=2)
plot(fide.rankings, main="FIDE", xlab="", ylab="", players=playerGraph)
plot(glicko.rankings, main="Glicko", xlab="", ylab="", players=playerGraph)
plot(steph.rankings, main="Stephanson", xlab="", ylab="", players=playerGraph)

#######

elo.rankings <- elo(Y2, history=TRUE, gamma=0)
fide.rankings <- fide(Y2, history=TRUE)
glicko.rankings <- glicko(Y2, history=TRUE, gamma=0)
steph.rankings <- steph(Y2, history=TRUE, gamma=0)

windows()
par(mfrow=c(2,2))
par(mar=c(5,4,4,2)-1)
plot(elo.rankings, main="Elo - Match Ratio", xlab="", ylab="", players=playerGraph)
legend("bottomleft", legend=playerGraph, col=1:length(playerGraph), 
       lty=1:length(playerGraph), bg="white", ncol=2)
plot(fide.rankings, main="FIDE", xlab="", ylab="", players=playerGraph)
plot(glicko.rankings, main="Glicko", xlab="", ylab="", players=playerGraph)
plot(steph.rankings, main="Stephanson", xlab="", ylab="", players=playerGraph)

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


#############

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

A2 <- t(A)
B2 <- t(B)

diag(A2) <- rowSums(A)
diag(B2) <- rowSums(B)

A2 <- apply(A2, 2, "/", rowSums(A2))
B2 <- apply(B2, 2, "/", rowSums(B2))

rho <- 0.95

idiag <- diag(1, nPlayers, nPlayers)
A3 <- idiag - rho*A2
B3 <- idiag - rho*B2

rank(sort(1/diag(solve(A3)), decreasing=TRUE))
rank(sort(1/diag(solve(B3)), decreasing=TRUE))

require(sna)

windows(h=7, w=7)
gplot(A, label=playerGraph)


