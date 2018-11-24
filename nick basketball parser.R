srcFile <- file.path("C:","Users","Rob","Downloads","playbyplay20120510040.txt")

file.exists(srcFile)

X <- read.table(srcFile, header=TRUE, sep="\t", stringsAsFactors=FALSE)
date <- substring(X$GameID,1,8)
team1 <- substring(X$GameID,9,11)
team2 <- substring(X$GameID,12,14)

splits <- strsplit(X$Entry, " ")

team <- character(nrow(X))
score <- character(nrow(X))
player <- character(nrow(X))
playerFirst <- character(nrow(X))
rebound <- logical(nrow(X))
missed <- logical(nrow(X))
made <- logical(nrow(X))
for (i in 1:nrow(X))
{
  if (grepl("[Tt]echnical",X$Entry[i]))
  {
    team[i] <- ""
    score[i] <- ""
    player[i] <- ""
    playerFirst[i] <- ""
    rebound[i] <- FALSE
    missed[i] <- FALSE
    next
  }
  if (grepl("[Rr]ebound", X$Entry[i]))
  {
    if (grepl("[Tt]eam", X$Entry[i]))
    {
      rebound[i] <- FALSE
    } else {
      rebound[i] <- TRUE
    }
  } else
  {
    rebound[i] <- FALSE
  }
  if (grepl("[Mm]issed", X$Entry[i]))
  {
    missed[i] <- TRUE
  } else
  {
    missed[i] <- FALSE
  }
  line <- splits[[i]]
  if (grepl("^[[]", line[1]))
  {
    team[i] <- substring(line[1], 2, 4)
    if (grepl("[]]$", line[1]))
    {
      score[i] <- ""
      if (grepl("[A-Z][a-z]*[.]", line[2]))
      {
        playerFirst[i] <- line[2]
        player[i] <- line[3]
      } else
      {
        playerFirst[i] <- ""
        player[i] <- line[2]
      }
    } else
    {
      score[i] <- gsub("[]]", "", line[2])
      if (grepl("[A-Z][a-z]*[.]", line[3]))
      {
        playerFirst[i] <- line[3]
        player[i] <- line[4]
      } else
      {
        playerFirst[i] <- ""
        player[i] <- line[3]
      }
    }
  } else
  {
    team[i] <- ""
    score[i] <- ""
    player[i] <- ""
    playerFirst[i] <- ""
  }
}

score <- numeric(nrow(X))
for (i in 1:(nrow(X)-2))
{
  if (missed[i] && rebound[i+1])
  {
    if (grepl("[Mm]ade", X$Entry[i+2]))
    {
      if (grepl("3pt", X$Entry[i+2]))
      {
        score[i] <- 3
      } else
      {
        score[i] <- 2
      }
    } else if (missed[i+2])
    {
      score[i] <- 0
    } else if ()
  }
}
