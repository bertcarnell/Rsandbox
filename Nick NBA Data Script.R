# Nick NBA Data Script

#----------
# packages
require(XML)
require(RCurl)

#-------------
# functions

getNbaTeams <- function (year)
{
  # HTML page with all teams
  teams <- htmlTreeParse(paste("http://www.82games.com/", year, sep=""))
  # get all values from the page
  y <- xmlRoot(teams)[["body"]][["pre"]]
  teamsList <- xmlApply(y, function(x) xmlValue(x))
  
  # get all the HTM
  ind <- grep(".HTM", teamsList)
  # take out unnecessary text
  teamsList <- gsub("11", "", teamsList[ind])
  teamsList <- gsub(".HTM", "", teamsList)
  teamsList <- gsub("[0-9]+", "", teamsList)
  # get the unique teams
  teamsList <- unique(teamsList)
  # take out bad team names
  ind <- which(teamsList %in% c("CSORT", "ROLRTG"))
  teamsList <- teamsList[-ind]
  return(teamsList)
}

getData <- function(year, team, pageNum)
{
  op.backup <- options()
  options(stringsAsFactors=FALSE)
  url.tmp <- paste("http://www.82games.com/", year, "/", year, team, pageNum, 
                   ".HTM", sep="")
  
  teamHTML <- readLines(url.tmp)
  teamHTML <- gsub("\\&[^;]+;","", teamHTML)
  teamHTML <- htmlParse(teamHTML)
  
  if (!is.null(pageNum) && pageNum == pagenum.units)
  {
    X <- readHTMLTable(teamHTML, header=FALSE, trim=TRUE)[[1]][,2:9]
    
    # get rid of NAs
    ind <- which(is.na(X[,1]))
    X <- X[-ind,]
    # find header rows
    ind <- which(X[,1]=="Unit")
    Y <- X[(ind[1]+1):(ind[2]-1),]
    Z <- X[(ind[2]+1):nrow(X),]
    # combine halves
    W <- cbind(Y, Z[,-1])
    
    names(W) <- c("Unit", "Min", "Off", "Def", "PlusMinus", "Win", "Loss", 
                  "WinPerc", "eFG", "eFGA", "FTA", "Close", "dClose", "Reb", 
                  "TO")
    W$Min <- gsub("\\%", "", W$Min)
    class(W$Min) <- "numeric"
    class(W$Off) <- "numeric"
    class(W$Def) <- "numeric"
    class(W$PlusMinus) <- "numeric"
    class(W$Win) <- "numeric"
    class(W$Loss) <- "numeric"
    class(W$WinPerc) <- "numeric"
    class(W$eFG) <- "numeric"
    class(W$eFGA) <- "numeric"
    class(W$FTA) <- "numeric"
    W$Close <- gsub("\\%", "", W$Close)
    class(W$Close) <- "numeric"
    W$dClose <- gsub("\\%", "", W$dClose)
    class(W$dClose) <- "numeric"
    W$Reb <- gsub("\\%", "", W$Reb)
    class(W$Reb) <- "numeric"
    W$TO <- gsub("\\%", "", W$TO)
    class(W$TO) <- "numeric"
  } else if (is.null(pageNum)) # == pagenum.players)
  {
    X <- readHTMLTable(teamHTML, header=FALSE, trim=TRUE)[[2]][-(1:5),]
    
    ind <- which(is.na(X$V2))
    W <- X[-ind,]
    
    names(W) <- c("Player", "Min", "Own", "Opp", "Net", "On", "Off", 
                  "PlusMinusNet", "SimpleRating")
    W$Min <- gsub("\\%", "", W$Min)
    class(W$Min) <- "numeric"
    class(W$Own) <- "numeric"
    class(W$Opp) <- "numeric"
    class(W$Net) <- "numeric"
    class(W$On) <- "numeric"
    class(W$Off) <- "numeric"
    class(W$PlusMinusNet) <- "numeric"
    class(W$SimpleRating) <- "numeric"
  } else {
    stop("only the code for parsing the unit stats and individual stats has been implemented")
  }
  
  options(op.backup)
  return(W)
}

#----------
# parameters

pagenum.units <- 2
pagenum.floortime <- 1
pagenum.season <- 3
pagenum.players <- NULL

#----------
# get teams

year <- 1112 # 2011-2012

teamsList <- getNbaTeams(year)

teamDataFrames <- vector("list", length=length(teamsList))
playerDataFrames <- vector("list", length=length(teamsList))
for (i in 1:length(teamsList))
{
  teamDataFrames[[i]] <- getData(year, teamsList[i], pagenum.units)
  playerDataFrames[[i]] <- getData(year, teamsList[i], pagenum.players)
}
names(teamDataFrames) <- teamsList
names(playerDataFrames) <- teamsList

save(teamDataFrames, playerDataFrames, file=file.path("~", "NickData.Rdata"))
