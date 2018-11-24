windows()
hist(USArrests$UrbanPop)

X <- USArrests
X$type <- ifelse(USArrests$UrbanPop >= 80, "urban", ifelse(USArrests$UrbanPop < 50, "rural", "suburban"))
X$region <- c(
"S", "W", "W", "MW", "W", "W",
"NE", "NE", "S", "S", "W", "W", 
"MW", "MW", "MW", "W", "S", "S", 
"NE", "NE", "NE", "MW", "MW", "S",
"MW", "W", "W", "W", "NE", "NE",
"W", "NE", "S", "W", "MW", "W",
"W", "NE", "NE", "S", "W", "S",
"S", "W", "NE", "S", "W", "MW",
"MW", "W")

table(X$region)

table(X$type, X$region)

ID <- paste(X$region, X$type, sep="_")

require(randomForest)

rf <- randomForest(X[,c(1,2,4)], as.factor(ID), mtry=2)

hc <- hclust(dist(USArrests))
plot(hc)
hc$merge

z <- cutree(hc, k=3)



