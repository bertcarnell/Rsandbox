filePath <- file.path("C:","Users","carnellr","Documents","Quicken",
  "vanguard investment transactions.TXT")

readLines(filePath, n=10)

# blank, date, account, type factor, security factor, junk, shares, shares,
#   adjust, cash, cash, blank
X <- read.table(filePath, header=FALSE, sep="\t", skip=7,
  nrows=1376, colClasses="character")[,-c(1,3,6,13)]

names(X) <- c("date", "action", "security", "quote", "shares", "comission", 
  "cash", "amount", "total")
unique(X$security)
unique(X$action)

array(NA, dim=c(length(
by(as.numeric(X$V7), list(as.factor(X$V4), as.factor(X$V5)), sum, na.rm=TRUE, 
  simplify=TRUE)
  





