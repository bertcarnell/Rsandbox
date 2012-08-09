rm(list=ls())
require(RUnit)

sumFirstN <- function(n) n*(n+1)/2
sumFirstN2 <- function(n) n*(n+1)*(2*n+1)/6
sumFirstN3 <- function(n) (sumFirstN(n))^2
mDiceVersusNDice <- function(m, n, k)
{
  #probability that one of the m dice is greater than all n dice with k sides
  # order statistics argument
  a <- 2:k
  1/k^(m+n)*sum((a^m - (a-1)^m)*(a-1)^n)
}

probMthOrderStatisticEqualsA <- function(m, k, a)
{
  (a^m - (a-1)^m)/k^m
}

probMthOrderStatisticLessThanA <- function(m, k, a)
{
  ((a-1)^m)/k^m
}

TwoVersusTwoLoop <- function(k, bAttackerWin2, bDefenderWin2, 
  attackerBonus=0, defenderBonus=0)
{
  # if calculating for the attacker to win 2
  if (bAttackerWin2 && !bDefenderWin2)
  {
    top1 <- k-1 + attackerBonus - defenderBonus
    top2 <- k-1 - defenderBonus
    offst1 <- 1 - attackerBonus + defenderBonus
    offst2 <- 1
  } else if (!bAttackerWin2 && bDefenderWin2)
  {
    top1 <- k - attackerBonus
    top2 <- k - attackerBonus
    offst1 <- 0 + attackerBonus - defenderBonus
    offst2 <- 0
  }
  su <- 0
  for (y2 in 1:top2)
  {
    for (y1 in y2:top1)
    {
      for (x2 in (y2+offst2):k)
      {
        for (x1 in max(y1+offst1,x2):k)
        {
          #print(c(x1,x2,y1,y2))
          if (x1==x2 && y1 != y2) su <- su + 2
          else if (x1==x2 && y1 == y2) su <- su + 1
          else if (x1!=x2 && y1 == y2) su <- su + 2
          else if (x1!=x2 && y1 != y2) su <- su + 4
        }
      }
    }
  }
  return(su/k^4)
}

countWinners <- function(x1, x2, x3, y1, y2)
{
  m <- 1
  if (y1 != y2) m <- m*2
  if (x1 != x2)
  {
    if (x2 != x3) m <- m*6
    else if (x2 == x3) m <- m*3
  } else if (x1 == x2)
  {
    if (x2 != x3) m <- m*3
    #else if (x2 == x3) m <- m*1
  }
  return(m)
}

ThreeVersusTwoLoop <- function(k, bAttackerWin2, bDefenderWin2,
  attackerBonus=0, defenderBonus=0)
{
  su <- 0
  if (bAttackerWin2 && !bDefenderWin2)
  {
    top1 <- k-1 + attackerBonus - defenderBonus
    top2 <- k-1 - defenderBonus
    offst1 <- 1 - attackerBonus + defenderBonus
    offst2 <- 1
    for (y2 in 1:top2)
    {
      for (y1 in y2:top1)
      {
        for (x2 in (y2+offst2):k)
        {
          for (x1 in max(y1+offst1,x2):k)
          {
            for (x3 in 1:x2)
            {
              su <- su + countWinners(x1, x2, x3, y1, y2)
            }
          }
        }
      }
    }
  } else if (!bAttackerWin2 && bDefenderWin2)
  {
    top <- k - attackerBonus
    offst <- 0 + attackerBonus - defenderBonus
    for (x3 in 1:top)
    {
      for (x2 in x3:top)
      {
        for (x1 in x2:top)
        {
          for (y2 in x2:k)
          {
            for (y1 in max(x1+offst, y2):k)
            {
              su <- su + countWinners(x1, x2, x3, y1, y2)
            }
          }
        }
      }
    }
  } 
  return(su/k^5)
}

prob2DiceAttackerVersus2DiceDefender <- function(k, attackerBonus=0, defenderBonus=0)
{
  a <- c(TwoVersusTwoLoop(k, T, F, attackerBonus, defenderBonus), 
         TwoVersusTwoLoop(k, F, T, attackerBonus, defenderBonus))
  c(a[1], 1-a[1]-a[2], a[2])
}

prob3DiceAttackerVersus2DiceDefender <- function(k, attackerBonus=0, defenderBonus=0)
{
  a <- c(ThreeVersusTwoLoop(k, T, F, attackerBonus, defenderBonus), 
         ThreeVersusTwoLoop(k, F, T, attackerBonus, defenderBonus))
  c(a[1], 1-a[1]-a[2], a[2])
}

################################################################################

createGrid <- function(pa11, pa21, pa31, pa12, pa22, pa32)
{
  # rows are attackers in the country, columns are defenders in the country
  Z <- matrix(NA, nrow=10, ncol=10)
  Z[2,1] <- pa11[1]
  Z[3,1] <- pa21[1] + pa21[2]*pa11[1]
  Z[2,2] <- pa12[1]*pa11[1]
  Z[3,2] <- pa22[1] + pa22[2]*Z[2,1] + pa22[3]*0
  
  for (b in 3:10)
  {
    Z[2,b] <- pa12[1]*Z[2,b-1] #+ pa12[2]*0
    Z[3,b] <- pa22[1]*Z[3,b-2] + pa22[2]*Z[2,b-1] #+ pa22[3]*0
  }
  
  for (a in 4:10) 
    Z[a,1] <- pa31[1] + pa31[2]*Z[a-1,1]
  for (a in 4:10) 
    Z[a,2] <- pa32[1] + pa32[2]*Z[a-1,1] + pa32[3]*Z[a-2,2]
  for (b in 3:10)
  {
    for (a in 4:10)
    {
      Z[a,b] <- pa32[1]*Z[a,b-2] + pa32[2]*Z[a-1,b-1] + pa32[3]*Z[a-2,b]
    }
  }
  return(Z)
}

expectedLossGrid <- function(pa11, pa21, pa31, pa12, pa22, pa32)
{
  # rows are attackers in the country, columns are defenders in the country
  Z <- matrix(NA, nrow=10, ncol=10)
  Z[2,1] <- pa11[2]*1
  Z[3,1] <- pa21[2]*(1 + Z[2,1])
  Z[2,2] <- pa12[2]*1
  Z[3,2] <- pa22[2]*(1 + Z[2,1]) + pa22[3]*2
  
  for (b in 3:10)
  {
    Z[2,b] <- pa12[1]*Z[2,b-1] + pa12[2]*1 
    Z[3,b] <- pa22[1]*Z[3,b-2] + pa22[2]*(1+Z[2,b-1]) + pa22[3]*2
  }
  
  for (a in 4:10) 
    Z[a,1] <- pa31[1]*0 + pa31[2]*(1+Z[a-1,1])
  for (a in 4:10) 
    Z[a,2] <- pa32[1]*0 + pa32[2]*(1+Z[a-1,1]) + pa32[3]*(2+Z[a-2,2])
  for (b in 3:10)
  {
    for (a in 4:10)
    {
      Z[a,b] <- pa32[1]*(0+Z[a,b-2]) + pa32[2]*(1+Z[a-1,b-1]) + pa32[3]*(2+Z[a-2,b])
    }
  }
  return(Z)
}

RiskResultGenerator <- function(k, attackerBonus=0, defenderBonus=0)
{
  sumOver <- k - 1 + attackerBonus - defenderBonus
  
  # pa11 should be P(X > Y) = sum_y{P(X>Y|Y=y)P(Y=y)}
  pa11 <- sumFirstN(sumOver)/k^2
  pa11 <- c(pa11, 1-pa11)
  # pa21 should be P(X1 > Y or X2 > Y) = sum_y{P(X1 > Y or X2 > Y | Y=y)P(Y=y)}
  #  = pa11 + pa11 - sum_y{P(X1 > Y & X2 > Y | Y=y)P(Y=y)}
  pa21 <- 2*sumFirstN(sumOver)/k^2 - sumFirstN2(sumOver)/k^3
  pa21 <- c(pa21, 1-pa21)
  # pa31 should be P(X1 > Y or X2 > Y or X3 > Y)
  pa31 <- 3*sumFirstN(sumOver)/k^2 - 3*sumFirstN2(sumOver)/k^3 + sumFirstN3(sumOver)/k^4
  pa31 <- c(pa31, 1-pa31)
  # pa12 should be P(X > Y1 and X > Y2) = sum_y1y2{P(X > Y1 and X > Y2 | Y1=y1 and Y2=y2)P(Y1=y1 and Y2=y2)}
  #1/6*1/6*(5/6*(0*2+1)+4/6*(1*2+1)+3/6*(2*2+1)+2/6*(3*2+1)+1/6*(4*2+1))
  pa12 <- sum((sumOver:1)*(2*(0:(sumOver-1))+1))/k^3
  pa12 <- c(pa12, 1-pa12)
  # pa22
  pa22 <- prob2DiceAttackerVersus2DiceDefender(k, attackerBonus, defenderBonus)
  # pa32
  pa32 <- prob3DiceAttackerVersus2DiceDefender(k, attackerBonus, defenderBonus)

  pWin <- createGrid(pa11, pa21, pa31, pa12, pa22, pa32)

  ELoss <- expectedLossGrid(pa11, pa21, pa31, pa12, pa22, pa32)
  
  return(list(pa11=pa11, pa21=pa21, pa31=pa31, pa12=pa12, pa22=pa22, 
              pa32=pa32, pWin=pWin, ELoss=ELoss))
}



baseCase <- RiskResultGenerator(6, attackerBonus=0, defenderBonus=0)
attackLeader <- RiskResultGenerator(6, attackerBonus=1, defenderBonus=0)
defendLeader <- RiskResultGenerator(6, attackerBonus=0, defenderBonus=1)
defendLeaderFortress <- RiskResultGenerator(6, attackerBonus=0, defenderBonus=2)




################################################################################
# Tests

checkEqualsNumeric(TwoVersusTwoLoop(3, T, F), 0.135802469) 
checkEqualsNumeric(TwoVersusTwoLoop(3, F, T), 0.567901235)
checkEqualsNumeric(TwoVersusTwoLoop(3, T, F, 1), 0.259259259)
checkEqualsNumeric(TwoVersusTwoLoop(3, F, T, 1), 0.259259259)
checkEqualsNumeric(TwoVersusTwoLoop(3, T, F, 0, 1), 0.037037037)
checkEqualsNumeric(TwoVersusTwoLoop(4, T, F, 0, 1), 0.08203125)
checkEqualsNumeric(TwoVersusTwoLoop(4, T, F, 0, 2), 0.01953125)
checkEqualsNumeric(TwoVersusTwoLoop(3, F, T, 0, 1), 0.691358025)
checkEqualsNumeric(TwoVersusTwoLoop(3, F, T, 0, 2), 0.716049383)

checkEqualsNumeric(ThreeVersusTwoLoop(3, T, F), 0.242798354)
checkEqualsNumeric(ThreeVersusTwoLoop(6, T, F), 0.3716563786)
checkEqualsNumeric(ThreeVersusTwoLoop(3, T, F, 1), 0.465020576)
checkEqualsNumeric(ThreeVersusTwoLoop(3, T, F, 0, 1), 0.065843621)
checkEqualsNumeric(ThreeVersusTwoLoop(4, T, F, 0, 2), 0.033203125)

checkEqualsNumeric(ThreeVersusTwoLoop(6, F, T), 0.2925668724)
checkEqualsNumeric(ThreeVersusTwoLoop(3, F, T), 0.403292181)
checkEqualsNumeric(ThreeVersusTwoLoop(3, F, T, 1), 0.144032922)
checkEqualsNumeric(ThreeVersusTwoLoop(2, F, T, 0, 1), 0.625)

calcProbs <- function(nAttackerDice, nDefenderDice)
{
  attackers <- lapply(1:nAttackerDice, function(x) 1:6)
  defenders <- lapply(1:nDefenderDice, function(x) 1:6)

  A <- expand.grid(attackers)
  D <- expand.grid(defenders)

  a <- 6^nAttackerDice
  d <- 6^nDefenderDice

  WA <- matrix(0, nrow=a, ncol=d)

  for (i in 1:a)
  {
    sortA <- sort(A[i,], decreasing=TRUE)

    for (j in 1:d)
    {
      sortD <- sort(D[j,], decreasing=TRUE)

      if (sortA[1] <= sortD[1])
      {
        WA[i,j] <- WA[i,j]-1
      }
      if (nAttackerDice >= 2 && nDefenderDice >= 2)
      {
        if (sortA[2] <=  sortD[2])
        {
          WA[i,j] <- WA[i,j]-1
        }
      }
    }
  }
  p <- numeric(3)
  p[1] <- length(which(WA == 0))/a/d
  p[2] <- length(which(WA == -1))/a/d
  if (nAttackerDice >= 2 && nDefenderDice >= 2)
  {
    p[3] <- length(which(WA == -2))/a/d
  }
  return(p)
}

checkEqualsNumeric(baseCase$pa32, calcProbs(3, 2))
checkEqualsNumeric(baseCase$pa31, calcProbs(3, 1)[1:2])
checkEqualsNumeric(baseCase$pa22, calcProbs(2, 2))
checkEqualsNumeric(baseCase$pa21, calcProbs(2, 1)[1:2])
checkEqualsNumeric(baseCase$pa11, calcProbs(1, 1)[1:2])
checkEqualsNumeric(baseCase$pa12, calcProbs(1, 2)[1:2])

checkEqualsNumeric(baseCase$pa11[1], mDiceVersusNDice(1,1,6))
checkEqualsNumeric(baseCase$pa21[1], mDiceVersusNDice(2,1,6))
checkEqualsNumeric(baseCase$pa31[1], mDiceVersusNDice(3,1,6))
checkEqualsNumeric(baseCase$pa12[1], mDiceVersusNDice(1,2,6))


simulate <- function(nStartAttack, nStartDefend, N)
{
  y <- numeric(N)
  for (i in 1:N)
  {
    x <- c(nStartAttack, nStartDefend)
    while (x[1] > 1 && x[2] > 0)
    {
      if (x[1] >= 4)
      {
        if (x[2] >= 2)
        {
          change <- sample(0:2, 1, prob=pa32)
          if (change == 0) x <- x - c(0,2)
          if (change == 1) x <- x - c(1,1)
          if (change == 2) x <- x - c(2,0)
        } else if (x[2] == 1)
        {
          change <- sample(0:1, 1, prob=pa31[1:2])
          if (change == 0) x <- x - c(0,1)
          if (change == 1) x <- x - c(1,0)
        }
      } else if (x[1] == 3)
      {
        if (x[2] >= 2)
        {
          change <- sample(0:2, 1, prob=pa22)
          if (change == 0) x <- x - c(0,2)
          if (change == 1) x <- x - c(1,1)
          if (change == 2) x <- x - c(2,0)
        } else if (x[2] == 1)
        {
          change <- sample(0:1, 1, prob=pa21[1:2])
          if (change == 0) x <- x - c(0,1)
          if (change == 1) x <- x - c(1,0)
        }
      } else if (x[1] == 2)
      {
        if (x[2] >= 2)
        {
          change <- sample(0:1, 1, prob=pa12[1:2])
          if (change == 0) x <- x - c(0,1)
          if (change == 1) x <- x - c(1,0)
        } else if (x[2] == 1)
        {
          change <- sample(0:1, 1, prob=pa11[1:2])
          if (change == 0) x <- x - c(0,1)
          if (change == 1) x <- x - c(1,0)
        }
      } else if (x[1] == 1)
      {
        stop("should not be here")
      }
    }
    if (x[2] <= 0)
    {
      y[i] <- 1
    } else if (x[1] == 1)
    {
      y[i] <- 0
    } else stop(paste("why am I here?", x[1], x[2]))
  }
  return(sum(y)/N)
}

answer <- matrix(nrow=10, ncol=10)
for (a in 2:10)
{
  for (d in 1:10)
  {
    answer[a,d] <- simulate(a,d, 10000)
  }
}

checkEqualsNumeric(baseCase$pWin, answer, tol=6E-3)


