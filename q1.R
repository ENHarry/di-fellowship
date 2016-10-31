diceroll <- function(N,M){
  set.seed(2745)
  die <- 1:6
  prob.die <- c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6)
  r = 0
  rollsum = 0
  rollproduct = 1
  rollvector = c()
  while (r <= N & rollsum < M) {
    r <- r + 1
    roll <- sample(die, size = 1, prob = prob.die, replace = T)
    rollvector = c(rollvector, roll)
    rollsum <- rollsum + roll
    rollproduct <- rollproduct * roll
    rollsd = sd(rollvector)
  }
  print(rollproduct)
  print(rollsd)
}