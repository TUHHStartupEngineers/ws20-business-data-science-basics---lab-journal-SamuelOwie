D <- 1000
K <- 5
h <- 0.25
round(sqrt(2*D*K/h),3)
mean(1:6)
die <- 1:6
round(mean(die))

roll_dice <- function(){
  die <- 1:6
  dice <- sample(die, size=2, replace=TRUE)
  sum(dice)
}
roll_dice()

roll_dice2 <- function(faces){
  dice <- sample(faces, size=2, replace=TRUE)
  sum(dice)
}
roll_dice2(faces=1:6)

calc_EOQ <- function(D=1000){
  K <- 5
  h <- 0.25
  Q <- sqrt(2*D*K/h)
  Q
}

calc_EOQ()
calc_EOQ(D=4000)
getwd()

?sqrt

