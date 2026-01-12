rgamma_AR <- function(n, alpha){
  stopifnot(alpha > 0, alpha <= 1)
  c1 <- 1/alpha + 1/exp(1)
  p1 <- (1/alpha) / c1
  
  out <- numeric(n)
  i <- 1
  while(i <= n){
    V <- runif(1)
    if(V <= p1){
      # tramo (0,1): Beta(alpha,1)
      Y <- runif(1)^(1/alpha)
    } else {
      # tramo [1,inf): 1 + Exp(1)
      Y <- 1 + rexp(1, rate = 1)
    }
    
    U <- runif(1)
    if(Y < 1){
      if(U <= exp(-Y)){
        out[i] <- Y; i <- i + 1
      }
    } else {
      if(U <= Y^(alpha - 1)){
        out[i] <- Y; i <- i + 1
      }
    }
  }
  out
}
