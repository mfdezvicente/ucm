#### FUNCIONES AUXILIARES 

T_HH <- function(wi, pi){
    mean(wi/pi)
}
V_HH <- function(wi, pi, n, T_HH){
    sum((wi/pi - T_HH)^2)/((n-1)*n)
}


## EJERICIO 4

N <- 10
n <- 3

xi <- c(2,5,4)
pi<- c(0.1,0.2,0.15)

T_HH <- T_HH(xi, pi)
T_HH
# [1] 23.88889

V_HH <- V_HH(xi,pi,n,T_HH)
V_HH
# [1] 4.012346


## EJERCICIO 5 
x <- c(3,6,5)
pii <- c(0.3, 0.4, 0.25)
pij <- matrix (c(NA, 0.1, 0.08, 0.10, NA, 0.09, 0.08, 0.09, NA), nrow = 3, byrow = TRUE)

T_HT <- sum(x/ pii)
T_HT

A <- sum(x^2* (1-pii)/pii^2)
A

B <- 0 
for (i in 1:(length(x) - 1)){
    for (j in (i+1): length(x)){
        B <- B + 2*(x[i]/pii[i]) * (x[j]/pii[j]) * ((pij [i,j] - pii[i] * pii[j])/pij [i,j])
    }
}
B

V = A +B 
V

## EJERCICIO 7 - RAZÓN Y RELACIÓN 

N <- 50 
n <- 5 
f <- n/N
sumx <- 25
sumx2 <- 135
sumy <- 15
sumy2 <- 55
sumxy <- 85 

x_bar <- sumx/n 
y_bar <- sumy/n 
sx <- (sumx2  - n*x_bar^2)/(n-1)
sy <- (sumy2  - n*y_bar^2)/(n-1)
sxy <- 1/(n-1) * (sumxy - n*x_bar*y_bar)

R = y_bar/x_bar 
V <- (1-f)/(n*x_bar**2) * ((sy**2) + ((R**2) * (sx**2)) - 2*R*sxy)
B <- (1-f)/(n*x_bar**2) * (R*(sy**2) - sxy)

R
V
B


## EJERCICIO 8 
N <- 40 
X_bar <- 6 

x <- c(5,6,7,4)
y <- c(3,4,5,2)

mean(x)
R <- mean(y)/mean(x)
Y_r <- R * X_bar
Y <- Y_r * R


## EJERCICIO 9 
N <- 60 
X <- 8 

x <- c(6,7,8,9,10)
y <- c(5,6,7,8,9)

b <- cov(x,y)/var(x)
b
Y_reg <- mean(y) + b*(X - mean(x))
Y_reg
Y <- Y_reg * N
Y


## PROPUESTO 

N <- 50
X <- 10

x <- c(8,10,12,9,11)
y <- c (5,7,8,6,7)

R <- mean(y)/mean(x)
R
