muestras_sistematicas_dos <- function(N,n){
    k <- as.integer(N/n)
    muestras <- lapply(1:k, function(delta){
    seq(from= delta, by=k, length.out = n)})
    names(muestras) = paste0("delta", 1:k)
    muestras
}

hola <- muestras_sistematicas_dos(6,3) 
hola


###### 2 de Mayo del 2026 
## En este día vamos a realizar dos ejercicios aleatorios de cada uno de los temas. Seran elegidos por R 
# Tema 3
tema3 <- sample.int(4,2)
tema3
# Haremos el ejercicio 8 y 9 

## EJERCICIO 8 

estrato <- c("I" , "II", "III")
sigma <- c(4,12,80)
wh <- c(0.6, 0.3, 0.1)
N <- 1000
Nh <- wh*N
sh2 <- (Nh*sigma**2)/(Nh-1)

afijacion_proporcional_sin <- (sum(wh*sh2))/(5 + (sum(wh*sh2))/N)
afijacion_proporcional_sin
nh <- wh * afijacion_proporcional_sin
ceiling(nh)


afijacion_proporcional_con <- (sum(wh*sh2))/5
afijacion_proporcional_con
nhc <- wh * afijacion_proporcional_con
ceiling(nhc)


## EJERCICIO 9
Nh <- c(24, 36, 30, 30)
N <- 120
n <- 20 
nh <- c(4,6,5,5)
wh <- Nh/N
fh <- nh/Nh
s

estrato1 = c(94, 90, 102, 110) 
estrato2 = c(91, 99, 93, 105, 111, 101)
estrato3 = c(108, 96, 100, 93, 93)
estrato4 = c(92, 110, 94, 91, 113)
muestras <- list(estrato1, estrato2, estrato3, estrato4)

medias <- sapply(muestras, function(estrato) mean(estrato))
medias
varianzas <- sapply(muestras, function(estrato) var(estrato))
varianzas
media_muestras <- sum(wh*medias)
media_muestras
# [1] 99.3

varianza <- sum((wh**2)*(1-fh)*varianzas*(1/nh))
varianza
error <- sqrt(varianza)
error


tema4 <- sample.int(4,2)
tema4


# Tema 4
tema4 <- sample.int(10,2)
tema4
# Haremos el ejercicio 4 y 9

# EJERCICIO 4

N <- 10 
n <- 3
xi <- c(2,5,4)
pi <- c(0.1, 0.2, 0.15)

THH <- mean(xi/pi)
THH

var <- sum((xi/pi - THH)**2)/(n*(n-1))
var
# EJERCICIO 9

N <- 60 
X_bar <- 8 
x <- c(6,7,8,9,10)
y <- c(5,6,7,8,9)
sx2<- var(x)
sxy <- cov(x,y)

b <- sxy/sx2
Y_bar_hat <- mean(y) + b* (8- mean(x))
Y_bar_hat 


# Tema 5
tema6 <- sample.int(12,2)
tema6
# Ejericio 7 y 9 

# EJERCICIO 7 
R1 <- c("B","A","M","M","B","M","A","A",
"A","A","M","M","A","A","A","A",
"A","B","B","M","A","A","A","B")
indicador <- ifelse(R1=="A", 1,0)
N <- length(R1)
n <- 6 
k <- N/N

muestras <- muestras_sistematicas_dos(N,n)

pj <- sapply(muestras, function(delta) mean(R1[delta] == "A"))
qj <- 1- pj

# EJERCICIO 9 
x <- c(160,350,325,283,289,293,296,385,327,
298,289,316,290,293,295,306,298,290)
N <- 180
n <- 18 
f <- n/N
k <- N/n 
valor_total <- N*mean(x)

sumdiff <- sum(diff(x)**2)
var <- ((1-f)*sumdiff)/(2*n*(n-1))
var_total <- var * N**2 
error <- sqrt(var_total)
error
