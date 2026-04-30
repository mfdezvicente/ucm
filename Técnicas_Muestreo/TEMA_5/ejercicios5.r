# EJERCICIO - TEMA 5


# EJERCICIO 1 - MUESTREO POR CONGLOMERADOS CON MISMO TAMAÑO
N <- 240
n <- 6
Mbar <- 10
M <- N * Mbar

bloques <- list(
    c(3,2,4,3,5,4,2,3,4,2),
    c(1,2,2,3,2,1,2,3,2,2),
    c(4,5,3,4,4,5,4,3,4,4),
    c(2,3,2,2,3,2,1,2,3,2),
    c(5,4,4,5,3,4,5,4,4,5),
    c(3,3,2,4,3,2,3,4,3,3)
)
x_bar_i <- sapply(bloques, mean)
xi <- sapply(bloques, sum)

datos <- data.frame(
    bloque= 1:6,
    Xi = xi,
    Media = x_bar_i)

datos

estimador_media = mean(x_bar_i)
estimador_media
#[1] 3.116667


f <- n/N
sb2 <- sum(Mbar*((x_bar_i - estimador_media)^2))/(n-1)
sb2

varianza <- ((1-f)*sb2)/(n*Mbar)
varianza

error <- sqrt(varianza)
error



# EJERCICIO 2
N <- 180 
M <- 5400
Mbar <- 5400/180
n <- 5


Mi <- c(28,35,24,31,29)
mi <- c(6,7,5,6,6)
xbari <- c(82, 76, 91, 88, 79)
si2 <- c(64, 49, 81, 36, 100)


# a) Precio medio del alquiler
x_bar <- (N/M)*sum(Mi*xbari)/n
x_bar

# b) Varianza 

# Primeramente calculamos el f1
f1 <- n/N
sbb2 <- 1/(n-1) * sum((Mi* xbari - Mbar*x_bar)**2)
