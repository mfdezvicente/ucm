# MUESTREO SISTEMÁTICO 

# FUNCIONES AUXILIARES

muestras_sistematicas <- function(N, n) {
k <- N / n
if (k != floor(k)) {
stop("En este material se trabaja con k entero: k = N/n.")
}
k <- as.integer(k)
muestras <- lapply(1:k, function(delta) {
seq(from = delta, by = k, length.out = n)
})

names(muestras) <- paste0("delta_", 1:k)
muestras
}





var_media_dif_sucesivas <- function(x, N) {
n <- length(x)
f <- n / N
suma_dif <- sum(diff(x)**2)
(1- f) * suma_dif / (2 * n * (n- 1))
}


var_total_dif_sucesivas <- function(x, N) {
N**2 * var_media_dif_sucesivas(x, N)
}



var_media_orden_aleatorio <- function(x, N) {
n <- length(x)
f <- n / N
(1- f) * var(x) / n
}


var_total_orden_aleatorio <- function(x, N) {
N**2 * var_media_orden_aleatorio(x, N)
}



var_prop_orden_aleatorio <- function(indicador, N) {
n <- length(indicador)
f <- n / N
p <- mean(indicador)
q <- 1- p
(1- f) * p * q / (n- 1)
}

# EJERCICIO 1 
N <- 5000 
n <- 10
k <- N/n 
k 
muestras <- muestras_sistematicas(N, n)
muestras


# EJERCICIO 2 
X <- c(1, 3, 5, 2, 4, 6, 2, 7, 3)
N <- length(X)
n <- 3
k <- N / n
muestras <- muestras_sistematicas(N, n)
resultado <- data.frame(
    arranque = 1:k,
    unidades = sapply(muestras, function(s) paste0("u", s, collapse = ", ")),
    valores = sapply(muestras, function(s) paste(X[s], collapse = ", ")),
    media = sapply(muestras, function(s) mean(X[s])),
    total_estimado = sapply(muestras, function(s) N * mean(X[s]))
)
resultado


# EJERCICIO 3

X <-  c(1,3,5,2,4,6,2,7)
k <-2 
N <- 8 
n <- 4

muestras <- muestras_sistematicas(N,n)
medias <- sapply(muestras, function(partes) mean(X[partes]))
medias

media_poblacional <- mean(X)

var_media_exacta <- mean((medias-media_poblacional)**2)
var_total_exacta <- (N**2)*var_media_exacta

# EJERCICIO 4
N <- 1300
k <- 10 
n <- N/k
f <- n/N

x_bar <- 3.52
sj2 <- 0.48

varianza_media <- (1-f)*sj2/n
varianza_media
error_media <- sqrt(varianza_media)
error_media
# [1] 0.05764614

error_total <- error_media * N
error_total
# [1] 74.93998
X_hat <- x_bar *N
IC <- X_hat + c(-1,1) * 1.96*error_total
IC



# EJERCICIO 5 
N <- 24
R2 <- c("SI","SI","NO","NO","SI","SI","NO","SI",
"NO","NO","SI","NO","NO","SI","SI","SI",
"SI","SI","NO","NO","SI","SI","NO","NO")
N <- length(R2)
n <- 8
k <- N / n
delta <- 1

muestra <- muestras_sistematicas(N,n)
s <- seq(from = delta, by= k, length.out = n)
indicador <- ifelse(R2[s]== "SI", 1, 0)
pj <- mean(indicador)
qj <- 1 -pj 
f <- n/N 

total_estimado <- N*pj
V_hat_P <- (1-f)*pj*qj/(n-1)
error <- sqrt(V_hat_P)
error
error_total = N*error
error_total

# EJERCICIO 6
R3 <- c(10,15,17,13,16,14,19,23,
12,19,20,13,21,18,19,23,
18,21,19,16,25,18,12,20)
N <- length(R3)
n <- 6
k <- N / n
f <- n/N

s1 <- seq(from = 1, by = k, length.out= n)
s3 <- seq(from = 3, by = k, length.out= n)
x1 <- R3[s1]
x3 <- R3[s3]

media1 <- mean(x1)
media3 <- mean(x3)

posiciones_comb <- sort(c(s1, s3))
X_comb <- R3[posiciones_comb]

media_comb <- mean(X_comb)

sumdiff <- sum(diff(X_comb)**2)
varianza_sucesiva <- (1- f) * sumdiff / (2 * n * (n- 1))
varianza_sucesiva
se <- sqrt(varianza_sucesiva)
se


# EJERCICIO 7
R1 <- c("B","A","M","M","B","M","A","A",
"A","A","M","M","A","A","A","A",
"A","B","B","M","A","A","A","B")
N <- length(R1)
n <- 6
k <- N / n
muestras <- muestras_sistematicas(N, n)
p_j <- sapply(muestras, function(s) mean(R1[s]== "A"))
q_j <- 1- p_j

P <- mean(R1== "A")
Q <- 1- P
V_exacta_directa <- mean((p_j- P)**2)
V_exacta_formula <- P * Q- mean(p_j * q_j)


# EJERCICIO 8
x <- c(12, 11.97, 12.01, 12.03, 12.01, 11.8, 11.91, 11.98,
12.03, 11.98, 12, 11.83, 11.87, 12.01, 11.98, 11.97)
N <- 240
n <- length(x)
f <- n / N

estimacion_media <- mean(x)
var <- var_media_dif_sucesivas(x, N)
error <- sqrt(var)
IC <- estimacion_media + c(-1,1) * 1.96 * error
IC


# EJERCICIO 9
x <- c(160,350,325,283,289,293,296,385,327,
298,289,316,290,293,295,306,298,290)
N <- 180
n <- length(x)
media_hat <- mean(x)
total_hat <- N * media_hat
suma_dif <- sum(diff(x)**2)
V_hat_media <- var_media_dif_sucesivas(x, N)
V_hat_total <- Nˆ2 * V_hat_media
SE_total <- sqrt(V_hat_total)
media_hat

# EJERCICIO 10 
x <- c(20,34,56,0,24,0,10,18,11,4,
19,22,33,3,12,3,25,12,30,10,
23,49,54,51,20,4,38,11,1,32,
35,17,32,43,45,25,52,2,29,31)
N <- length(x)
n <- 10
k <- N / n
delta <- 3

s <- seq(from = delta, by = k, length.out = n)
s
xs <- x[s]

estimacion_media <- mean(xs)
estimacion_media

varianza <- var_media_dif_sucesivas(xs, N) 
error <- sqrt(varianza)
error

