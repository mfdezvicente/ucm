# =========================
# Funciones útiles (m.a.s. sin reposición, no orden) choose(N,n)
# =========================
# Esto es la probabilidad de generar una muestra en concreto a partir de N elementos en la población y n elementos en la muestra
prob_muestra <- function(N, n){
    1 / choose(N, n)
} 

# Probabilidades de primera y de segunda inclusión 
# Probabilidad de primera inclusión es la probabilidad de que un elemento en concreto esté presente en una muestra
pi_1 <- function(N, n) {
    n / N
}
# Probabilidad de segunda inclusión es la probabilidad de que dos elementos en concreto estén presentes en una muestra
pi_2 <- function(N, n) {
    n * (n - 1) / (N * (N - 1)) # i != j
}

# Estimadores insesgados (m.a.s. sin reposición)
# Valor estimado de la media poblacional
est_media <- function(x) {
    mean(x)
}

#Valor estimado del total poblacional
est_total <- function(x, N) {
    N * mean(x)
}

# Varianza estimada de la media
var_hat_media <- function(x, N) {
    n = length(x)
    f = n / N
    (1 - f) * var(x) / n
}

#Varianza estimada del total
var_hat_total <- function(x, N) {
    N**2 * var_hat_media(x, N)
}


# Proporción (variable indicadora 0/1 en la muestra)
est_prop <- function(d) {
    mean(d)
}
var_hat_prop <- function(d, N) {
    n <- length(d)
    f <- n / N
    p <- mean(d)
    q <- 1 - p
    (N / (N - 1)) * (1 - f) * p * q / (n - 1)
}


# Medidas de error
error_muestreo <- function(var_hat) {
    sqrt(var_hat) # e(Tˆ)
}

# Este es el error relativo
emr <- function(est, var_hat) {
    sqrt(var_hat) / est # EMR(Tˆ)
}
ema <- function(var_hat, alpha=0.05) {
    qnorm(1 - alpha/2) * sqrt(var_hat) # semianchura IC
}

ic_normal <- function(est, var_hat, alpha=0.05) {
    z <- qnorm(1 - alpha/2)
    c(inferior = est - z*sqrt(var_hat), superior = est + z*sqrt(var_hat))
}



# =========================
# Ejercicio 1
# =========================

X <- c(1,2,3,4,5)
n <- 2

# a)
muestras <- combn(X, n, simplify = FALSE)
muestras

# b)
choose(length(X),n) #[1] 10
length(muestras) # [1] 10 

#c) 
N <- length(X)
prob_muestra(N,n)
# [1] 0.1


# =========================
# Ejercicio 2
# =========================

# n = 3

X <- c(1,2,3,4,5)
n <- 3

# a)
muestras <- combn(X, n, simplify = FALSE)
muestras

# b)
choose(length(X),n) #[1] 10
length(muestras) # [1] 10 

#c) 
N <- length(X)
prob_muestra(N,n)
# [1] 0.1


# n = 4

X <- c(1,2,3,4,5)
n <- 4

# a)
muestras <- combn(X, n, simplify = FALSE)
muestras

# b)
choose(length(X),n) #[1] 5
length(muestras) # [1] 5

#c) 
N <- length(X)
prob_muestra(N,n)
# [1] 0.2

# =========================
# Ejercicio 3
# =========================

N <- 20
n <- 6

prob1 <- pi_1(N, n)
prob1
# [1] 0.3

# La probabilidad de que un elemento cualquiera de esos 20 que forman la población, se encuentre dentro de una muestra de seis elementos sacados mediante mas sin reposición es 0.3

# =========================
# Ejercicio 4
# =========================

N <- 6
n <- 3

calc_pi <- function(N,n){
    muestras <- combn(N,n, simplify = FALSE)
    pij <- sapply(1:N, function(i) mean(sapply(muestras, function(M) i %in% M)))
    pij
}
calc_pi(N,n)
n/N


# =========================
# Ejercicio 5 - Segunda Inclusión por Fórmula y por Enumeración
# =========================

N <- 6
n <- 3
calc_pij_enum <- function(N,n){
    muestras <- combn(1:N, n, simplify = FALSE)
    pares <- combn(1:N, 2)
    out <- apply(pares, 2, function(par){
        i <- par[1]
        j <- par [2]
        mean(sapply(muestras, function(M) (i %in% M) & (j %in% M)))
    })
    data.frame(i = pares[1,], j = pares[2,], pij_enum = out)
}


# =========================
# Ejercicio 6 - Características Poblacioneales y Muestrales
# =========================

X <- c(18,12,15,10,21,14,16,9)
x <- c(18,15,14,9)
N <- length(X)
n <- length(x)

total_muestral <- sum(x)
media_muestral <- mean(x)
cuasivarianza_muestral <- var(x)

total_poblacional <- sum(X)
media_poblacional <- mean(X)

# =========================
# Ejercicio 7 - Características Poblacioneales y Muestrales
# =========================