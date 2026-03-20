# =========================
# Funciones útiles (m.a.s. con reposición, no orden) 
# ========================= 

muestras_ordenadas_repetidas <- function(x,  n){
    as.data.frame(
        expand.grid(rep(list(x), n), stringsAsFactors = FALSE)
    )
}

muestras_no_ordenadas_repetidas <- function(x,n){
    df <- muestras_ordenadas_repetidas(x,n)
    sort_files <- t(apply(df, 1, sort))
    unique(sort_files)
}

total_muestras <- function(N,n){
    choose(N+n-1 , n)
}

# Probabilidad de una secuencia concreta (m.a.s. con reposición, ordenado)
prob_secuencia_rep <- function(N,n) {
    1/N^n
}

# Probabilidaddes de inclusión (m.a.s. con reposición, no ordenado)
prob_muestras <- function(N,n){
    total <- total_muestras(N,n)
    1/total
}


# Probabilidades de primera inclusión (m.a.s. con reposición, no ordenado)
pi_1_rep_noord <- function(N, n) 1 - ((N - 1) / N)^n
pi_2_rep_noord <- function(N, n) 1 - 2 * ((N - 1) / N)^n + ((N - 2) / N)^n

# Estimadores
est_media_rep <- function(x){
    mean(x)
}
est_total_rep <- function(x,N){
    N*mean(x)
}
est_prop_rep <- function(d){
    mean(d)
}

# Varianzas estimadas (con reposición)
var_hat_media_rep <- function(x) {
n <- length(x)
var(x) / n
}
var_hat_total_rep <- function(x, N) {
N^2 * var_hat_media_rep(x)
}
var_hat_prop_rep <- function(d) {
n <- length(d)
p <- mean(d); q <- 1 - p
p * q / (n - 1)
}
# Errores e intervalos
error_muestreo <- function(v) sqrt(v)
erm <- function(est, v) sqrt(v) / est # error relativo muestral
ema <- function(v, alpha = 0.05) qnorm(1 - alpha/2) * sqrt(v)
ic_normal <- function(est, v, alpha = 0.05) {
z <- qnorm(1 - alpha/2)
c(inferior = est - z * sqrt(v), superior = est + z * sqrt(v))
}



# =========================
# EJERCICIO 1  - SECUENCIAS ORDENADAS CON REPOSICION
# ========================= 

X <- c("A", "B", "C")
N <- length(X)
n <- 2
# Todas las muestras 
muestras_ordenadas_repetidas(X, n)
# Número total de meuestras
nrow(muestras_ordenadas_repetidas(X, n))
#[1] 9
# Probabilidad de una concreta
prob_muestras(N,n)
#[1] 0.1111111


# =========================
# EJERCICIO 2  - SIMULACIÓN DE UNA MUESTRA CON REPETICIÓN
# ========================= 
pob <- c(1,2,3,4,5,6)
n <- 10 

set.seed(123)
muestra_simulada <- sample(pob, n, replace = TRUE)
muestra_simulada

# Frecuencia de aparición 
table(muestra_simulada)

# Comprueba si hay valores repetidos
any(duplicated(muestra_simulada))  # any lo que hace es que de una cadena de TRUE, FALSE te dice si hay algún TRUE


# =========================
# EJERCICIO 3  - PRIMERA INCLUSIÓN
# ========================= 
N <- 20
n <- 6
pi1 <- pi1_rep(N, n) 
pi1
# [1] 0.2649081


# =========================
# EJERCICIO 4  - SEGUNDA INCLUSIÓN
# ========================= 
N <- 20
n <- 6
pi2 <- pi_2_rep_noord(N,n)
pi2
# [1] 0.06125722
pi1**2 == pi2
# [1] FALSE

# =========================
# EJERCICIO 5  - EJERCICIO TIPO PRESENTACIÓN
# ========================= 
X <- c("u1", "u2", "u3", "u4")
N <- length(X)
n <- 3

# a) Probabilidad de que una muestra contenga u1
# Sabemos que se podría hacer mediante el cálculo de la primera inclusión. Aun así vamos a hacerlo por enumeración y sacando cuando aparece este dato

muestras_ordenadas_repetidas <- function(x,  n){
    as.data.frame(
        expand.grid(rep(list(x), n), stringsAsFactors = FALSE)
    )
}

muestras_no_ordenadas_repetidas <- function(x,n){
    df <- muestras_ordenadas_repetidas(x,n)
    sort_files <- t(apply(df, 1, sort))
    unique(sort_files)
}
total_muestras(N,n)
pi1_rep <- function(N,n){
    1- ((N-1)/N)**n
}
calc_ui <- function(X, n){
    muestras <- muestras_no_ordenadas_repetidas(X,n)
    probabilidad <- apply(muestras, 1, function(fila) as.integer("u1" %in% fila))
    mean(probabilidad)

}
calc_ui(X,n)
pi1_(N,n)


# A) Y B) por fórmulas de inclusión (no ordenado con reposición)
p_u1 <- pi_1_rep_noord(N,n)
p_u1u2 <- pi_2_rep_noord(N,n)



# =========================
# EJERCICIO 6  - EJERCICIO VERIFICACIÓN POR SIMULACIÓN DE PROBABILIDADES DE INCLUSIÓN
# ========================= 

# Datos
x <- 1:10
N <- length(x)
n <- 5


# Teórico
pi1_teo <- pi_1_rep_noord(N, n)
pi12_teo <- pi_2_rep_noord(N, n)
pi1_teo
pi12_teo

# Visión por simulación
set.seed(321)
B <- 5000
simulacion <- replicate(B, sample(x, n, replace= TRUE), simplify = FALSE)
pi1_sim <- mean(sapply(simulacion, function(elemento) 1 %in% elemento))
pi2_sim <- mean(sapply(simulacion, function(elemento) (1 %in% elemento) & (2 %in% elemento)))
pi1_sim
pi2_sim



# Visión de sacar todas las muestras, calcular las probabilidades y ver qué da
muestras_ordenadas_repetidas <- function(x,  n){
    as.data.frame(
        expand.grid(rep(list(x), n), stringsAsFactors = FALSE)
    )
}
muestras_no_ordenadas_repetidas <- function(x,n){
    df <- muestras_ordenadas_repetidas(x,n)
    sort_files <- t(apply(df, 1, sort))
    unique(sort_files)
}

calc_prob1 <- function(x, n){
    muestras <- muestras_ordenadas_repetidas(x,  n)
    probabilidad <- apply(muestras, 1, function(fila) as.integer(1 %in% fila))
    mean(probabilidad)

}
calc_prob2 <- function(x, n){
    muestras <- muestras_no_ordenadas_repetidas(x,n)
    probabilidad <- apply(muestras, 1, function(fila) as.integer((1 %in% fila) & (2 %in% fila)))
    mean(probabilidad)

}
calc_prob1(x,n)
calc_prob2(x,n)


resumen <- c(valor_simulado = pi1_sim, valor_extracción = calc_prob1(x,n), valor_teórico = pi1_teo)
resumen

N <- 5
n <- 2
muestras <- combn(1:N, n, simplify = FALSE)
muestras


# =========================
# EJERCICIO 7  - RESUMEN DE MUESTRAL OBSERVADA
# ========================= 
x <- c(12, 15, 12, 18, 11, 15, 14, 13)
sum(x)

mean(x)

var(x)


# =========================
# EJERCICIO 8  - ESTIMACIONES INSESGADAS
# ========================= 
x <- c(21.5, 19.8, 23.1, 18.7, 20.4, 21.5, 22.0, 19.1, 24.3, 18.7, 20.9, 22.4)
N <- 100
xbar_hat <- est_media(x)
Xhat <- est_total(x, N)
Vhat_xbar <- var_hat_media_rep(x)
Vhat_X <- var_hat_total_rep(x, N)
e_xbar <- error_muestreo(Vhat_xbar)
e_X <- error_muestreo(Vhat_X)
IC_xbar_95 <- ic_normal(xbar_hat, Vhat_xbar, alpha = 0.05)
IC_X_95 <- ic_normal(Xhat, Vhat_X, alpha = 0.05)
xbar_hat

# =========================
# EJERCICIO 8  - PROPORCIÓN
# ========================= 
resp <- c("Si", "No", "No", "No", "Si", "No", "No", "Si")
d <- ifelse(resp == "Si", 1, 0)
phat <- est_prop(d)
Vhat_p <- var_hat_prop_rep(d)
e_p <- error_muestreo(Vhat_p)
IC_p_95 <- ic_normal(phat, Vhat_p, alpha = 0.05)
d


# =========================
# EJERCICIO 11 - APLICADO
# ========================= 

datos <- data.frame(
vaca = c(5, 23, 36, 36, 52, 64, 71, 92),
leche = c(40, 35, 50, 50, 38, 51, 23, 28),
enfermedad = c("Si", "No", "No", "No", "Si", "No", "No", "Si")
)
N <- 100
# Variable cuantitativa: leche
x <- datos$leche
xbar_hat <- est_media(x)
Vhat_xbar <- var_hat_media_rep(x)
IC_xbar_95 <- ic_normal(xbar_hat, Vhat_xbar, alpha = 0.05)
Xhat <- est_total(x, N)
Vhat_X <- var_hat_total_rep(x, N)
IC_X_95 <- ic_normal(Xhat, Vhat_X, alpha = 0.05)
# Variable dicotómica: enfermedad
d <- ifelse(datos$enfermedad == "Si", 1, 0)
phat <- est_prop(d)
Vhat_p <- var_hat_prop_rep(d)
IC_p_95 <- ic_normal(phat, Vhat_p, alpha = 0.05)
datos





xbar_hat
