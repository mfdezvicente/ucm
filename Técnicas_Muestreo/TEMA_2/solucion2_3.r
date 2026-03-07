# =========================
# Funciones útiles (m.a.s. con reposición, no orden) 
# ========================= 

muestras_ordenadas_repetidas <- function(x,  n){
    as.data.frame(
        expand.grid(rep(list(x), n), stringsAsFactors = FALSE)
    )
}

muestas_no_ordenadas_repetidas <- function(x,n){
    df <- muestras_ordenadas_repetidas(x,n)
    sort_files <- t(apply(df, 1, sort))
    unique(sort_files)
}

total_muestras <- function(N,n){
    choose(N+n-1 , n)
}

prob_muestras <- function(N,n){
    total <- total_muestras(N,n)
    1/total
}

pi1_rep <- function(N,n){
    1- ((N-1)/N)**n
}


pi2_rep <- function(N,n){
    1- 2*((N-1)/N)**n + ((N-2)/N)**n
}


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

var_hat_media_rep <- function(x, n){
    var(x)/n
}
var_hat_total_rep <- function(x, n,N){
    (N**2) *var(x)/n
}
var_hat_prop_rep <- function(d, n,N){
    p <- mean(d)
    q <- 1-p
    p*q/(n-1)
}
error_muestro <- function(var_hat){
    sqrt(var_hat)
}
emr <- function(est, var_hat){
    sqrt(var_hat)/est
}
ema <- function(est, var_hat, alpha= 0.05){
    z <- qnorm(1-alpha/2)
    z*sqrt(var_hat)
}
IC <- function(est, var_hat, alpha= 0.05){
    z <- qnorm(1-alpha/2)
    q <- sqrt(var_hat)
    c(inferior = est -z*q, superior= est + z*q)
}


# =========================
# EJERCICIO 1  - SECUENCIAS ORDENADAS CON REPOSICION
# ========================= 

X <- c("A", "B", "C")
N <- length(X)
n <- 2
# Todas las muestras 
muestas_no_ordenadas_repetidas(X, n)
# Número total de meuestras
total_muestras(N,n)
# [1] 6
# Probabilidad de una concreta
prob_muestras(N,n)
# [1] 0.166666


# =========================
# EJERCICIO 2  - SIMULACIÓN DE UNA MUESTRA CON REPETICIÓN
# ========================= 

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
pi2 <-pi2_rep(N, n)
pi2
# [1] 0.06125722
pi1**2 == pi2
# [1] FALSE

# =========================
# EJERCICIO 5  - EJERCICIO POR SIMULACIÓN
# ========================= 
X <- c("u1", "u2", "u3", "u4")
N <- length(X)
n <- 3

# a) Probabilidad de que una muestra contenga u1
# Sabemos que se podría hacer mediante el cálculo de la primera inclusión. Aun así vamos a hacerlo por enumeración y sacando cuando aparece este dato


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
pi1_rep(N,n)
