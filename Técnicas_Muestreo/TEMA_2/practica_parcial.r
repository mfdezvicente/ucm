#  FUNCIONES ÚTILES 

pi1_reposicion <- function(N,n){
    1-((N-1)/(N))**n
}
pi2_reposicion <- function(N,n){
    1- 2*((N-1)/(N))**n + ((N-2)/(N))**n
}

# Estimadores 
est_media <- function(x){
    mean(x)
}
est_total <- function(x, N){
    N*mean(x)
}
est_prop <- function(d){
    mean(d)
}


var_hat_media_rep <- function(x, n){
    var(x)/n
}
var_hat_total_rep <- function(x, n){
    N**2 * var(x)/n
}

var_hat_prop_rep <- function(x, n){
    p <- mean(d)
    q <- 1-p 
    p*q/(n-1)
}

IC <- function(var, est, alpha){
    c(inferior = est - qnorm(1-alpha/2)* sqrt(var), superior = est + qnorm(1-alpha/2)* sqrt(var))
}

error <- function(var){
    sqrt(var)
}
emr <- function(var, est){
    sqrt(var)/est
}
ema <- function(var, alpha){
    qnorm(1-alpha/2)* sqrt(var)
}


# ===============================
# EJERCICIO 1 - PROBABILIDAD DE PRIMERA INCLUSIÓN
# ===============================

N <- 10
n <- 5
pi <- pi1_reposicion(N,n)
pi 

# ===============================
# EJERCICIO 2 - PROBABILIDAD DE SEGUNDA INCLUSION
# ===============================

N <- 10
n <- 5
pi_2 <- pi2_reposicion(N,n)
pi_2 

# Comparación con pi1**2
pi_2  == pi**2

# ===============================
# EJERCICIO 3 - PROBABILIDAD DE SEGUNDA INCLUSION
# ===============================

# Es lo mismo 

# ===============================
# EJERCICIO 4 - PROBABILIDAD DE SEGUNDA INCLUSION
# ===============================
X <- 1:12
N <- 12
n <- 6
B <- 6000

muestras <- replicate(B, sample(X, n, replace = TRUE), simplify = FALSE)
# a) 
probabilidad_1 <- mean(sapply(muestras, function(filas) 1 %in% filas))
probabilidad_1
pi1_reposicion(N,n)

# b)
probabilidad_2 <- mean(sapply(muestras, function(filas) (1 %in% filas) &(2 %in% filas)))
probabilidad_2
pi2_reposicion(N,n)


# ===============================
# EJERCICIO 5 - CARACTERÍSTICAS MUESTRALES Y ESTIMADORES 
# ===============================
n <- 9 
x <- c(14, 18, 14, 16, 15, 18, 13, 14, 17)
# a ) Total Muestral 
sum(x)
# b) Media Muestral 
mean(x)
# c) Cuasivarianza muestral 
var(x)

# ===============================
# EJERCICIO 6 - ESTIMAR MEDIA Y TOTAL Y IC 
# ===============================
N <- 90 
n <- 12
x <- c(68,72,65,71,74,68,70,77,69,73,66,75)

# a) Estimar la media poblacional y total poblacional
est_media(x)
est_total(x, N)

# [1] 70.66667
# [[1] 6360

# b) Estimar sus varianzas 
var_hat_media_rep(x, n)
var_hat_total_rep(x,n)
# [1] 1.126263
# [1] 9122.727

# c) IC 
IC_media <- IC(var_hat_media_rep(x, n), est_media(x), alpha = 0.05)
IC_media
# 68.58664 72.74669  

IC_total <- IC(var_hat_total_rep(x, n),est_total(x, N),  alpha = 0.05)
IC_total
# 6172.798 6547.202 

# ===============================
# EJERCICIO 7 - ESTIMAR PROPORCIONES
# ===============================
n<- 15
d <- c(rep(1,6), rep(0, 9))
d
length(d)
# b) Estimar la p
est_prop(d)
# [1] 0.4
var_hat_prop_rep(d, n)
# [1] 0.01714286
error <- sqrt(var_hat_prop_rep(x, n))
error
# [1] 0.1309307

IC_prop <- IC(var_hat_prop_rep(d, n),est_prop(d),  alpha = 0.05)
IC_prop
# 0.1433805 0.6566195 


# ===============================
# EJERCICIO 8 - TIPOS DE ERRORES
# ===============================

n<- 15
d <- c(rep(1,6), rep(0, 9))
d

# b) Estimar la p
est <- est_prop(x)
est
# [1] 70.66667
var <- var_hat_prop_rep(x, n)
var

error(var)
emr(var, est)
ema(var, alpha = 0.05)

# ===============================
# EJERCICIO 9 - EJERCICIOS APLICADOS 
# ===============================
N <- 300
datos <- data.frame(
    ruta = c(14,57,57,88,103,146,177,201,245,292),
    tiempo = c(29,35,35,31,28,40,33,27,30,38),
    tarde = c( "No","Sí", "Sí", "No", "No", "Sí", "Sí", "No", "No", "Sí" )
)
datos

# a) Estimar la media del tiempo de entrega y dar un IC95%
tiempos <- datos$tiempo
# Media 
est_media(tiempos)
# [1] 32.6
IC <- IC(var_hat_media_rep(tiempos, length(tiempos)), est_media(tiempos), alpha = 0.05)
IC
# 29.90312 35.29688 


# b) Estimar el total del tiempo de entrega y dar un IC95%
tiempos <- datos$tiempo
# Media 
est_total(tiempos, N)
var_hat_total_rep(tiempos, 10)
# [1] 9780
# [1] 170400
IC_2 <- IC(170400, 9780, alpha = 0.05)
IC_2
# 970.936 10589.064 

# c) Estimar la proporción de rutas tarde 
d <- ifelse(datos$tarde == 'Sí', 1, 0)
d
n<- length(d)
est_prop(d)
var_hat_prop_rep(d, n)
IC(var_hat_prop_rep(d, n), est_prop(d), alpha = 0.05)
# 0.1733393 0.8266607 



# ===============================
# EJERCICIO 11 - SIMULACIÓN FINAL 
# ===============================
X <- c(9,12,15,20,24)
n <- 3
set.seed(1)
B <- 2500

muestras <- replicate(B, sample(X, n, replace = TRUE), simplify = FALSE)
muestras

# a) Calcula lamedia muestral en cada muestra
media <- sapply(muestras, function(fila) mean(fila))
media 

# b) 
mean(media) # [1] 16.12467
media_poblacional <- mean(X) # [1] 16

# c) 
hist(media, main= "Distribución de las medias muestrales")
abline(v = media_poblacional, col = "red", lwd = 2)



#  ================================================================================== #
# Practica de Combinatoria
Omega <- c(1,2,3)
n <- 2

muestras_ordenadas_con_repeticion <- function(Omega, n){
    expand.grid(rep(list(Omega),n), stringsAsFactors = FALSE)
}
muestras_ordenadas_con_repeticion(Omega,n)

muestras_no_ordenadas_con_repeticion <- function(Omega,n){
    df <- muestras_ordenadas_con_repeticion(Omega, n)
    sorted_rows <- t(apply(df, 1, sort))
    unique(sorted_rows)
}
muestras_no_ordenadas_con_repeticion(Omega, n)

muestras_ordenado_sin_rep <- function(Omega, n){
  df <- muestras_ordenadas_con_repeticion(Omega, n)
  keep <- apply(df, 1, function(x) length(unique(x)) == length(x))
  df[keep, , drop=FALSE]
}

muestras_no_ordenadas_sin_repeticion <- function(Omega, n){
    t(combn(Omega,n))
}

muestras <- function(Omega, n){
    df <- muestras_ordenadas_con_repeticion(Omega, n)
    keep <- apply(df, 1, function(x) length(unique(x)) == length(x))
    df[keep , ,drop = FALSE]
}


PR <- function(a.vec) {
    N <- sum(a.vec)
    factorial(N)/prod(factorial(a.vec))
}
a.vec <- c(2,1,1)
PR(a.vec)


tab <- table(strsplit("BANANA", "")[[1]])
tab
PR(as.integer(tab))
