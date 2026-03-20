# Funciones Auxiliares 

## Muestra 
est_total <- function(x,N){
    N*mean(x)
}

est_media <- function(x){
    mean(x)   
}

cuasivarianza_muestral <- function(x){
    var(x)  
}


## Probabilidad de Inclusión 

pi1 <- function(N, n) {
    1 - ((N - 1) / N)^n
}

pi2 <- function(N,n){
    1 - 2*((N - 1) / N)^n +  ((N - 2) / N)^n
}

##Estimadores 

est_prop_repeticion <- function(d) mean(d)
est_varianza_prop_rep <- function(d, n) {
    p <- mean(d)
    q <- 1-p
    n <- length(x)
    p*q/(n-1)
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

error_muestreo_rep <- function(var) sqrt(var)
emr <- function(var, est) sqrt(var)/est
ema <- function(var, alpha=0.05) sqrt(var)*qnorm(1 - alpha/2)
IC <- function(var, est, alpha=0.05) {
    inf <- est - sqrt(var)*qnorm(1 - alpha/2)
    sup <- est + sqrt(var)*qnorm(1 - alpha/2)
    c(inferior = inf, superior = sup)
}

# =========================
# EJERCICIO 1 - PROBABIILDAD DE PRIMERA INCLUSIÓN
# ========================= 
N <- 10 
n <- 4 
pi1(N, n)

# [1] 0.3439 


# =========================
# EJERCICIO 2 - PROBABIILDAD DE SEGUNDA INCLUSIÓN
# ========================= 
N <- 10
n <- 5
resumen <- c(Distintos = pi2(N, n), Iguales = pi1(N,n)^2)
resumen


# =========================
# EJERCICIO 3 - PROBABIILDAD DE PRIMERA Y DE SEGUNDA INCLUSIÓN
# ========================= 
N <- 5 
n <- 3

# a) 𝑃(𝑢1 ∈ 𝑀)
pi1(N, n)
#[1] 0.488


# b) 𝑃(𝑢1, 𝑢2 ∈ 𝑀)
pi2(N, n)
#[1] 0.488

# c) 𝑃(𝑢1, 𝑢2 ∈ 𝑀)
0

# =========================
# EJERCICIO 4 - VERIFICACIÓN POR SIMULACIÓN
# ========================= 

N <- 12
n <- 6
B <- 6000

# Valores Teoricos 
pi1_teorico <- pi1(N, n)
pi2_teorico <- pi2(N, n)


# Valores Simulados
muestras <- replicate(B, sample(1:N, n, replace=TRUE), simplify = FALSE)
probabilidad1 <- sapply(muestras, function(filas) 1 %in% filas)
pi1_simulado <- mean (probabilidad1)


probabilidad2 <- sapply(muestras, function(filas) (1 %in% filas) & (2 %in% filas))
pi2_simulado <- mean (probabilidad2)


resumen <- c(teorico1 = pi1_teorico, teorico2 = pi2_teorico, simulado1 = pi1_simulado, simulado2 = pi2_simulado)
resumen

# > resumen
# teorico1  teorico2 simulado1 simulado2
# 0.4067078 0.1483136 0.4068333 0.1488333



# =========================
# EJERCICIO 5 - CARACTERÍSTICAS MUESTRALES Y ESTIMADORES (CON REPOSICIÓN)
# ========================= 
x = c(14, 18, 14, 16, 15, 18, 13, 14, 17)
N <- length(x)
n <- 9
total <- est_total(x, N)
media <- est_media(x)
cuasi <- cuasivarianza_muestral(x)

resumen <- c(total = est_total(x, N), media = est_media(x), cuasi = cuasivarianza_muestral(x))
resumen

# =========================
# EJERCICIO 6 -ESTIMACIÓN DEL TOTAL Y LA MEDIA (CON REPOSICIÓN)
# ========================= 
N = 90
n <- 12
x <- c(68,	72,	65,	71,	74,	68,	70,	77,	69,	73,	66,	75)

# a) Estima	media poblacional y el total
media <- est_media(x)
media
total <- est_total(x,N)
total
# b) Estima	varianza media poblacional y el total
var_hat_media_rep(x)
var_hat_total_rep(x,N)
# c) IC 
IC(var_hat_media_rep(x), est_media(x), alpha = 0.05)


# =========================
# EJERCICIO 7 -ESTIMACIÓN DEL PROPORCION
# ========================= 

n <- 15
# a)Construir la d 
d <- c(rep(1,6), rep(0, n-6))
est_prop_repeticion(d)
# [1] 0.4
est_varianza_prop_rep(d, n)
# [1] 0.02
IC(est_prop_repeticion(d), est_varianza_prop_rep(d, n), alpha = 0.05)
# inferior superior  -1.217772  1.261408

# =========================
# EJERCICIO 9 - EJERCICIOS APLICADOS
# ========================= 
datos <- data.frame(
    ruta = c(14,57, 57,88,103,146,177,201,245,292),
    tiempo = c(29,35,35,31,28,40,33,27,30,38),
    tarde = c("NO", "SÍ", "SÍ", "NO", "NO",  "SÍ", "SÍ", "NO",  "NO", "SÍ" )
)
N <- 300

# a) Estimar la	media del tiempo de	entrega	y dar IC del 95%.
entrega <- datos$tiempo
media_entrega <- est_media(entrega)
var_hat_media_rep(entrega)
IC_rep <- IC(var_hat_media_rep(entrega), est_media(entrega), alpha= 0.05)

cat("El valor de la media de entrega es", media_entrega,". El valor del IC al 95 % es", IC_rep)


# c) Estimar	la	proporción de	rutas	“tarde”	y	dar	IC	del	95%.
d <- ifelse(datos$tarde == "SÍ", 1, 0)
d

proporcion <- mean(d)
cat("La proporción es", proporcion)
p <- mean(d)
q <- 1 -p 
n <- length(d)
varianza_proporcion <- p*q/(n-1)
varianza_proporcion

# =========================
# EJERCICIO 11 - SIMULACIÓND DE MEDIAS MUESTRALES
# ========================= 

X <- c(9,12,15,20,24)
n <- 3 
B <- 2500

# a) calcula media muestral en cada muestra 
muestras <- replicate(B, sample(X, n, replace = TRUE), simplify = FALSE)
medias <- sapply(muestras, function(fila) mean(fila))

# b) Compruebe que la media de las medias simuladas se aproxima a la media poblacional

mean(medias)
mean(X)

# c) Reprensenta en un histograma
hist(medias, main = "Histograma de medias muestrales con reposición", xlab= "Media Muestral", breaks = 20)

hist(medias_sim, main = "Histograma de medias muestrales (con reposicion)",
xlab = "Media muestral", breaks = 20)
abline(v = mean(X), lwd = 2, col = "red")

