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
# a) Tamaño 2
pop = c(1,2,3,4)
N = length(pop)
n = 2

muestras2 <- combn(pop, n, simplify = FALSE) # El simplify FALSE devuelve una lista
muestras2

m2 <- length(muestras2)
pM2 <- 1/m2
pM2


# b) Tamaño 3
n = 3
muestras3 <- combn(pop, n, simplify = FALSE)
m3 <- length(muestras3)
pM3 <- 1/m3
pM3
# [1] 0.25

#c) Confuso

# =========================
# Ejercicio 2
# =========================

#a) 
pm <- prob_muestra(10,4) #[1] 0.004761905

#b)
N=10
n <- 4
muestras <- combn(N, n, simplify = FALSE)
muestras
sum(rep(pm, length(muestras)))  # rep hace repterir algo tantas veces como la longitud que le pones, el sum los suma

# [1] 1

# =========================
# Ejercicio 3
# =========================
# Recuperamos la urna del ejercicio 1. 
N <- 4

# a) Para n = 2, calcula pi_i para 1,2,3,4. El número es solo una etiqueta, es por ello que da igual el número que elijamos, son equipobrables
n = 2

pi_formula <- pi_1(N,n) #[1] 0.5
muestras2 <- combn(1:N, n, simplify = FALSE)
pi_enum2 <- sapply(1:N, function(i) mean(sapply(muestras2, function(M) i %in% M)))
pi_formula
pi_enum2

#b) Para n = 3
n = 3
muestras3 <- combn(1:4, n, simplify = FALSE)
pi_enum3 <- sapply(1:N, function(i) mean(sapply(muestras3, function(M) i %in%  M)))
pi_enum3


# c) 
muestras_no_ordenado_sin_rep(pop, n)
# El ejericico pide que miremos y contemos a mano


# =========================
# Ejercicio 4 - Segunda Inclusión
# =========================

N <- 4

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

# n = 2
n <- 2

pij_formula2 <- pi_2(N,n)
tabla2 <- calc_pij_enum(N,n)

pij_formula2
tabla2


# n = 3

n<- 3
pij_formula3 <- pi_3(N,n)
tabla3 <- calc_pij_enum(N,n)

pij_formula3
tabla3

# =========================
# Ejercicio 5 - Caracterísitcas Poblacionales y Muestrales
# =========================

X <- c(12, 15, 9, 20, 14, 10)
x <- c(12, 9, 14)

# Muestral 
sum_muestral <- sum(x)
media_muestral <- mean(x)
s2_muestral <- var(x)

# Población
sum_pob <- sum(X)
media_pob <- mean(X)
s2_pob <- var(X)


# =========================
# Ejercicio 6 - Estimadores Insensagos de Media, Total y Proporción
# =========================
x <- c(35.5,30.2,28.9,36.4,29.8,34.1,32.6,26.4,38.0,38.2,32.2,27.5)
N <- 120
n <- length(x)

# a) Estimar el total poblacional y el error de muestreo 
est_total(x,N)
var_hat_total(x,N)
error_muestreo(var_hat_total(x,N))
ic_normal(est_total(x,N), var_hat_total(x,N), alpha = 0.05)

#b) Estimar la media poblacional y el error de muestro
est_media(x)
var_hat_media(x,N)
error_muestreo(var_hat_media(x,N))
ic_normal(est_media(x), var_hat_media(x,N), alpha = 0.05)

# Proporción 
d <- as.integer(x>32)
est_prop(d)
var_hat_prop(d,N)
error_muestreo(var_hat_prop(d,N))
ic_normal(est_prop(d), var_hat_prop(d,N), alpha = 0.05)


# =========================
# Ejercicio 7 - Tipos de Error de Muestreo
# =========================
# Usamos el total estimado
x <- c(35.5,30.2,28.9,36.4,29.8,34.1,32.6,26.4,38.0,38.2,32.2,27.5)
N <- 120
n <- length(x)
est_total(x,N)
var_hat_total(x, N)
e <- error_muestreo(var_hat_total(x,N))
EMR <- emr(est_total(x,N), var_hat_total(x, N))
ECM <- var_hat_total(x, N) # Esto es porque es insengado
EMA_95 <- ema(var_hat_total(x, N), alpha = 0.05) # Semianchura del IC95


0.3*2.5 + 0.3*0.7 + 0.7*1.4 - 3
