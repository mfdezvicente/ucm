############################################################
# TEMA 1 · INTRODUCCIÓN — TÉCNICAS DE MUESTREO (SCRIPT .R)
# Objetivo: replicar el contenido del .Rmd en formato R
# Autor: (pon tu nombre)
############################################################

#############################
# 0) Preparación: notación -> R
#############################

# Ω (población) como vector
Omega <- c("A","B","C","D")

# ui: i-ésima unidad. En R, si u = Omega, entonces u[i] representa ui
u <- Omega

# N: tamaño poblacional
N <- length(Omega)

# n: tamaño muestral
n <- 2

# Comprobaciones rápidas
Omega
N
u[1]; u[2]; u[3]; u[4]


############################################################
# 1) Los 4 esquemas de muestreo (concepto)
# - Con reposición: puede repetirse una unidad (A, A) permitido
# - Sin reposición: no puede repetirse (A, A) imposible
#
# - Ordenado: (A, B) distinto de (B, A)
# - No ordenado: (A, B) y (B, A) son la misma muestra
############################################################


########################################
# 2) Enumeración completa de muestras
########################################

# 2.2.1 Todas las n-tuplas (ordenado con reposición)
# Idea: expand.grid crea todas las combinaciones posibles de posiciones
muestras_ordenado_con_rep <- function(Omega, n){
  as.data.frame(
    expand.grid(rep(list(Omega), n), stringsAsFactors = FALSE)
  )
}


# 2.2.2 Ordenado sin reposición
# Filtramos las tuplas donde todos los elementos de cada fila son distintos
muestras_ordenado_sin_rep <- function(Omega, n){
  df <- muestras_ordenado_con_rep(Omega, n)
  keep <- apply(df, 1, function(x) length(unique(x)) == length(x))
  df[keep, , drop = FALSE]
}

# 2.2.3 No ordenado sin reposición
# combn devuelve combinaciones (sin orden) sin repetición
muestras_no_ordenado_sin_rep <- function(Omega, n){
  t(combn(Omega, n))
}

# 2.2.4 No ordenado con reposición
# Truco general: 
# (i) generamos las tuplas ordenadas con reposición
# (ii) ordenamos cada fila internamente (A,B) y (B,A) quedan iguales
# (iii) quitamos duplicados
muestras_no_ordenado_con_rep <- function(Omega, n){
  df <- muestras_ordenado_con_rep(Omega, n)
  sorted_rows <- t(apply(df, 1, sort))
  unique(sorted_rows)
}

# Ejemplo con Ω={A,B,C,D}, n=2
Omega <- c("A","B","C","D")
N <- length(Omega)
n <- 2

S_osr  <- muestras_ordenado_sin_rep(Omega, n)      # ordenado sin reposición
S_nosr <- muestras_no_ordenado_sin_rep(Omega, n)   # no ordenado sin reposición
S_ocr  <- muestras_ordenado_con_rep(Omega, n)      # ordenado con reposición
S_nocr <- muestras_no_ordenado_con_rep(Omega, n)   # no ordenado con reposición

# Tamaños m (número total de muestras posibles)
m_osr  <- nrow(S_osr)
m_nosr <- nrow(S_nosr)
m_ocr  <- nrow(S_ocr)
m_nocr <- nrow(S_nocr)

c(osr = m_osr, nosr = m_nosr, ocr = m_ocr, nocr = m_nocr)

# Inspección de las listas
S_osr
S_nosr
S_ocr
S_nocr


############################################################
# 3) Combinatoria: VN,n, CN,n, VRN,n, CRN,n, PN, PR
############################################################

# Variación sin repetición: VN,n = N!/(N-n)!
VN  <- function(N, n) factorial(N) / factorial(N - n)

# Combinación sin repetición: CN,n = choose(N,n)
CN  <- function(N, n) choose(N, n)

# Variación con repetición: VRN,n = N^n
VRN <- function(N, n) N^n

# Combinación con repetición: CRN,n = choose(N + n - 1, n)
CRN <- function(N, n) choose(N + n - 1, n)

# Permutación: PN = N!
PN  <- function(N) factorial(N)

# Permutación con repetición:
# Si hay N elementos en total, y repeticiones a,b,c,... con sum(a_vec)=N:
# PR = N!/(a! b! c! ...)
PR  <- function(a_vec){
  N <- sum(a_vec)
  factorial(N) / prod(factorial(a_vec))
}

# Comprobación con N=4, n=2 (debería dar 12, 6, 16, 10)
N <- 4; n <- 2
c(VN = VN(N,n), CN = CN(N,n), VRN = VRN(N,n), CRN = CRN(N,n))


############################################################
# 4) Validación general: fórmula vs enumeración (N=5, n=3)
############################################################

Omega <- LETTERS[1:5]
N <- length(Omega)
n <- 3

m_teor <- c(
  osr  = VN(N,n),
  nosr = CN(N,n),
  ocr  = VRN(N,n),
  nocr = CRN(N,n)
)

m_emp <- c(
  osr  = nrow(muestras_ordenado_sin_rep(Omega,n)),
  nosr = nrow(muestras_no_ordenado_sin_rep(Omega,n)),
  ocr  = nrow(muestras_ordenado_con_rep(Omega,n)),
  nocr = nrow(muestras_no_ordenado_con_rep(Omega,n))
)

m_teor
m_emp


############################################################
# 5) Probabilístico vs no probabilístico (simulación)
# Ejemplo: muestreo por conveniencia puede ser sesgado
############################################################

set.seed(123)

# Construimos una población grande:
# A = accesibilidad (1 accesible, 0 no accesible)
# Y = variable de interés, con media mayor cuando A=1
N <- 10000
A <- rbinom(N, 1, 0.4)
Y <- rnorm(N, mean = 0 + 1.0 * A, sd = 1)

# Media poblacional "real"
Y_bar_pop <- mean(Y)
Y_bar_pop

# Simulación Monte Carlo: repetimos B muestras
B <- 1000
n <- 200

est_conven <- numeric(B)  # estimaciones con conveniencia
est_MAS    <- numeric(B)  # estimaciones con MAS (aleatorio simple)

for(b in 1:B){
  # Conveniencia: solo se elige dentro de A=1
  idx_conv <- sample(which(A == 1), n, replace = FALSE)
  est_conven[b] <- mean(Y[idx_conv])

  # MAS: muestra aleatoria simple sin reposición en toda la población
  idx_mas <- sample.int(N, n, replace = FALSE)
  est_MAS[b] <- mean(Y[idx_mas])
}

resumen <- c(
  media_pop    = Y_bar_pop,
  media_conven = mean(est_conven),
  media_MAS    = mean(est_MAS),
  sesgo_conven = mean(est_conven) - Y_bar_pop,
  sesgo_MAS    = mean(est_MAS) - Y_bar_pop,
  sd_conven    = sd(est_conven),
  sd_MAS       = sd(est_MAS)
)
resumen


############################################################
# 6) Fuentes de error: sesgo de cobertura, no respuesta, error de muestreo
############################################################

#############################
# 6.1 Sesgo de cobertura
#############################
set.seed(1)

N <- 5000
grupo <- rbinom(N, 1, 0.2)                  # 20% subgrupo difícil
Y <- rnorm(N, mean = 2 * grupo, sd = 1)     # grupo=1 tiene media mayor

mu_pop <- mean(Y)

# Marco incompleto: excluye grupo=1
marco <- which(grupo == 0)

n <- 200
idx <- sample(marco, n, replace = FALSE)
mu_muestra <- mean(Y[idx])

c(media_poblacion = mu_pop,
  media_estimada  = mu_muestra,
  sesgo           = mu_muestra - mu_pop)


#############################
# 6.2 No respuesta
#############################
set.seed(2)

N <- 5000
Y <- rnorm(N)

n <- 400
idx <- sample.int(N, n, replace = FALSE)

# (a) No respuesta aleatoria (80% responde)
resp_a <- rbinom(n, 1, 0.8)
est_a <- mean(Y[idx][resp_a == 1])

# (b) No respuesta depende de Y (ejemplo): valores altos responden menos
p_resp <- plogis(1 - 1.2 * Y[idx])
resp_b <- rbinom(n, 1, p_resp)
est_b <- mean(Y[idx][resp_b == 1])

c(
  media_pop = mean(Y),
  est_noresp_aleatoria    = est_a,
  est_noresp_dependiente  = est_b,
  tasa_no_resp_a = mean(resp_a == 0),
  tasa_no_resp_b = mean(resp_b == 0),
  sesgo_a = est_a - mean(Y),
  sesgo_b = est_b - mean(Y)
)


#############################
# 6.3 Error de muestreo vs n
#############################
set.seed(4)

N <- 2000
Y <- rnorm(N, mean = 10, sd = 3)
mu_pop <- mean(Y)

B <- 2000
ns <- c(20, 50, 100, 200)

res <- data.frame(n = ns, media_est = NA, sesgo = NA, sd_est = NA)

for(j in seq_along(ns)){
  n <- ns[j]
  est <- replicate(B, mean(Y[sample.int(N, n, replace = FALSE)]))
  res$media_est[j] <- mean(est)
  res$sesgo[j]     <- mean(est - mu_pop)
  res$sd_est[j]    <- sd(est)
}

res

############################################################
# FIN
############################################################
