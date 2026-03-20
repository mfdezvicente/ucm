  # Soluciones (Tema 1) - Hoja de ejercicios

## Funciones auxiliares

# Ordenado con reposición
muestras_ordenado_con_rep <- function(Omega, n){
  as.data.frame(expand.grid(rep(list(Omega), n), stringsAsFactors = FALSE))
}

# Ordenado sin reposición
muestras_ordenado_sin_rep <- function(Omega, n){
  df <- muestras_ordenado_con_rep(Omega, n)
  keep <- apply(df, 1, function(x) length(unique(x)) == length(x))
  df[keep, , drop=FALSE]
}

# No ordenado sin reposición
muestras_no_ordenado_sin_rep <- function(Omega, n){
  # combn devuelve una matriz con columnas = combinaciones; transponemos
  t(combn(Omega, n))
}

# No ordenado con reposición
muestras_no_ordenado_con_rep <- function(Omega, n){
  df <- muestras_ordenado_con_rep(Omega, n)
  sorted_rows <- t(apply(df, 1, sort))
  unique(sorted_rows)
}


## Implementación directa de las fórmulas en R

VN  <- function(N, n) factorial(N)/factorial(N-n)              # VN,n = N!/(N-n)!
CN  <- function(N, n) choose(N, n)                             # CN,n = N!/(n!(N-n)!)
VRN <- function(N, n) N^n                                      # VRN,n = N^n
CRN <- function(N, n) choose(N + n - 1, n)                     # CRN,n = (N+n-1)!/(n!(N-1)!)

PN  <- function(N) factorial(N)                                # PN = N!
PR  <- function(a_vec){                                        # PR^N_{a,b,c,...} = N!/(a!b!c!...), sum(a_vec)=N
  N <- sum(a_vec)
  factorial(N)/prod(factorial(a_vec))
}



# ------------------------------
# Ejercicio 1- V
# ------------------------------

Omega <- c("A","B","C","D","E"); N <- length(Omega); n <- 3

m_teor <- c(osr=VN(N,n), nosr=CN(N,n), ocr=VRN(N,n), nocr=CRN(N,n))
m_teor

m_emp <- c(
  osr  = nrow(muestras_ordenado_sin_rep(Omega,n)),
  nosr = nrow(muestras_no_ordenado_sin_rep(Omega,n)),
  ocr  = nrow(muestras_ordenado_con_rep(Omega,n)),
  nocr = nrow(muestras_no_ordenado_con_rep(Omega,n))
)
m_emp


# ------------------------------
# Ejercicio 2 -V
# ------------------------------

# a)
VRN(6,4)

# b) fórmula
choose(4,2) * (1/6)^2 * (5/6)^2

# (opcional) comprobación por enumeración
S <- muestras_ordenado_con_rep(1:6, 4)
mean(rowSums(S==6) == 2)


# ------------------------------
# Ejercicio 3 - V
# ------------------------------

# Ω = 0:9, n = 8
VRN(10,8)   # con repetición
VN(10,8)    # sin repetición


# ------------------------------
# Ejercicio 4
# ------------------------------

CN(30,5)


# ------------------------------
# Ejercicio 5
# ------------------------------

VN(30,5)



# ------------------------------
# Ejercicio 6
# ------------------------------

CN(12,4) * VN(4,2)


# ------------------------------
# Ejercicio 7
# ------------------------------

tab <- table(strsplit("MUESTREO","")[[1]])
tab
PR(as.integer(tab))


# ------------------------------
# Ejercicio 8
# ------------------------------

# a)
CN(6,3)

# c)
choose(3,2)*choose(2,1) / choose(6,3)

# (opcional) enumeración por bolas etiquetadas
Omega <- c("R1","R2","R3","A1","A2","V1")
S <- muestras_no_ordenado_sin_rep(Omega, 3)
mean(apply(S, 1, function(x) sum(grepl("^R", x))==2 && sum(grepl("^A", x))==1))


# ------------------------------
# Ejercicio 9
# ------------------------------

CN(5,2) * CN(4,2)


# ------------------------------
# Ejercicio 10
# ------------------------------

Omega <- 1:6; N <- length(Omega); n <- 2

S_ocr  <- muestras_ordenado_con_rep(Omega, n)
S_osr  <- muestras_ordenado_sin_rep(Omega, n)
S_nosr <- muestras_no_ordenado_sin_rep(Omega, n)
S_nocr <- muestras_no_ordenado_con_rep(Omega, n)

c(
  ocr  = nrow(S_ocr),
  osr  = nrow(S_osr),
  nosr = nrow(S_nosr),
  nocr = nrow(S_nocr)
)

c(
  ocr  = VRN(N,n),
  osr  = VN(N,n),
  nosr = CN(N,n),
  nocr = CRN(N,n)
)


# ------------------------------
# Ejercicio 11
# ------------------------------

Omega <- c("A","B","C","D"); n <- 2

# OCR
1 - (3/4)^2

# OSR
1 - VN(3,2)/VN(4,2)

# NOSR
CN(3,2) / CN(4,2)  # muestras SIN A
1 - CN(3,2) / CN(4,2)

# NOCR
1 - CRN(3,2)/CRN(4,2)


# ------------------------------
# Ejercicio 12
# ------------------------------

CN(9,2) / CN(10,3)


# ------------------------------
# Ejercicio 13
# ------------------------------

choose(12,1) * choose(38,4) / choose(50,5)


# ------------------------------
# Ejercicio 14
# ------------------------------

VN(6,4) / VRN(6,4)


# ------------------------------
# Ejercicio 15
# ------------------------------

CN(8,2) * (CN(10,3)^2)


# ------------------------------
# Ejercicio 16
# ------------------------------

set.seed(123)
N <- 10000
A <- rbinom(N, 1, 0.4)
Y <- rnorm(N, mean = 0 + 1.0*A, sd=1)

mu_pop <- mean(Y)

B <- 1000; n <- 200
est_conven <- numeric(B)
est_MAS    <- numeric(B)

for(b in 1:B){
  idx_conv <- sample(which(A==1), n, replace=FALSE)
  est_conven[b] <- mean(Y[idx_conv])

  idx_mas <- sample.int(N, n, replace=FALSE)
  est_MAS[b] <- mean(Y[idx_mas])
}

c(mu_pop=mu_pop,
  media_conven=mean(est_conven),
  media_MAS=mean(est_MAS),
  sesgo_conven=mean(est_conven-mu_pop),
  sesgo_MAS=mean(est_MAS-mu_pop))


# ------------------------------
# Ejercicio 17
# ------------------------------

set.seed(1)
N <- 5000
grupo <- rbinom(N, 1, 0.2)
Y <- rnorm(N, mean = 2*grupo, sd=1)

mu_pop <- mean(Y)

marco <- which(grupo==0)          # excluye grupo=1
n <- 200
idx <- sample(marco, n, replace=FALSE)
mu_muestra <- mean(Y[idx])

c(media_poblacion=mu_pop, media_estimacion=mu_muestra, sesgo=mu_muestra-mu_pop)


# ------------------------------
# Ejercicio 18
# ------------------------------

set.seed(2)
N <- 5000
Y <- rnorm(N)

n <- 400
idx <- sample.int(N, n, replace=FALSE)

# (a) No respuesta aleatoria
resp_a <- rbinom(n, 1, 0.8)
est_a <- mean(Y[idx][resp_a==1])

# (b) No respuesta dependiente de Y: valores altos responden menos
p_resp <- plogis(1 - 1.2*Y[idx])
resp_b <- rbinom(n, 1, p_resp)
est_b <- mean(Y[idx][resp_b==1])

c(media_real=mean(Y),
  est_noresp_aleatoria=est_a,
  est_noresp_dependiente=est_b)


# ------------------------------
# Ejercicio 19
# ------------------------------

N <- 2000
sigma <- 3
ns <- c(20, 50, 100, 200)

SE <- function(N, n, sigma) sigma/sqrt(n) * sqrt((N-n)/(N-1))

data.frame(n=ns, SE=sapply(ns, function(n) SE(N,n,sigma)))
# Valores esperados (aprox):
# n=20: SE≈0.668
# n=50: SE≈0.419
# n=100: SE≈0.292
# n=200: SE≈0.201


# ------------------------------
# Ejercicio 20
# ------------------------------

resumen_diseno <- function(Omega, n, tipo=c("ocr","osr","nosr","nocr")){
  tipo <- match.arg(tipo)
  N <- length(Omega)

  S <- switch(tipo,
    ocr  = muestras_ordenado_con_rep(Omega, n),
    osr  = muestras_ordenado_sin_rep(Omega, n),
    nosr = muestras_no_ordenado_sin_rep(Omega, n),
    nocr = muestras_no_ordenado_con_rep(Omega, n)
  )

  m_emp <- nrow(S)
  m_teor <- switch(tipo,
    ocr  = VRN(N,n),
    osr  = VN(N,n),
    nosr = CN(N,n),
    nocr = CRN(N,n)
  )

  if(m_emp != m_teor) stop("No coincide: empírico=", m_emp, " teórico=", m_teor)

  list(tipo=tipo, N=N, n=n, m=m_emp, espacio=S)
}

# prueba
Omega <- LETTERS[1:5]; n <- 3
lapply(c("ocr","osr","nosr","nocr"), function(t) resumen_diseno(Omega,n,t)$m)



# ------------------------------
# Ejercicio 17
# ------------------------------


N <- 10000
Y <- rnorm(N, mean = 0, sd = 1)
A <- rbinom(N, 1, 0.2)
n <- 1000 
mu_y <- mean(Y)

set.seed(1)
muestra <- which(A ==1)
idx <- sample(muestra,n, replace = FALSE)
mu_preguntado <- mean(Y[idx])

mu_preguntado


resp <- c(mu_y = mu_y, mu_preguntado = mu_preguntado, sesgo = mu_preguntado - mu_y)

# Esto sería en el caso de que no hubiera dependencia de Y 


