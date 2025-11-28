## ESTIMADOR KERNEL DE DENSIDAD, DISTRIBUCIÓN, MSE Y MISE ----------------------

## 1. Función densidad kernel
kernel.densidad <- function(datos, puntos, h, kernel){
  ndatos <- length(datos)
  # Matriz con (puntos - datos)/h
  dif <- outer(puntos, datos, function(x, y) (x - y) / h)
  matk <- kernel(dif)              # Aplico el kernel elemento a elemento
  f_est <- rowMeans(matk) / h      # (1 / (n h)) * sum_j K((x - X_j)/h)
  return(f_est)
}

## 2. Función distribución (CDF) kernel con kernel integrado Kint
kernel.distribucion <- function(datos, puntos, h, Kint){
  ndatos <- length(datos)
  dif <- outer(puntos, datos, function(x, y) (x - y) / h)
  matk <- Kint(dif)                # Kint es la CDF del kernel (p.ej. pnorm)
  F_est <- rowMeans(matk)          # (1 / n) * sum_j Kint((x - X_j)/h)
  return(F_est)
}

## 3. Ejemplo: estimación de densidad ---------------------------
set.seed(1)
n <- 20
muestra <- rnorm(n)

kernelg <- function(x){
  dnorm(x)
}

h <- 0.25
puntos <- seq(-3, 3, length = 1000)

f_est <- kernel.densidad(muestra, puntos, h, kernelg)

# Histograma + densidad estimada
hist(muestra, freq = FALSE, main = "Densidad kernel vs histograma")
lines(puntos, f_est)

# Comparación con densidad teórica
f_teorica <- dnorm(puntos)
plot(puntos, f_teorica, type = "l", col = "red", ylim = c(0, 0.5),
     main = "Densidad teórica (rojo) vs kernel (negro)")
lines(puntos, f_est)

## 4. Ejemplo: estimación de distribución ----------------------
set.seed(1)
n <- 20
muestra <- rnorm(n)

Kerneldfg <- function(x){
  pnorm(x)   # kernel integrado del normal estándar
}

# Regla de Silverman: h = 1.06 * sd * n^(-1/5)
h <- 1.06 * sd(muestra) * n^(-1/5)
puntos <- seq(-3, 3, length = 1000)

F_est <- kernel.distribucion(muestra, puntos, h, Kerneldfg)

# Comparación con la empírica
plot(ecdf(muestra), main = "ECDF (escalones) vs CDF kernel (línea)")
lines(puntos, F_est)

# Comparación con la CDF teórica
F_teorica <- pnorm(puntos)
plot(puntos, F_teorica, type = "l", col = "red",
     main = "CDF teórica (rojo) vs CDF kernel (negro)")
lines(puntos, F_est)

## 5. MSE y MISE por simulación para la densidad ---------------
set.seed(1)

kernelg <- function(x){
  dnorm(x)
}

n <- 20
MC <- 100
puntos <- seq(-6, 6, length = 100)
f_teorica <- dnorm(puntos)

error <- matrix(NA, nrow = MC, ncol = length(puntos))

for (t in 1:MC){
  muestra <- rnorm(n)
  h <- 1.06 * sd(muestra) * n^(-1/5)
  f_est <- kernel.densidad(muestra, puntos, h, kernelg)
  error[t, ] <- (f_est - f_teorica)^2
}

MSE <- colMeans(error)

plot(puntos, MSE, type = "l",
     main = "MSE del estimador de densidad kernel",
     ylab = "MSE", xlab = "x")

# Aproximación numérica de la MISE
MISE <- sum(MSE) * (puntos[2] - puntos[1])
MISE
