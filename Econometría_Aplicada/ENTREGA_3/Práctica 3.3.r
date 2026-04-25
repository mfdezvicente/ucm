##############################################################
############   ENTREGA 3 - PRÁCTICA 3.3 (APPLE) ##############
##############################################################
# Objetivo:
# Estudiar la demanda individual de manzanas ecológicas (ecolbs)
# y analizar el papel de precios, ingresos y tamaño del hogar.
#
# Dado que muchos hogares consumen 0 libras de manzana ecológica,
# se compara:
#   1) un modelo lineal por MCO
#   2) un modelo Tobit con censura a la izquierda en 0
##############################################################

# ------------------------------------------------------------
# 0) Preparación
# ------------------------------------------------------------
rm(list = ls())

library(readxl)
library(AER)   # tobit

# ------------------------------------------------------------
# 1) Lectura de datos
# ------------------------------------------------------------
datos <- read_excel("ucm/Econometría_Aplicada/ENTREGA_3/Práctica 3_3 Datos apple.xls")

# Vista rápida
# head(datos)
# str(datos)

# ------------------------------------------------------------
# 2) Análisis exploratorio
# ------------------------------------------------------------
# Distribución de la variable dependiente
table(datos$ecolbs)

# Histograma
hist(datos$ecolbs,
     main = "Histograma de ecolbs",
     xlab = "ecolbs")

# Número de observaciones y número de ceros
n <- nrow(datos)
n_ceros <- sum(datos$ecolbs == 0)
prop_ceros <- mean(datos$ecolbs == 0)

n
n_ceros
prop_ceros

# ------------------------------------------------------------
# 3) Modelo lineal inicial (MCO)
# ------------------------------------------------------------
# Especificación:
# ecolbs = beta0 + beta1*ecoprc + beta2*regprc + beta3*faminc + beta4*hhsize + u

modelo_mco <- lm(ecolbs ~ ecoprc + regprc + faminc + hhsize, data = datos)
summary(modelo_mco)

# Guardamos coeficientes MCO
beta_mco <- coef(modelo_mco)
beta_mco

# ------------------------------------------------------------
# 4) Modelo Tobit
# ------------------------------------------------------------
# Motivación:
# ecolbs está censurada a la izquierda en 0 porque muchos hogares no compran.
# Entonces se estima:
# ecolbs = max(0, ecolbs*)
# ecolbs* = Xb + u

modelo_tobit <- tobit(ecolbs ~ ecoprc + regprc + faminc + hhsize,
                      left = 0,
                      data = datos)

summary(modelo_tobit)

# Parámetros Tobit
beta_tobit <- coef(modelo_tobit)
sigma <- modelo_tobit$scale

beta_tobit
sigma

# ------------------------------------------------------------
# 5) Efectos marginales del Tobit sobre E[y|x]
# ------------------------------------------------------------
# En el Tobit, una aproximación habitual para el efecto marginal sobre el valor esperado observado es:
# ME_j(x) = Phi(xb/sigma) * beta_j

X <- model.matrix(modelo_tobit)
xb <- as.vector(X %*% beta_tobit)
Phi_x <- pnorm(xb / sigma)

# Efectos marginales individuales
datos$me_ecoprc <- Phi_x * beta_tobit["ecoprc"]
datos$me_regprc <- Phi_x * beta_tobit["regprc"]
datos$me_faminc <- Phi_x * beta_tobit["faminc"]
datos$me_hhsize <- Phi_x * beta_tobit["hhsize"]

# Efectos marginales medios
mean(datos$me_ecoprc)
mean(datos$me_regprc)
mean(datos$me_faminc)
mean(datos$me_hhsize)

# ------------------------------------------------------------
# 6) Efectos marginales en el punto medio
# ------------------------------------------------------------
newdata <- data.frame(
  ecoprc = mean(datos$ecoprc, na.rm = TRUE),
  regprc = mean(datos$regprc, na.rm = TRUE),
  faminc = mean(datos$faminc, na.rm = TRUE),
  hhsize = mean(datos$hhsize, na.rm = TRUE)
)

newdata

# Índice lineal en el punto medio
xb_medio <- predict(modelo_tobit, newdata = newdata, type = "link")
xb_medio

Phi_medio <- pnorm(xb_medio / sigma)
Phi_medio

# Efectos marginales en el punto medio
# OJO: quitamos el intercepto porque no tiene interpretación económica como efecto marginal
marginales_medio <- Phi_medio * beta_tobit[c("ecoprc", "regprc", "faminc", "hhsize")]
marginales_medio

# ------------------------------------------------------------
# 7) Comparación rápida MCO vs Tobit
# ------------------------------------------------------------
comparacion <- rbind(
  MCO = beta_mco[c("ecoprc", "regprc", "faminc", "hhsize")],
  Tobit = beta_tobit[c("ecoprc", "regprc", "faminc", "hhsize")]
)

comparacion