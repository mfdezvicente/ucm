##############################################################
############      EJERCICI 3 - ECONOMETRÍA   #################
############      MIGUEL FERNÁNDEZ VICENTE   #################
##############################################################

# Enunciado:
# Se desea conocer qué factores influyen en el precio de una casa utilizando el fichero
# hprice1 (Wooldridge).

# ------------------------------------------------------------
# 1) Preparación y carga de datos
# ------------------------------------------------------------
rm(list = ls())

library(readxl)
library(car)      # vif
library(olsrr)    # ols_coll_diag
library(tseries)  # Jarque-Bera

datos <- read_excel("ucm/Econometría_Aplicada/Datos Hprice1.xls")
# head(datos)
# str(datos)

# ------------------------------------------------------------
# 2) Especificación y estimación inicial (MCO)
# ------------------------------------------------------------
# Modelo base (con las variables que estás usando):
# Price = beta0 + beta1*Assess + beta2*Bdrms + beta3*Lotsize + beta4*sqrft + beta5*colonial + u

modelo_1 <- lm(Price ~ Assess + Bdrms + Lotsize + sqrft + colonial, data = datos)
summary(modelo_1)

# ------------------------------------------------------------
# 3) Multicolinealidad (diagnóstico)
# ------------------------------------------------------------
# Regla práctica:
# - VIF entre 4 y 10: posible sospecha
# - VIF > 10: multicolinealidad fuerte

vif(modelo_1)

# Diagnóstico adicional (incluye índice de condición, etc.)
ols_coll_diag(modelo_1)

# ------------------------------------------------------------
# 4) Simplificación del modelo (manteniendo tu criterio)
# ------------------------------------------------------------
# Quitamos variables poco significativas (según p-valores) y nos quedamos con un modelo más parsimonioso.

modelo_2 <- lm(Price ~ Bdrms + Lotsize + sqrft + colonial, data = datos)
summary(modelo_2)

# Quitamos la variable con mayor p-valor (según tu estrategia):
modelo_3 <- lm(Price ~ Bdrms + Lotsize + sqrft, data = datos)
summary(modelo_3)

# OJO: aquí tú decides estimar SIN intercepto (+0). Lo dejo porque es tu enfoque,
# pero normalmente no es obligatorio. Simplemente cambia la interpretación.
modelo_4 <- lm(Price ~ Bdrms + Lotsize + sqrft + 0, data = datos)
summary(modelo_4)

# ------------------------------------------------------------
# 5) Validación: normalidad de residuos + QQ-plot
# ------------------------------------------------------------
residuos_4 <- residuals(modelo_4)

jarque.bera.test(residuos_4)

qqnorm(residuos_4, main = "QQ-plot residuos (modelo 4)")
qqline(residuos_4, col = "red")

# ------------------------------------------------------------
# 6) Búsqueda de observaciones muy influyentes / atípicas (según residuos)
# ------------------------------------------------------------
# Ordenamos los residuos en valor absoluto, de mayor a menor
res_ordenados4 <- sort(abs(residuos_4), decreasing = TRUE)
head(res_ordenados4)

# Creamos dummies para observaciones concretas (las que tú has señalado)
datos$o76 <- as.integer(seq_len(nrow(datos)) == 76)
datos$o42 <- as.integer(seq_len(nrow(datos)) == 42)
datos$o73 <- as.integer(seq_len(nrow(datos)) == 73)

# Reestimamos incluyendo esas dummies (capturan “saltos” específicos)
modelo_5 <- lm(Price ~ Bdrms + Lotsize + sqrft + 0 + o76 + o42 + o73, data = datos)
summary(modelo_5)

# Validación de residuos del modelo final según tu criterio
residuos_5 <- residuals(modelo_5)

jarque.bera.test(residuos_5)

qqnorm(residuos_5, main = "QQ-plot residuos (modelo 5)")
qqline(residuos_5, col = "red")
