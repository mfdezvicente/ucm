##############################################################
############      EJERCICIO 1 - ECONOMETRÍA   #################
############      MIGUEL FERNÁNDEZ VICENTE   #################
##############################################################


# Enunciado:
# Se ha recopilado información salarial de 40 empleados del sector bancario:
# - Salario (Y, euros mensuales)
# - Experiencia laboral (X, años)
# Estimar un modelo que explique el salario en función de la experiencia.

# ------------------------------------------------------------
# 1) Preparación
# ------------------------------------------------------------
rm(list = ls())

library(readxl)
library(tseries)

# Carga de datos
df <- read_excel("/Users/miguelfernandez/GitHub/ucm/Econometría_Aplicada/Pr1_Datos.xlsx")

# Echa un vistazo rápido a las variables
# head(df)
# str(df)

# IMPORTANTE:
# En lm estabas usando y y x. Esto solo funciona si el Excel tiene columnas llamadas exactamente "y" y "x".
# Si en el archivo se llaman "Salario" y "Experiencia", habría que cambiarlo a:
# lm(Salario ~ Experiencia, data = df)

# ------------------------------------------------------------
# 2) Especificación y estimación (MCO)
# ------------------------------------------------------------
# Modelo: y = beta0 + beta1 * x + u

regresion <- lm(y ~ x, data = df)
summary(regresion)

# Coeficientes estimados
coeficientes <- coef(regresion)
beta0 <- coeficientes[1]  # Intercepto
beta1 <- coeficientes[2]  # Pendiente

beta0
beta1

# Interpretación rápida (en texto, para ti):
# - beta0: salario estimado cuando x = 0 años de experiencia.
# - beta1: aumento medio del salario por cada año adicional de experiencia.

# ------------------------------------------------------------
# 3) Validación básica: normalidad de residuos (Jarque-Bera)
# ------------------------------------------------------------
# H0: residuos ~ Normal
# Si p-valor < 0.05, rechazamos normalidad

jb <- jarque.bera.test(residuals(regresion))
jb

# ------------------------------------------------------------
# 4) Representación gráfica: nube de puntos + recta estimada
# ------------------------------------------------------------
plot(df$x, df$y,
     main = "Salario vs Experiencia (nube de puntos)",
     xlab = "Años de experiencia (X)",
     ylab = "Salario mensual (Y)",
     col = "red")

abline(regresion, col = "blue", lwd = 2)
