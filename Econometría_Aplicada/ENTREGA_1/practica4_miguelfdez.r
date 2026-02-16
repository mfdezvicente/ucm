##############
# EJERCICIO 4 #
##############

# Enunciado:
# Se quiere explicar el gasto en formación profesional (FORMA, miles €) en función de:
# - INGRE: ingresos (miles €)
# - TRABA: nº trabajadores
# - PRODU: índice de productividad
# Además, se conoce por un estudio previo que Var(u_i) es directamente proporcional a PRODU_i.

# ------------------------------------------------------------
# 1) Preparación y carga de datos
# ------------------------------------------------------------
rm(list = ls())

library(readxl)
library(lmtest)

datos <- read_excel("ucm/Econometría_Aplicada/Datos_pr4.xlsx")
# head(datos)
# str(datos)

# ------------------------------------------------------------
# 2) Estimación inicial por MCO
# ------------------------------------------------------------
# Modelo base:
# FORMA = beta0 + beta1*INGRE + beta2*PRODU + beta3*TRABA + u

modelo_mco <- lm(FORMA ~ INGRE + PRODU + TRABA, data = datos)
summary(modelo_mco)

# ------------------------------------------------------------
# 3) Comprobación de heterocedasticidad (BP)
# ------------------------------------------------------------
# H0: homocedasticidad
# H1: heterocedasticidad
bp_mco <- bptest(modelo_mco)
bp_mco

# ------------------------------------------------------------
# 4) Estimación por Mínimos Cuadrados Ponderados (WLS)
# ------------------------------------------------------------
# Según el enunciado: Var(u_i) ∝ PRODU_i
# -> pesos recomendados: w_i = 1 / PRODU_i

modelo_wls <- lm(FORMA ~ INGRE + PRODU + TRABA, data = datos, weights = 1/PRODU)
summary(modelo_wls)

# Repetimos BP como comprobación tras ponderar
bp_wls <- bptest(modelo_wls)
bp_wls
