##############################################################
############      PRÁCTICA 2 - ECONOMETRÍA   #################
############      MIGUEL FERNÁNDEZ VICENTE   #################
##############################################################

# EJERCICIO 2
# Enunciado:
# El fichero Bordeaux.csv contiene datos de subastas de vinos tintos de reserva de Burdeos
# (añadas 1952-1980). Variables:
# pr (precio), lluvinv (lluvia invierno), tempmed (temperatura media maduración),
# lluvcos (lluvia durante maduración), edad (años desde la cosecha).
# Se pide estudiar la rentabilidad de conservar el vino.

# ------------------------------------------------------------
# 1) Preparación y carga de datos
# ------------------------------------------------------------
rm(list = ls())

library(tseries)

df <- read.csv(
    "C:/Users/Usuario/GitHub/ucm/Econometría_Aplicada/ENTREGA_1/Pr2_Datos Bordeaux.csv",
  sep = ";",
  dec = ",",
  na.strings = "NA"
)

# Vista rápida
# head(df)
# str(df)

# ------------------------------------------------------------
# 2) Especificación y estimación (MCO)
# ------------------------------------------------------------
# Usamos log(pr) para interpretar el efecto de la edad como crecimiento porcentual aproximado.
df$lpr <- log(df$pr)

# Modelo: ln(pr) = beta0 + beta1*lluvinv + beta2*tempmed + beta3*lluvcos + beta4*edad + u
modelo_mco <- lm(lpr ~ lluvinv + tempmed + lluvcos + edad, data = df)
summary(modelo_mco)

# Interpretación de la rentabilidad asociada a "edad":
beta_edad <- coef(modelo_mco)["edad"]
beta_edad

# Aproximación: 100 * beta_edad (% por año)
cat("Rentabilidad aproximada por año (%):", round(100 * beta_edad, 3), "\n")

# Interpretación más exacta en porcentaje: (exp(beta)-1)*100
cat("Rentabilidad anual más exacta (%):", round((exp(beta_edad) - 1) * 100, 3), "\n")

# ------------------------------------------------------------
# 3) Validación básica: normalidad de residuos (Jarque-Bera)
# ------------------------------------------------------------
jb <- jarque.bera.test(residuals(modelo_mco))
jb

# ------------------------------------------------------------
# 4) Predicción (como en clase)
# ------------------------------------------------------------
# Creamos unos nuevos datos (ejemplo) para predecir el precio del vino
nuevos_datos <- data.frame(
  lluvinv = c(605, 600),
  tempmed = c(16.5, 17.0),
  lluvcos = c(135, 134),
  edad    = c(20, 22)
)

# Predicción puntual de ln(precio)
pred_log <- predict(modelo_mco, newdata = nuevos_datos)
pred_log

# Volvemos a nivel de precio (porque el modelo está en log)
pred_precio <- exp(pred_log)
pred_precio

# Intervalo de confianza (en log) y transformado a precio
ic_log <- predict(modelo_mco, newdata = nuevos_datos, interval = "confidence", level = 0.95)
ic_log

ic_precio <- exp(ic_log)
ic_precio
