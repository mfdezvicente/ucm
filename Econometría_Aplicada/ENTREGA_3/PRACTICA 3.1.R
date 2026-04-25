##############################################################
############   ENTREGA 3 - PRÁCTICA 3.1 (mrozv1) ##############
##############################################################
# Objetivo:
# Estudiar cómo influye la maternidad en la inserción en el mercado laboral
# de mujeres casadas, usando la variable binaria:
# inlf = 1 si trabaja fuera del hogar, 0 en caso contrario.
#
# Variables (según enunciado):
# - kidslt6: nº hijos < 6 años
# - kidsge6 (o kidsage6): nº hijos entre 6 y 18 años
# - age, educ, exper, expersq
# - nwifeinc: ingresos del marido
##############################################################

# ------------------------------------------------------------
# 0) Preparación
# ------------------------------------------------------------
rm(list = ls())

library(readxl)
library(lmtest)        # coeftest
library(sandwich)      # vcovHC
library(marginaleffects)  # efectos marginales

# ------------------------------------------------------------
# 1) Lectura de datos
# ------------------------------------------------------------
datos <- read_excel("C:/Users/Usuario/GitHub/ucm/Econometría_Aplicada/ENTREGA_3/Práctica 3_1 Datos mrozv1.xls")

# Vista rápida (opcional)
# head(datos)
# str(datos)

# Si la variable de hijos mayores aparece como kidsage6 en tu Excel, descomenta:
# names(datos)[names(datos) == "kidsage6"] <- "kidsge6"

# ------------------------------------------------------------
# 2) Modelo de Probabilidad Lineal (MPL / LPM)
# ------------------------------------------------------------
# OJO: el LPM puede dar probabilidades fuera de [0,1] y suele tener heterocedasticidad.
modelo_mpl <- lm(inlf ~ kidslt6 + kidsge6 + age + educ + exper + nwifeinc + expersq, data = datos)
summary(modelo_mpl)

# Predicción LPM (puede salir fuera de rango)
datos$inlfhat_lpm <- predict(modelo_mpl)

# Observaciones fuera de rango
fuera_rango <- subset(datos, inlfhat_lpm < 0 | inlfhat_lpm > 1)
nrow(fuera_rango)  # cuántas hay
# fuera_rango       # si quieres verlas

# Inferencia robusta (recomendable en LPM)
coeftest(modelo_mpl, vcov = vcovHC(modelo_mpl, type = "HC1"))

# ------------------------------------------------------------
# 3) Logit
# ------------------------------------------------------------
modelo_logit <- glm(
  inlf ~ kidslt6 + kidsge6 + age + educ + exper + nwifeinc + expersq,
  family = binomial(link = "logit"),
  data = datos
)
summary(modelo_logit)

# Probabilidades predichas (SIEMPRE en [0,1])
datos$phat_logit <- predict(modelo_logit, type = "response")

# ------------------------------------------------------------
# 4) Probit
# ------------------------------------------------------------
modelo_probit <- glm(
  inlf ~ kidslt6 + kidsge6 + age + educ + exper + nwifeinc + expersq,
  family = binomial(link = "probit"),
  data = datos
)
summary(modelo_probit)

datos$phat_probit <- predict(modelo_probit, type = "response")

# ------------------------------------------------------------
# 5) Efectos marginales (en el punto medio de las covariables)
# ------------------------------------------------------------
# Esto te devuelve dP/dX (efecto marginal sobre la probabilidad) evaluado en medias.
# IMPORTANTE: en tu código ponías "nwifein" en lugar de "nwifeinc" -> lo corrigo.
slopes(
  modelo_logit,
  newdata = datagrid(
    kidslt6  = mean(datos$kidslt6, na.rm = TRUE),
    kidsge6  = mean(datos$kidsge6, na.rm = TRUE),
    age      = mean(datos$age, na.rm = TRUE),
    educ     = mean(datos$educ, na.rm = TRUE),
    exper    = mean(datos$exper, na.rm = TRUE),
    nwifeinc = mean(datos$nwifeinc, na.rm = TRUE),
    expersq  = mean(datos$expersq, na.rm = TRUE)
  )
)

slopes(
  modelo_probit,
  newdata = datagrid(
    kidslt6  = mean(datos$kidslt6, na.rm = TRUE),
    kidsge6  = mean(datos$kidsge6, na.rm = TRUE),
    age      = mean(datos$age, na.rm = TRUE),
    educ     = mean(datos$educ, na.rm = TRUE),
    exper    = mean(datos$exper, na.rm = TRUE),
    nwifeinc = mean(datos$nwifeinc, na.rm = TRUE),
    expersq  = mean(datos$expersq, na.rm = TRUE)
  )
)

# ------------------------------------------------------------
# 6) Predicción comparativa: variar nº de hijos <6 manteniendo medias
# ------------------------------------------------------------
nuevos_datos <- data.frame(
  kidslt6  = c(0, 1, 2, 3),
  kidsge6  = rep(mean(datos$kidsge6, na.rm = TRUE), 4),
  age      = rep(mean(datos$age, na.rm = TRUE), 4),
  educ     = rep(mean(datos$educ, na.rm = TRUE), 4),
  exper    = rep(mean(datos$exper, na.rm = TRUE), 4),
  nwifeinc = rep(mean(datos$nwifeinc, na.rm = TRUE), 4),
  expersq  = rep(mean(datos$expersq, na.rm = TRUE), 4)
)

pred_logit  <- predict(modelo_logit,  newdata = nuevos_datos, type = "response")
pred_probit <- predict(modelo_probit, newdata = nuevos_datos, type = "response")

pred_logit
diff(pred_logit)      # cambio marginal al pasar de 0->1->2->3 hijos pequeños

pred_probit
diff(pred_probit)

# ------------------------------------------------------------
# 7) Clasificación (umbral 0.5) + matriz de confusión
# ------------------------------------------------------------
# (Este bloque corrige el error que te salía: antes no existía inlfhat_logit2)
datos$yhat_logit  <- ifelse(datos$phat_logit  >= 0.5, 1, 0)
datos$yhat_probit <- ifelse(datos$phat_probit >= 0.5, 1, 0)

# Matrices de confusión
mconf_logit  <- table(Real = datos$inlf, Predicho = datos$yhat_logit)
mconf_probit <- table(Real = datos$inlf, Predicho = datos$yhat_probit)

mconf_logit
mconf_probit

# Exactitud (accuracy)
acc_logit  <- mean(datos$yhat_logit  == datos$inlf, na.rm = TRUE)
acc_probit <- mean(datos$yhat_probit == datos$inlf, na.rm = TRUE)

acc_logit
acc_probit

# (Opcional) tasa de acierto por clase
# Sensibilidad = P(pred=1 | real=1), Especificidad = P(pred=0 | real=0)
sens_logit <- mconf_logit["1","1"] / sum(mconf_logit["1",])
spec_logit <- mconf_logit["0","0"] / sum(mconf_logit["0",])

sens_probit <- mconf_probit["1","1"] / sum(mconf_probit["1",])
spec_probit <- mconf_probit["0","0"] / sum(mconf_probit["0",])

sens_logit; spec_logit
sens_probit; spec_probit