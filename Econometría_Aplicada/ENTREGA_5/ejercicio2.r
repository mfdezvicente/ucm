##############################################################
############   PRÁCTICA 6.2 - MODELOS DINÁMICOS  #############
############      MIGUEL FERNÁNDEZ VICENTE      #############
##############################################################

# Objetivo:
# Analizar los costes de una muestra de 6 aerolíneas comerciales
# observadas durante 15 años.
#
# Variables:
# aerolinea   = identificador de la aerolínea
# anio        = año de referencia
# Coste       = coste de la aerolínea, en miles de dólares
# Produccion  = producción, ingresos por milla por pasajero
# PrCombust   = precio del combustible
# Carga       = factor de carga, utilización promedio de la capacidad
#
# Estrategia:
# 1) Lectura y estructura del panel
# 2) Transformación logarítmica
# 3) Análisis descriptivo
# 4) Modelo estático: pooled, efectos fijos y efectos aleatorios
# 5) Contrastes de panel
# 6) Modelo dinámico con coste retardado
# 7) Primeras diferencias con variable instrumental
# 8) Arellano-Bond
# 9) Comparación final de resultados

# ------------------------------------------------------------
# 0) Preparación
# ------------------------------------------------------------

rm(list = ls())

library(readxl)
library(dplyr)
library(plm)
library(lmtest)
library(sandwich)
library(AER)

# ------------------------------------------------------------
# 1) Lectura de datos
# ------------------------------------------------------------

datos <- read_excel(
  "C:/Users/Usuario/GitHub/ucm/Econometría_Aplicada/ENTREGA_4/aerolineas.xlsx"
)

# Vista inicial
head(datos)
str(datos)
summary(datos)
names(datos)

# Si por algún motivo la columna aparece con tilde, la renombramos
if ("aerolínea" %in% names(datos)) {
  names(datos)[names(datos) == "aerolínea"] <- "aerolinea"
}

# ------------------------------------------------------------
# 2) Estructura del panel
# ------------------------------------------------------------

n_aerolineas <- length(unique(datos$aerolinea))
n_periodos <- length(unique(datos$anio))
n_total <- nrow(datos)

n_aerolineas
n_periodos
n_total

table(datos$anio)
summary(as.numeric(table(datos$aerolinea)))

# Ordenamos por aerolínea y año
datos <- datos %>%
  arrange(aerolinea, anio)

# Convertimos a formato panel
pdata <- pdata.frame(datos, index = c("aerolinea", "anio"))

# Comprobación del panel
pdim(pdata)

# ------------------------------------------------------------
# 3) Transformación logarítmica
# ------------------------------------------------------------

# Trabajamos con logaritmos para Coste, Produccion y PrCombust.
# La variable Carga se mantiene en nivel porque es un factor entre 0 y 1.

datos <- datos %>%
  mutate(
    lCoste = log(Coste),
    lProduccion = log(Produccion),
    lPrCombust = log(PrCombust)
  )

# Actualizamos el objeto panel con las nuevas variables
pdata <- pdata.frame(datos, index = c("aerolinea", "anio"))

# ------------------------------------------------------------
# 4) Análisis descriptivo básico
# ------------------------------------------------------------

summary(datos[, c("Coste", "Produccion", "PrCombust", "Carga")])
summary(datos[, c("lCoste", "lProduccion", "lPrCombust", "Carga")])

cor(
  datos[, c("lCoste", "lProduccion", "lPrCombust", "Carga")],
  use = "complete.obs"
)

# Medias por año
medias_anio <- datos %>%
  group_by(anio) %>%
  summarise(
    Coste_medio = mean(Coste, na.rm = TRUE),
    Produccion_media = mean(Produccion, na.rm = TRUE),
    PrCombust_medio = mean(PrCombust, na.rm = TRUE),
    Carga_media = mean(Carga, na.rm = TRUE),
    lCoste_medio = mean(lCoste, na.rm = TRUE)
  )

medias_anio

# Medias por aerolínea
medias_aerolinea <- datos %>%
  group_by(aerolinea) %>%
  summarise(
    Coste_medio = mean(Coste, na.rm = TRUE),
    Produccion_media = mean(Produccion, na.rm = TRUE),
    PrCombust_medio = mean(PrCombust, na.rm = TRUE),
    Carga_media = mean(Carga, na.rm = TRUE),
    lCoste_medio = mean(lCoste, na.rm = TRUE)
  )

medias_aerolinea

# Gráficos exploratorios
plot(datos$lProduccion, datos$lCoste,
     main = "Coste y producción",
     xlab = "log(Producción)",
     ylab = "log(Coste)",
     pch = 19, col = "blue")

plot(datos$lPrCombust, datos$lCoste,
     main = "Coste y precio del combustible",
     xlab = "log(Precio combustible)",
     ylab = "log(Coste)",
     pch = 19, col = "red")

plot(datos$Carga, datos$lCoste,
     main = "Coste y factor de carga",
     xlab = "Carga",
     ylab = "log(Coste)",
     pch = 19, col = "darkgreen")

# Evolución media del coste por año
plot(medias_anio$anio, medias_anio$lCoste_medio,
     type = "b",
     main = "Evolución media del coste",
     xlab = "Año",
     ylab = "log(Coste medio)",
     pch = 19)

# ------------------------------------------------------------
# 5) Modelo estático de panel
# ------------------------------------------------------------
# Modelo base:
# lCoste_it = beta0 + beta1*lProduccion_it
#             + beta2*lPrCombust_it
#             + beta3*Carga_it
#             + alpha_i + u_it

# ------------------------------------------------------------
# 5.1) Modelo pooled
# ------------------------------------------------------------

modelo_pool <- plm(
  lCoste ~ lProduccion + lPrCombust + Carga,
  data = pdata,
  model = "pooling"
)

summary(modelo_pool)

# ------------------------------------------------------------
# 5.2) Modelo de efectos fijos individuales
# ------------------------------------------------------------

modelo_fe <- plm(
  lCoste ~ lProduccion + lPrCombust + Carga,
  data = pdata,
  model = "within",
  effect = "individual"
)

summary(modelo_fe)

# Errores robustos agrupados por aerolínea
coeftest(
  modelo_fe,
  vcov = vcovHC(modelo_fe, type = "HC1", cluster = "group")
)

# ------------------------------------------------------------
# 5.3) Modelo de efectos fijos individuales y temporales
# ------------------------------------------------------------

modelo_fe_tw <- plm(
  lCoste ~ lProduccion + lPrCombust + Carga,
  data = pdata,
  model = "within",
  effect = "twoways"
)

summary(modelo_fe_tw)

coeftest(
  modelo_fe_tw,
  vcov = vcovHC(modelo_fe_tw, type = "HC1", cluster = "group")
)

# ------------------------------------------------------------
# 5.4) Modelo de efectos aleatorios
# ------------------------------------------------------------

modelo_re <- plm(
  lCoste ~ lProduccion + lPrCombust + Carga,
  data = pdata,
  model = "random",
  effect = "individual"
)

summary(modelo_re)

# ------------------------------------------------------------
# 6) Contrastes entre modelos estáticos
# ------------------------------------------------------------

# FE vs pooled
# H0: no hay efectos individuales
# H1: hay efectos individuales
pFtest(modelo_fe, modelo_pool)

# Breusch-Pagan LM: RE vs pooled
# H0: no hay efectos aleatorios
# H1: hay efectos aleatorios
plmtest(modelo_pool, type = "bp")

# Hausman: FE vs RE
# H0: RE es consistente
# H1: RE no es consistente; preferimos FE
phtest(modelo_fe, modelo_re)

# Autocorrelación serial en panel
pbgtest(modelo_fe)

# Dependencia contemporánea entre aerolíneas
pcdtest(modelo_fe, test = "cd")

# ------------------------------------------------------------
# 7) Modelo dinámico con coste retardado
# ------------------------------------------------------------
# Modelo:
# lCoste_it = gamma*lCoste_i,t-1
#             + beta1*lProduccion_it
#             + beta2*lPrCombust_it
#             + beta3*Carga_it
#             + alpha_i + u_it

# ------------------------------------------------------------
# 7.1) Modelo dinámico pooled
# ------------------------------------------------------------

modelo_dyn_pool <- plm(
  lCoste ~ lag(lCoste, 1) + lProduccion + lPrCombust + Carga,
  data = pdata,
  model = "pooling"
)

summary(modelo_dyn_pool)

# ------------------------------------------------------------
# 7.2) Modelo dinámico con efectos fijos
# ------------------------------------------------------------

modelo_dyn_fe <- plm(
  lCoste ~ lag(lCoste, 1) + lProduccion + lPrCombust + Carga,
  data = pdata,
  model = "within",
  effect = "individual"
)

summary(modelo_dyn_fe)

coeftest(
  modelo_dyn_fe,
  vcov = vcovHC(modelo_dyn_fe, type = "HC1", cluster = "group")
)

# ------------------------------------------------------------
# 8) Primeras diferencias + variable instrumental
# ------------------------------------------------------------
# Al diferenciar eliminamos el efecto fijo individual:
#
# d_lCoste_it = gamma*d_lCoste_i,t-1
#               + beta1*d_lProduccion_it
#               + beta2*d_lPrCombust_it
#               + beta3*d_Carga_it
#               + d_u_it
#
# Como d_lCoste_i,t-1 puede ser endógeno, usamos lCoste_i,t-2
# como instrumento.

datos_diff <- datos %>%
  arrange(aerolinea, anio) %>%
  group_by(aerolinea) %>%
  mutate(
    L1_lCoste = dplyr::lag(lCoste, 1),
    L2_lCoste = dplyr::lag(lCoste, 2),

    d_lCoste = lCoste - dplyr::lag(lCoste, 1),
    d_L1_lCoste = dplyr::lag(lCoste, 1) - dplyr::lag(lCoste, 2),

    d_lProduccion = lProduccion - dplyr::lag(lProduccion, 1),
    d_lPrCombust = lPrCombust - dplyr::lag(lPrCombust, 1),
    d_Carga = Carga - dplyr::lag(Carga, 1)
  ) %>%
  ungroup()

datos_iv <- datos_diff %>%
  filter(
    !is.na(d_lCoste),
    !is.na(d_L1_lCoste),
    !is.na(L2_lCoste),
    !is.na(d_lProduccion),
    !is.na(d_lPrCombust),
    !is.na(d_Carga)
  )

# Con 6 aerolíneas y 15 años, al perder dos retardos deberían quedar:
# 6 * 13 = 78 observaciones
nrow(datos_iv)
head(datos_iv)

# ------------------------------------------------------------
# 8.1) Primera etapa: relevancia del instrumento
# ------------------------------------------------------------

primera_etapa <- lm(
  d_L1_lCoste ~ L2_lCoste + d_lProduccion + d_lPrCombust + d_Carga,
  data = datos_iv
)

summary(primera_etapa)

# ------------------------------------------------------------
# 8.2) Estimación IV en primeras diferencias
# ------------------------------------------------------------

modelo_iv_diff <- ivreg(
  d_lCoste ~ d_L1_lCoste + d_lProduccion + d_lPrCombust + d_Carga |
    L2_lCoste + d_lProduccion + d_lPrCombust + d_Carga,
  data = datos_iv
)

summary(modelo_iv_diff, diagnostics = TRUE)

coeftest(
  modelo_iv_diff,
  vcov = vcovHC(modelo_iv_diff, type = "HC1")
)

# ------------------------------------------------------------
# 9) Arellano-Bond / GMM dinámico
# ------------------------------------------------------------
# Estimación dinámica con instrumentos internos.
# Con N = 6, hay que tener cuidado: el número de aerolíneas es pequeño.
# Por eso usamos pocos retardos y collapse = TRUE.

modelo_ab <- pgmm(
  lCoste ~ lag(lCoste, 1) + lProduccion + lPrCombust + Carga |
    lag(lCoste, 2:3) + lProduccion + lPrCombust + Carga,
  data = pdata,
  effect = "individual",
  model = "twosteps",
  transformation = "d",
  collapse = TRUE
)

summary(modelo_ab, robust = TRUE)

# AR(1): normalmente se rechaza en diferencias
mtest(modelo_ab, order = 1)

# AR(2): no debería rechazarse
mtest(modelo_ab, order = 2)

# Test de Sargan/Hansen
sargan(modelo_ab)

# ------------------------------------------------------------
# 10) Comparación final de coeficientes
# ------------------------------------------------------------

coef(modelo_fe)
coef(modelo_dyn_pool)
coef(modelo_dyn_fe)
coef(modelo_iv_diff)
coef(modelo_ab)

coef_resumen <- data.frame(
  Modelo = c(
    "FE estático",
    "Dinámico pooled",
    "Dinámico FE",
    "IV diferencias",
    "Arellano-Bond"
  ),

  Gamma_Coste_lag = c(
    NA,
    as.numeric(coef(modelo_dyn_pool)["lag(lCoste, 1)"]),
    as.numeric(coef(modelo_dyn_fe)["lag(lCoste, 1)"]),
    as.numeric(coef(modelo_iv_diff)["d_L1_lCoste"]),
    as.numeric(coef(modelo_ab)["lag(lCoste, 1)"])
  ),

  Produccion = c(
    as.numeric(coef(modelo_fe)["lProduccion"]),
    as.numeric(coef(modelo_dyn_pool)["lProduccion"]),
    as.numeric(coef(modelo_dyn_fe)["lProduccion"]),
    as.numeric(coef(modelo_iv_diff)["d_lProduccion"]),
    as.numeric(coef(modelo_ab)["lProduccion"])
  ),

  PrCombust = c(
    as.numeric(coef(modelo_fe)["lPrCombust"]),
    as.numeric(coef(modelo_dyn_pool)["lPrCombust"]),
    as.numeric(coef(modelo_dyn_fe)["lPrCombust"]),
    as.numeric(coef(modelo_iv_diff)["d_lPrCombust"]),
    as.numeric(coef(modelo_ab)["lPrCombust"])
  ),

  Carga = c(
    as.numeric(coef(modelo_fe)["Carga"]),
    as.numeric(coef(modelo_dyn_pool)["Carga"]),
    as.numeric(coef(modelo_dyn_fe)["Carga"]),
    as.numeric(coef(modelo_iv_diff)["d_Carga"]),
    as.numeric(coef(modelo_ab)["Carga"])
  )
)

coef_resumen

##############################################################
############       FIN DEL SCRIPT PRÁCTICA 6.2     ###########
##############################################################