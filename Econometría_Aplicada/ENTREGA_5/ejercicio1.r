##############################################################
############   PRÁCTICA 6.1 - MODELOS DINÁMICOS  #############
############      MIGUEL FERNÁNDEZ VICENTE      #############
##############################################################

# Objetivo:
# Analizar los factores que influyen en el empleo de una muestra
# de 140 empresas británicas observadas durante 5 periodos.
#
# Variables:
# Empresa     = identificador de empresa
# Anio        = año
# Empleo      = empleo de la empresa (log)
# Salarios    = salario medio de la empresa (log)
# Capital     = capital bruto de la empresa (log)
# Produccion  = producción agregada de la empresa (log)

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
  "C:/Users/Usuario/GitHub/ucm/Econometría_Aplicada/ENTREGA_5/empresas.xlsx"
)

head(datos)
str(datos)
summary(datos)
names(datos)

# ------------------------------------------------------------
# 2) Estructura del panel
# ------------------------------------------------------------

n_empresas <- length(unique(datos$Empresa))
n_periodos <- length(unique(datos$Anio))
n_total <- nrow(datos)

n_empresas
n_periodos
n_total

table(datos$Anio)
summary(as.numeric(table(datos$Empresa)))

# Ordenamos por empresa y año
datos <- datos %>%
  arrange(Empresa, Anio)

# Convertimos a formato panel
pdata <- pdata.frame(datos, index = c("Empresa", "Anio"))

# Comprobación del panel
pdim(pdata)

# ------------------------------------------------------------
# 3) Análisis descriptivo
# ------------------------------------------------------------

summary(datos[, c("Empleo", "Salarios", "Capital", "Produccion")])

cor(
  datos[, c("Empleo", "Salarios", "Capital", "Produccion")],
  use = "complete.obs"
)

medias_anio <- datos %>%
  group_by(Anio) %>%
  summarise(
    Empleo_medio = mean(Empleo, na.rm = TRUE),
    Salarios_medio = mean(Salarios, na.rm = TRUE),
    Capital_medio = mean(Capital, na.rm = TRUE),
    Produccion_medio = mean(Produccion, na.rm = TRUE)
  )

medias_anio

# Gráficos exploratorios
plot(datos$Capital, datos$Empleo,
     main = "Empleo y Capital",
     xlab = "Capital (log)",
     ylab = "Empleo (log)",
     pch = 19, col = "blue")

plot(datos$Produccion, datos$Empleo,
     main = "Empleo y Producción",
     xlab = "Producción (log)",
     ylab = "Empleo (log)",
     pch = 19, col = "darkgreen")

plot(datos$Salarios, datos$Empleo,
     main = "Empleo y Salarios",
     xlab = "Salarios (log)",
     ylab = "Empleo (log)",
     pch = 19, col = "red")

# ------------------------------------------------------------
# 4) Modelo estático de panel
# ------------------------------------------------------------
# Modelo:
# Empleo_it = beta0 + beta1*Salarios_it + beta2*Capital_it
#             + beta3*Produccion_it + alpha_i + u_it

# ------------------------------------------------------------
# 4.1) Pooled OLS
# ------------------------------------------------------------

modelo_pool <- plm(
  Empleo ~ Salarios + Capital + Produccion,
  data = pdata,
  model = "pooling"
)

summary(modelo_pool)

# ------------------------------------------------------------
# 4.2) Efectos fijos individuales
# ------------------------------------------------------------

modelo_fe <- plm(
  Empleo ~ Salarios + Capital + Produccion,
  data = pdata,
  model = "within",
  effect = "individual"
)

summary(modelo_fe)

# Errores robustos agrupados por empresa
coeftest(
  modelo_fe,
  vcov = vcovHC(modelo_fe, type = "HC1", cluster = "group")
)

# ------------------------------------------------------------
# 4.3) Efectos fijos individuales y temporales
# ------------------------------------------------------------

modelo_fe_tw <- plm(
  Empleo ~ Salarios + Capital + Produccion,
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
# 4.4) Efectos aleatorios
# ------------------------------------------------------------

modelo_re <- plm(
  Empleo ~ Salarios + Capital + Produccion,
  data = pdata,
  model = "random",
  effect = "individual"
)

summary(modelo_re)

# ------------------------------------------------------------
# 5) Contrastes entre modelos estáticos
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

# Autocorrelación serial
pbgtest(modelo_fe)

# Dependencia contemporánea entre empresas
pcdtest(modelo_fe, test = "cd")

# ------------------------------------------------------------
# 6) Modelo dinámico con empleo retardado
# ------------------------------------------------------------
# Modelo:
# Empleo_it = gamma*Empleo_i,t-1 + beta1*Salarios_it
#             + beta2*Capital_it + beta3*Produccion_it
#             + alpha_i + u_it

# ------------------------------------------------------------
# 6.1) Modelo dinámico pooled
# ------------------------------------------------------------

modelo_dyn_pool <- plm(
  Empleo ~ lag(Empleo, 1) + Salarios + Capital + Produccion,
  data = pdata,
  model = "pooling"
)

summary(modelo_dyn_pool)

# ------------------------------------------------------------
# 6.2) Modelo dinámico con efectos fijos
# ------------------------------------------------------------

modelo_dyn_fe <- plm(
  Empleo ~ lag(Empleo, 1) + Salarios + Capital + Produccion,
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
# 7) Primeras diferencias + variable instrumental
# ------------------------------------------------------------
# Al diferenciar eliminamos el efecto fijo individual:
#
# d_Empleo_it = gamma*d_Empleo_i,t-1
#               + beta1*d_Salarios_it
#               + beta2*d_Capital_it
#               + beta3*d_Produccion_it
#               + d_u_it
#
# Como d_Empleo_i,t-1 puede ser endógeno, usamos Empleo_i,t-2
# como instrumento.

datos_diff <- datos %>%
  arrange(Empresa, Anio) %>%
  group_by(Empresa) %>%
  mutate(
    L1_Empleo = dplyr::lag(Empleo, 1),
    L2_Empleo = dplyr::lag(Empleo, 2),

    d_Empleo = Empleo - dplyr::lag(Empleo, 1),
    d_L1_Empleo = dplyr::lag(Empleo, 1) - dplyr::lag(Empleo, 2),

    d_Salarios = Salarios - dplyr::lag(Salarios, 1),
    d_Capital = Capital - dplyr::lag(Capital, 1),
    d_Produccion = Produccion - dplyr::lag(Produccion, 1)
  ) %>%
  ungroup()

datos_iv <- datos_diff %>%
  filter(
    !is.na(d_Empleo),
    !is.na(d_L1_Empleo),
    !is.na(L2_Empleo),
    !is.na(d_Salarios),
    !is.na(d_Capital),
    !is.na(d_Produccion)
  )

# Comprobación: con 140 empresas y 5 años, deben quedar 420 observaciones
nrow(datos_iv)
head(datos_iv)

# ------------------------------------------------------------
# 7.1) Primera etapa: relevancia del instrumento
# ------------------------------------------------------------

primera_etapa <- lm(
  d_L1_Empleo ~ L2_Empleo + d_Salarios + d_Capital + d_Produccion,
  data = datos_iv
)

summary(primera_etapa)

# ------------------------------------------------------------
# 7.2) Estimación IV en primeras diferencias
# ------------------------------------------------------------

modelo_iv_diff <- ivreg(
  d_Empleo ~ d_L1_Empleo + d_Salarios + d_Capital + d_Produccion |
    L2_Empleo + d_Salarios + d_Capital + d_Produccion,
  data = datos_iv
)

summary(modelo_iv_diff, diagnostics = TRUE)

coeftest(
  modelo_iv_diff,
  vcov = vcovHC(modelo_iv_diff, type = "HC1")
)

# ------------------------------------------------------------
# 8) Arellano-Bond / GMM dinámico
# ------------------------------------------------------------
# Estimación dinámica con instrumentos internos.
# Usamos retardos de Empleo como instrumentos.

modelo_ab <- pgmm(
  Empleo ~ lag(Empleo, 1) + Salarios + Capital + Produccion |
    lag(Empleo, 2:4) + Salarios + Capital + Produccion,
  data = pdata,
  effect = "individual",
  model = "twosteps",
  transformation = "d",
  collapse = TRUE
)

summary(modelo_ab, robust = TRUE)

# AR(1): normalmente se rechaza en diferencias
mtest(modelo_ab, order = 1)

# AR(2): no debería rechazarse para que los instrumentos sean válidos
mtest(modelo_ab, order = 2)

# Test de Sargan/Hansen
sargan(modelo_ab)

# ------------------------------------------------------------
# 9) Comparación final de coeficientes
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

  Gamma_Empleo_lag = c(
    NA,
    as.numeric(coef(modelo_dyn_pool)["lag(Empleo, 1)"]),
    as.numeric(coef(modelo_dyn_fe)["lag(Empleo, 1)"]),
    as.numeric(coef(modelo_iv_diff)["d_L1_Empleo"]),
    as.numeric(coef(modelo_ab)["lag(Empleo, 1)"])
  ),

  Salarios = c(
    as.numeric(coef(modelo_fe)["Salarios"]),
    as.numeric(coef(modelo_dyn_pool)["Salarios"]),
    as.numeric(coef(modelo_dyn_fe)["Salarios"]),
    as.numeric(coef(modelo_iv_diff)["d_Salarios"]),
    as.numeric(coef(modelo_ab)["Salarios"])
  ),

  Capital = c(
    as.numeric(coef(modelo_fe)["Capital"]),
    as.numeric(coef(modelo_dyn_pool)["Capital"]),
    as.numeric(coef(modelo_dyn_fe)["Capital"]),
    as.numeric(coef(modelo_iv_diff)["d_Capital"]),
    as.numeric(coef(modelo_ab)["Capital"])
  ),

  Produccion = c(
    as.numeric(coef(modelo_fe)["Produccion"]),
    as.numeric(coef(modelo_dyn_pool)["Produccion"]),
    as.numeric(coef(modelo_dyn_fe)["Produccion"]),
    as.numeric(coef(modelo_iv_diff)["d_Produccion"]),
    as.numeric(coef(modelo_ab)["Produccion"])
  )
)

coef_resumen

##############################################################
############       FIN DEL SCRIPT PRÁCTICA 6.1     ###########
##############################################################
