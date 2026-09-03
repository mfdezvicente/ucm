##############
# PRÁCTICA 5 #
##############

# Enunciado:
# Analizar cómo influye el nivel de estudios en el salario mensual en una región.
# Variables:
# EDAD (años), SALARIO (€/mes), ESTUDIOS: 1=Básicos, 2=Medios, 3=Superiores

# ------------------------------------------------------------
# 0) Preparación y carga
# ------------------------------------------------------------
rm(list = ls())

library(readxl)
library(lmtest)     # bptest
library(tseries)    # jarque.bera.test
library(sandwich)   # vcovHC
# library(car)      # vif (si quieres mirarlo)

datos <- read_excel("ucm/Econometría_Aplicada/Datos.xlsx")
datos <- read_excel("C:/Users/Usuario/GitHub/ucm/Econometría_Aplicada/ENTREGA_1/Datos.xlsx")
# Dummies (base: estudios básicos)
datos$D_MEDIOS     <- ifelse(datos$ESTUDIOS == 2, 1, 0)
datos$D_SUPERIORES <- ifelse(datos$ESTUDIOS == 3, 1, 0)

# Transformaciones
datos$EDAD2 <- datos$EDAD^2
datos$ln_salario <- log(datos$SALARIO)

# ------------------------------------------------------------
# 1) Modelo inicial: edad + edad^2 + dummies (en niveles)
# ------------------------------------------------------------
# Idea inicial: controlar por edad de forma flexible y capturar diferencias de nivel por estudios.
m_base <- lm(SALARIO ~ EDAD + EDAD2 + D_MEDIOS + D_SUPERIORES, data = datos)
summary(m_base)

# Validación heterocedasticidad
bptest(m_base)
# -> Se detecta heterocedasticidad fuerte. Probamos alternativas.

# ------------------------------------------------------------
# 2) Transformación logarítmica
# ------------------------------------------------------------
# Buscamos estabilizar la varianza y trabajar con una escala más “natural” para salarios.
m_log <- lm(ln_salario ~ EDAD + EDAD2 + D_MEDIOS + D_SUPERIORES, data = datos)
summary(m_log)
bptest(m_log)
# -> Sigue habiendo heterocedasticidad.

# Si la heterocedasticidad persiste, una salida correcta es inferencia robusta:
coeftest(m_log, vcov = vcovHC(m_log, type = "HC1"))

# ------------------------------------------------------------
# 3) Intento de MCP/WLS modelando la varianza con los residuos^2
# ------------------------------------------------------------
# (A veces funciona, pero depende de si acertamos la forma de la varianza)
u2 <- resid(m_log)^2
var_mod <- lm(u2 ~ EDAD + EDAD2 + D_MEDIOS + D_SUPERIORES, data = datos)
w <- 1 / pmax(fitted(var_mod), 1e-8)

m_wls <- lm(ln_salario ~ EDAD + EDAD2 + D_MEDIOS + D_SUPERIORES,
            data = datos, weights = w)
summary(m_wls)
bptest(m_wls)
# -> No se corrige. Esto sugiere que el problema no es solo “varianza”,
# sino que hay una estructura que el modelo no está recogiendo bien.

# ------------------------------------------------------------
# 4) Diagnóstico visual: el patrón “a rayas” sugiere grupos con pendientes distintas
# ------------------------------------------------------------
plot(datos$EDAD, datos$SALARIO,
     main="Gráfico de Dispersión (SALARIO vs EDAD)",
     xlab="Edad",
     ylab="Salario",
     pch=19, col="blue")

# Se ven claramente tres grupos/bandas. Esto sugiere que el efecto de la edad sobre el salario
# depende del nivel educativo (no es la misma pendiente para todos).
# Por ello, introducimos interacción: EDAD × dummies.

# ------------------------------------------------------------
# 5) Modelo con interacción (TU BLOQUE)
# ------------------------------------------------------------
m_int <- lm(SALARIO ~ EDAD * (D_MEDIOS + D_SUPERIORES), data = datos)
summary(m_int)
bptest(m_int)

# Desaparece la heterocedasticidad, por lo que parece que el problema se debía a la no linealidad
# (en realidad, a que había pendientes distintas por grupo) y al incluir la interacción entre edad
# y las dummies, hemos conseguido corregirlo. Veamos la normalidad de los residuos.

residuos <- residuals(m_int)
jarque.bera.test(residuos)
qqnorm(residuals(m_int))
qqline(residuals(m_int), col="red")

# Observamos que los residuos no siguen una distribución normal. Vamos a intentar corregirlo
# mediante el tratamiento de outliers.

res_ordenados <- sort(abs(residuos), decreasing=TRUE)
head(res_ordenados)

# ------------------------------------------------------------
# 6) Outliers: dummies para las observaciones más extremas (tal como lo tienes)
# ------------------------------------------------------------
datos$o334 <- 0; datos$o334[334] <- 1
datos$o463 <- 0; datos$o463[463] <- 1
datos$o44  <- 0; datos$o44[44]   <- 1
datos$o217 <- 0; datos$o217[217] <- 1
datos$o490 <- 0; datos$o490[490] <- 1

modelo_5 <- lm(SALARIO ~ EDAD * (D_MEDIOS + D_SUPERIORES) +
                 o334 + o463 + o44 + o217 + o490,
               data = datos)
summary(modelo_5)

bptest(modelo_5)

residuos <- residuals(modelo_5)
jarque.bera.test(residuos)
qqnorm(residuos)
qqline(residuos, col="red")

# Quiero hacer un if
if(jarque.bera.test(residuos)$p.value < 0.05){
  print("Los residuos no son normales")
} else {
  print("Los residuos son normales")
}
