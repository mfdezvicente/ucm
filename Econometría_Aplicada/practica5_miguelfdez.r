##############
# PRÁCTICA 5 #
##############

# Enunciado:
# Analizar cómo influye el nivel de estudios en el salario mensual en una región.
# Variables:
# EDAD (años), SALARIO (euros/mes), ESTUDIOS: 1=Básicos, 2=Medios, 3=Superiores

# ------------------------------------------------------------
# 0) Preparación
# ------------------------------------------------------------
rm(list = ls())

library(readxl)
library(lmtest)     # bptest, coeftest
library(sandwich)   # vcovHC
library(tseries)    # Jarque-Bera
library(car)        # vif

datos <- read_excel("ucm/Econometría_Aplicada/Datos.xlsx")

# Dummies de estudios (base: Básicos)
datos$D_MEDIOS     <- ifelse(datos$ESTUDIOS == 2, 1, 0)
datos$D_SUPERIORES <- ifelse(datos$ESTUDIOS == 3, 1, 0)

# Transformaciones típicas
datos$EDAD2 <- datos$EDAD^2
datos$ln_salario <- log(datos$SALARIO)

# ------------------------------------------------------------
# 1) Modelo base (tu idea inicial): EDAD + EDAD^2 + dummies
# ------------------------------------------------------------
m_base <- lm(SALARIO ~ EDAD + EDAD2 + D_MEDIOS + D_SUPERIORES, data = datos)
summary(m_base)

# Validación: heterocedasticidad (BP)
bptest(m_base)

# ------------------------------------------------------------
# 2) Modelo en log (para interpretar % y ver si mejora varianza)
# ------------------------------------------------------------
m_log <- lm(ln_salario ~ EDAD + EDAD2 + D_MEDIOS + D_SUPERIORES, data = datos)
summary(m_log)
bptest(m_log)

# Si la heterocedasticidad persiste (muy habitual), una opción sensata es robustez:
coeftest(m_log, vcov = vcovHC(m_log, type = "HC1"))

# ------------------------------------------------------------
# 3) Intento WLS "a mano" (tu enfoque): modelar varianza con u^2
# ------------------------------------------------------------
u2 <- resid(m_log)^2
var_mod <- lm(u2 ~ EDAD + EDAD2 + D_MEDIOS + D_SUPERIORES, data = datos)

# pesos = 1/varianza estimada (evitar valores demasiado pequeños)
w <- 1 / pmax(fitted(var_mod), 1e-8)

m_wls <- lm(ln_salario ~ EDAD + EDAD2 + D_MEDIOS + D_SUPERIORES, data = datos, weights = w)
summary(m_wls)
bptest(m_wls)

# (Si sigue habiendo heterocedasticidad, lo normal es que la forma de varianza no esté bien capturada)

# ------------------------------------------------------------
# 4) Modelo alternativo (tu prueba): quitar EDAD^2 para multicolinealidad
# ------------------------------------------------------------
m_log2 <- lm(ln_salario ~ EDAD + D_MEDIOS + D_SUPERIORES, data = datos)
summary(m_log2)
bptest(m_log2)

# Multicolinealidad (si la quieres mirar)
vif(m_log2)

# ------------------------------------------------------------
# 5) Modelo del compañero: interacción EDAD * (Medios + Superiores)
#    Interpretación: permite que el "retorno" de la edad sea distinto por nivel de estudios
#    (pendiente distinta) y también distintos niveles (interceptos) para Medios/Superiores.
# ------------------------------------------------------------
m_int <- lm(ln_salario ~ EDAD * (D_MEDIOS + D_SUPERIORES), data = datos)
summary(m_int)
bptest(m_int)

# Si quieres inferencia robusta (recomendable si BP sigue detectando heterocedasticidad):
coeftest(m_int, vcov = vcovHC(m_int, type = "HC1"))

# ------------------------------------------------------------
# 6) Normalidad de residuos: diagnóstico + tratar outliers
#    OJO: "arreglar normalidad" quitando puntos hay que justificarlo.
#    Aquí lo hacemos de forma transparente: detectamos los más extremos.
# ------------------------------------------------------------

# Elegimos como modelo de referencia el de interacción (tiene más sentido económico)
m_ref <- m_int
res <- residuals(m_ref)

# JB + QQplot
jarque.bera.test(res)
qqnorm(res, main = "QQ-plot residuos (modelo referencia)")
qqline(res, col = "red")

# Detectamos outliers por residuo estudentizado (más estándar que residuo simple)
stud_res <- rstudent(m_ref)

# Nos quedamos con los 3 más extremos (como tú estabas haciendo a mano)
k <- 3
idx_out <- order(abs(stud_res), decreasing = TRUE)[1:k]
idx_out

# ------------------------------------------------------------
# 6A) Opción A (como tú): meter dummies para outliers y reestimar
# ------------------------------------------------------------
datos$out_1 <- as.integer(seq_len(nrow(datos)) == idx_out[1])
datos$out_2 <- as.integer(seq_len(nrow(datos)) == idx_out[2])
datos$out_3 <- as.integer(seq_len(nrow(datos)) == idx_out[3])

m_int_out <- lm(ln_salario ~ EDAD * (D_MEDIOS + D_SUPERIORES) + out_1 + out_2 + out_3, data = datos)
summary(m_int_out)
bptest(m_int_out)

res2 <- residuals(m_int_out)
jarque.bera.test(res2)
qqnorm(res2, main = "QQ-plot residuos (interacción + dummies outliers)")
qqline(res2, col = "red")

# ------------------------------------------------------------
# 6B) Opción B: eliminar esas observaciones y reestimar (más simple)
# ------------------------------------------------------------
datos_clean <- datos[-idx_out, ]

m_int_clean <- lm(ln_salario ~ EDAD * (D_MEDIOS + D_SUPERIORES), data = datos_clean)
summary(m_int_clean)
bptest(m_int_clean)

res3 <- residuals(m_int_clean)
jarque.bera.test(res3)
qqnorm(res3, main = "QQ-plot residuos (interacción sin outliers)")
qqline(res3, col = "red")

# ------------------------------------------------------------
# 7) Conclusión práctica (para ti):
# - Si BP sigue dando heterocedasticidad: reporta HC1 (robustos) y ya.
# - Si quieres normalidad para una validación “de clase”, muestra que al tratar outliers
#   mejora el QQplot/JB, pero deja claro qué has hecho.
# ------------------------------------------------------------
