##############################################################
############   ENTREGA 2 - EJERCICIO 2 (bwght)  ##############
############      MIGUEL FERNÁNDEZ VICENTE      ##############
##############################################################
# Objetivo:
# Estudiar el efecto del consumo de tabaco de la madre durante el embarazo
# sobre el peso del bebé al nacer.
#
# Dataset: bwght (Wooldridge) en formato .xls
# Variable dependiente:
#   - lbwght: log(peso del bebé al nacer)
#
# Regressors (controles):
#   - packs: paquetes fumados al día durante el embarazo
#   - male: dummy (1 si el bebé es niño)
#   - parity: orden de nacimiento
#   - lfaminc: log(ingreso familiar)
#
# Posible problema:
#   - Endogeneidad de packs (hábitos de salud, nutrición, estrés, etc.)
# Estrategia:
#   1) Estimar MCO
#   2) Diagnosticar residuos (normalidad)
#   3) Probar IV/2SLS con distintos instrumentos:
#        - cigprice
#        - cigprice + cigtax
#        - motheduc
#   4) Validar instrumentos (primera etapa y tests diagnósticos)
##############################################################

# ------------------------------------------------------------
# 0) Preparación
# ------------------------------------------------------------
rm(list = ls())

# Paquetes
library(readxl)    # leer .xls
library(AER)       # ivreg
library(lmtest)    # coeftest, tests
library(sandwich)  # errores robustos
library(tseries)   # Jarque-Bera
library(car)       # linearHypothesis

# ------------------------------------------------------------
# 1) Cargar datos
# ------------------------------------------------------------
# Ajusta esta ruta a la tuya (la que usas en GitHub / carpeta)
datos <- read_excel("ucm/Econometría_Aplicada/ENTREGA_2/Práctica 2_3 bwght.xls", na = ".")
datos <- read_excel("C:/Users/Usuario/GitHub/ucm/Econometría_Aplicada/ENTREGA_2/PrÃ¡ctica 2_3 bwght.xls")
# Vista rápida
# head(datos)
# str(datos)

# Tamaño muestral (en tu caso te daba 1388)
n <- length(datos$lbwght)
n

# ------------------------------------------------------------
# 2) Modelo MCO (baseline)
# ------------------------------------------------------------
m_ols <- lm(lbwght ~ packs + male + parity + lfaminc, data = datos)
summary(m_ols)

# Interpretación rápida (para ti):
# packs < 0 => más tabaco, menor peso al nacer (en log)
# Aproximación en %: 100 * beta_packs

beta_packs <- coef(m_ols)["packs"]
cat("Efecto aproximado en % por +1 pack/día:", round(100 * beta_packs, 2), "%\n")

# Errores robustos (por si el examen te pide inferencia robusta)
coeftest(m_ols, vcov = vcovHC(m_ols, type = "HC1"))

# ------------------------------------------------------------
# 3) Diagnóstico: normalidad de residuos
# ------------------------------------------------------------
res_ols <- residuals(m_ols)

# Test Jarque-Bera
jarque.bera.test(res_ols)

# Histograma de residuos (puedes guardarlo a png si quieres)
# png("hist_residuos_ols.png", width = 900, height = 600)
hist(res_ols, main = "Histograma de residuos (MCO)", xlab = "residuos")
# dev.off()

# QQ-plot (opcional)
# qqnorm(res_ols); qqline(res_ols, col="red")

# Nota:
# En muestras grandes, la no normalidad de residuos no invalida MCO,
# pero puede ser un indicio de colas largas / outliers / heterogeneidad.

# ------------------------------------------------------------
# 4) Sospecha de endogeneidad y IV (2SLS)
# ------------------------------------------------------------
# Idea: packs puede estar correlacionado con u (hábitos de salud, etc.)
# Se prueban instrumentos.

# ------------------------------------------------------------
# 4A) Instrumento 1: cigprice
# ------------------------------------------------------------
# 2SLS:
m_iv_price <- ivreg(lbwght ~ packs + male + parity + lfaminc |
                      cigprice + male + parity + lfaminc,
                    data = datos)

summary(m_iv_price, diagnostics = TRUE)

# Primera etapa explícita (muy importante en exámenes):
fs_price <- lm(packs ~ cigprice + male + parity + lfaminc, data = datos)
summary(fs_price)

# Conclusión típica si cigprice no es significativo:
# -> problema de instrumento débil: no explica packs

# ------------------------------------------------------------
# 4B) Instrumentos 2: cigprice + cigtax (conjunto)
# ------------------------------------------------------------
fs_price_tax <- lm(packs ~ cigprice + cigtax + male + parity + lfaminc, data = datos)
summary(fs_price_tax)

# Contraste conjunto de relevancia: H0 cigprice=0 y cigtax=0
linearHypothesis(fs_price_tax, c("cigprice = 0", "cigtax = 0"))

m_iv_price_tax <- ivreg(lbwght ~ packs + male + parity + lfaminc |
                          cigprice + cigtax + male + parity + lfaminc,
                        data = datos)

summary(m_iv_price_tax, diagnostics = TRUE)

# Si el F conjunto es bajo (regla práctica < 10), instrumentos débiles.

# ------------------------------------------------------------
# 4C) Instrumento alternativo: motheduc
# ------------------------------------------------------------
# OJO ECONOMÉTRICO:
# motheduc puede ser relevante para packs (sí), pero exogeneidad es discutible:
# educación de la madre podría afectar al peso del bebé por otras vías (nutrición, cuidados).
# Aun así, lo probamos como ejercicio.

# (Al tener motheduc, puede haber NA; tu summary indicaba 1 obs eliminada)
fs_meduc <- lm(packs ~ male + parity + lfaminc + motheduc, data = datos)
summary(fs_meduc)

m_iv_meduc <- ivreg(lbwght ~ packs + male + parity + lfaminc |
                      male + parity + lfaminc + motheduc,
                    data = datos)

summary(m_iv_meduc, diagnostics = TRUE)

# ------------------------------------------------------------
# 5) Comparación rápida entre modelos (opcional)
# ------------------------------------------------------------
# Función útil para comparar coeficiente de packs
extrae_coef <- function(mod, nombre = "packs"){
  c(est = coef(mod)[nombre],
    se  = sqrt(diag(vcov(mod)))[nombre])
}

cat("\nComparación coeficiente packs (estimación y se):\n")
cat("MCO       :", extrae_coef(m_ols), "\n")

# Para ivreg, vcov por defecto es homocedástica. Puedes usar robusta si quieres:
# Ejemplo robusto para IV:
vcov_iv_rob <- vcovHC(m_iv_meduc, type = "HC1")
cat("IV(meduc) :", c(est = coef(m_iv_meduc)["packs"],
                    se  = sqrt(diag(vcov_iv_rob))["packs"]), "\n")

# ------------------------------------------------------------
# 6) Conclusión tipo examen (para ti, en comentarios)
# ------------------------------------------------------------
# - MCO: packs negativo y significativo -> fumar reduce lbwght.
# - IV con cigprice (y con cigprice+cigtax): instrumentos débiles (no explican packs).
#   => 2SLS no fiable, varianzas grandes, coeficiente de packs impreciso.
# - IV con motheduc: instrumento más relevante (first stage fuerte),
#   pero Wu-Hausman no rechaza endogeneidad -> no evidencia clara de que MCO esté sesgado.
#   Además, exogeneidad de motheduc es debatible.
#
# Resultado práctico:
# - Presentar MCO como baseline y discutir limitaciones.
# - Explicar por qué IV no funciona con esos instrumentos (debilidad/exogeneidad).
##############################################################



