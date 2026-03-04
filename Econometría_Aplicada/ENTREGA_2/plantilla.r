
############################################################
# PLANTILLA IV / 2SLS (MC2E) EN R — CHECKLIST COMENTADA
# Qué pensar al proponer un instrumento z, y qué mirar en R
############################################################

# 0) Limpieza y paquetes
rm(list = ls())

# install.packages(c("AER","car","sandwich","lmtest"))  # si te faltan
library(AER)       # ivreg()
library(car)       # linearHypothesis() para F conjunto
library(sandwich)  # vcovHC(), vcovCL()
library(lmtest)    # coeftest()

############################################################
# 1) Define el problema (cambia nombres)
#
# Ecuación estructural (la "causal"):
#   y = beta0 + beta1 * x + gamma' W + u
#   - x: ENDÓGENA (sospechas cov(x,u) != 0)
#   - W: controles EXÓGENOS
#
# Instrumentos:
#   - Zx: instrumentos EXCLUIDOS (NO entran en la ecuación estructural)
#         pero deben explicar x y ser exógenos respecto a u.
#
# Regla mental:
#   (A) Relevancia (inclusión): Zx explica x dado W
#   (B) Validez (exclusión): Zx NO afecta y salvo vía x  (cov(Zx,u)=0)
############################################################

# Ejemplo de nombres (ajusta a tu dataset):
# datos <- read.csv("...")

# y  <- "children"
# x  <- "educ"
# W  <- c("age", "I(age^2)")   # controles exógenos
# Zx <- c("frsthalf", "urban") # instrumentos excluidos


############################################################
# 2) Paso 0: OLS (referencia)
#   Esto NO arregla endogeneidad, pero te da baseline.
############################################################

# modelo_ols <- lm(children ~ educ + age + I(age^2), data = datos)
# summary(modelo_ols)

# Nota: No uses Jarque-Bera como prueba de endogeneidad.
# La endogeneidad es cov(x,u) != 0, no "residuos no normales".


############################################################
# 3) CONDICIÓN 1 — RELEVANCIA (restricción de inclusión)
#   "¿El instrumento mueve x, controlando por W?"
#
# Primera etapa:
#   x = pi0 + pi1*Zx + delta'W + v
#
# Lo que miras:
#   - Significatividad de los instrumentos en 1ª etapa
#   - F de instrumentos excluidos (regla práctica: F > 10)
############################################################

# 3.1) Primera etapa (ejemplo con 2 instrumentos excluidos)
# primera_etapa <- lm(educ ~ frsthalf + urban + age + I(age^2), data = datos)
# summary(primera_etapa)

# 3.2) F conjunto de instrumentos excluidos: H0: frsthalf=0 y urban=0
# linearHypothesis(primera_etapa, c("frsthalf = 0", "urban = 0"))

# Si tienes SOLO 1 instrumento excluido (ej. frsthalf),
# el F de instrumentos excluidos coincide con t^2 aproximadamente,
# pero mejor decirlo como "F de primera etapa" igualmente.


############################################################
# 4) CONDICIÓN 2 — VALIDEZ (restricción de exclusión / exogeneidad)
#   "¿Zx está incorrelado con u? ¿Afecta a y solo a través de x?"
#
# Esto NO se observa en datos: se justifica con argumento económico.
#
# Estadísticamente:
#   - Si el modelo está SOBREIDENTIFICADO (más instrumentos que endógenas),
#     puedes hacer un test de sobreidentificación (Sargan/Hansen).
#   - Si está EXACTAMENTE identificado, NO puedes testear validez con Sargan/Hansen.
############################################################


############################################################
# 5) Estimación IV / 2SLS (MC2E) con ivreg()
#
# Sintaxis clave:
#   ivreg( y ~ x + W | Zx + W )
#
# A la izquierda del |: ecuación estructural
# A la derecha del |: TODOS los instrumentos (excluidos Zx + exógenos W)
############################################################

# modelo_iv <- ivreg(children ~ educ + age + I(age^2) |
#                    frsthalf + urban + age + I(age^2),
#                    data = datos)

# Diagnósticos útiles (pon TRUE, no 'T'):
# summary(modelo_iv, diagnostics = TRUE)

# Qué significa lo que suele salir:
# - Weak instruments: fuerza de la 1ª etapa (si es bajo => instrumento débil)
# - Wu-Hausman: H0: x es exógena (si NO rechazas, OLS podría bastar)
# - Sargan (si sobreidentificado): H0: instrumentos válidos (no prueba validez;
#   si rechazas, al menos uno es inválido)


############################################################
# 6) Errores estándar robustos (recomendado)
############################################################

# Robustos a heterocedasticidad:
# coeftest(modelo_iv, vcov. = vcovHC(modelo_iv, type = "HC1"))

# Si hay clusters (ej. provincia):
# cl <- datos$provincia
# coeftest(modelo_iv, vcov. = vcovCL(modelo_iv, cluster = cl, type = "HC1"))


############################################################
# 7) Decisión final: ¿me quedo con OLS o con IV?
#
# (i) Si 1ª etapa es débil (F bajo): IV es impreciso y poco fiable.
# (ii) Si Wu-Hausman rechaza exogeneidad de x: IV tiene más sentido.
# (iii) Si Sargan/Hansen rechaza: hay sospecha de instrumentos inválidos.
#
# Frase para informe:
# - "Instrumentos fuertes" -> F de excluidos alto.
# - "Endogeneidad" -> Wu-Hausman.
# - "Validez conjunta (si sobreidentificado)" -> Sargan/Hansen.
############################################################


############################################################
# 8) (Extra) Caso con DOS endógenas: y2 y y3
#
# Estructural: y1 = ... + b1*y2 + b2*y3 + W + u
# Necesitas al menos tantos instrumentos excluidos como endógenas (>=2).
#
# En 2SLS, las primeras etapas conceptuales son:
#   y2 ~ Z + W
#   y3 ~ Z + W
# donde Z incluye instrumentos excluidos y W los exógenos del modelo.
############################################################