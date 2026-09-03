##############################################################
######## PLANTILLA DEFINITIVA - EXAMEN PRÁCTICO ECONOMETRÍA ##
######## Miguel Fernández - Temas 2, 3, 4, 5 y 6 ##############
##############################################################

# ============================================================
# CÓMO USAR ESTA PLANTILLA
# ============================================================
# Esta plantilla NO está pensada para ejecutarse entera de golpe.
# Está pensada como "chuleta de trabajo" para el examen práctico.
#
# Estructura:
#   TEMA 2  -> Variables instrumentales / MC2E
#   TEMA 3  -> Variable dependiente limitada:
#              MPL, Logit, Probit, Multinomial, Tobit, Two-part, Heckman
#   TEMA 4-5 -> Datos de panel:
#              pooling, efectos fijos, efectos aleatorios, tests
#   TEMA 6  -> Modelos dinámicos de panel con retardos e instrumentos
#
# Método de examen:
#   1) Identifica qué tipo de problema tienes.
#   2) Copia el bloque correspondiente.
#   3) Cambia nombres de archivo y variables.
#   4) Estima.
#   5) Interpreta usando los comentarios de esta plantilla.
#
# IMPORTANTE:
#   - En modelos lineales, los coeficientes se interpretan como efectos marginales.
#   - En Logit/Probit/Tobit/Heckman, cuidado: muchas veces hay que mirar efectos marginales.
#   - En panel, los coeficientes de efectos fijos lineales se interpretan como MCO,
#     pero con interpretación "dentro del individuo".
#   - En IV, el R2 no es lo importante. Lo importante es consistencia, validez
#     y relevancia de los instrumentos.
##############################################################


##############################################################
######################## 0) LIBRERÍAS ########################
##############################################################

rm(list = ls())

# Si alguna librería no está instalada:
# install.packages("readxl")
# install.packages("lmtest")
# install.packages("sandwich")
# install.packages("AER")
# install.packages("marginaleffects")
# install.packages("nnet")
# install.packages("car")
# install.packages("tseries")
# install.packages("sampleSelection")
# install.packages("plm")
# install.packages("dplyr")

library(readxl)
library(lmtest)
library(sandwich)
library(AER)
library(marginaleffects)
library(nnet)
library(car)
library(tseries)
library(sampleSelection)
library(plm)
library(dplyr)


##############################################################
############ 0.1) FUNCIONES ÚTILES PARA INTERPRETAR ##########
##############################################################

# ------------------------------------------------------------
# Función para interpretar p-valores de forma rápida
# ------------------------------------------------------------
interpretar_p <- function(pvalor, alpha = 0.05) {
  if (is.na(pvalor)) {
    return("p-valor no disponible")
  } else if (pvalor < alpha) {
    return("p < 0.05: se rechaza H0")
  } else {
    return("p >= 0.05: no se rechaza H0")
  }
}

# ------------------------------------------------------------
# Errores robustos HC1 para modelos lm, glm, ivreg, etc.
# ------------------------------------------------------------
robust_HC1 <- function(modelo) {
  coeftest(modelo, vcov = vcovHC(modelo, type = "HC1"))
}

# Interpretación:
#   coeftest(..., vcovHC(..., type="HC1")) NO cambia los coeficientes.
#   Cambia los errores estándar, estadísticos t/z y p-valores.
#   Se usa cuando sospechamos heterocedasticidad.
#   En IV corrige la inferencia, pero NO corrige la endogeneidad.
#   La endogeneidad la corrige el instrumento.


##############################################################
###################### 1) DECISIÓN GENERAL ###################
##############################################################

# Antes de estimar, pregúntate:
#
# A) ¿Tengo una variable explicativa endógena?
#    -> Tema 2: Variables instrumentales / MC2E.
#
# B) ¿Mi variable dependiente es limitada?
#    -> Si Y es 0/1: MPL, Logit, Probit.
#    -> Si Y tiene varias categorías sin orden: Logit multinomial.
#    -> Si Y es continua con muchos ceros: MCO, Tobit, Two-part o Heckman.
#    -> Si Y solo se observa para una submuestra seleccionada: Heckman.
#
# C) ¿Tengo datos de panel?
#    -> Tema 4-5: pooling, efectos fijos, efectos aleatorios.
#
# D) ¿Tengo panel y variable dependiente retardada?
#    -> Tema 6: panel dinámico + instrumentos internos.


##############################################################
##############################################################
####################### TEMA 2: IV / MC2E ####################
##############################################################
##############################################################

# ============================================================
# 1) IDEA TEÓRICA
# ============================================================
# Problema:
#   En MCO necesitamos que cov(x, u) = 0.
#   Si cov(x, u) != 0, x es endógena.
#
# Causas típicas de endogeneidad:
#   - Variable omitida: habilidad no observada en salarios.
#   - Simultaneidad: precio y cantidad.
#   - Error de medida.
#   - Selección no aleatoria.
#
# Solución:
#   Usar una variable instrumental z.
#
# Un instrumento válido debe cumplir:
#
# 1) Restricción de inclusión / relevancia:
#      cov(z, x_endog) != 0
#      El instrumento debe explicar la variable endógena.
#      Esto SÍ se puede contrastar en la primera etapa.
#
# 2) Restricción de exclusión / exogeneidad:
#      cov(z, u) = 0
#      El instrumento no debe afectar directamente a y salvo a través de x.
#      Esto NO se puede contrastar directamente salvo si el modelo
#      está sobreidentificado.
#
# Modelo exactamente identificado:
#   Nº instrumentos excluidos = Nº variables endógenas.
#   No se puede hacer Sargan.
#
# Modelo sobreidentificado:
#   Nº instrumentos excluidos > Nº variables endógenas.
#   Sí se puede hacer Sargan/Hansen.


# ============================================================
# 2) PLANTILLA GENERAL IV
# ============================================================

# datos <- read_excel("RUTA/A/TU/ARCHIVO.xlsx")

# Supongamos:
#   y       = variable dependiente
#   x_endog = variable explicativa endógena
#   x_exog1, x_exog2 = controles exógenos
#   z1, z2 = instrumentos excluidos

# ------------------------------------------------------------
# Paso 1: MCO inicial
# ------------------------------------------------------------
# Sirve como referencia, pero si x_endog es endógena será sesgado e inconsistente.

modelo_mco <- lm(y ~ x_endog + x_exog1 + x_exog2, data = datos)
summary(modelo_mco)
robust_HC1(modelo_mco)

# Interpretación:
#   El coeficiente de x_endog solo se interpreta causalmente si cov(x_endog,u)=0.
#   Si sospechamos endogeneidad, MCO es solo una referencia.


# ------------------------------------------------------------
# Paso 2: Primera etapa
# ------------------------------------------------------------
# Se comprueba si los instrumentos explican la variable endógena.

primera_etapa <- lm(x_endog ~ z1 + z2 + x_exog1 + x_exog2, data = datos)
summary(primera_etapa)

# Interpretación:
#   H0: los instrumentos no explican x_endog.
#   Si los instrumentos son significativos, hay evidencia de inclusión.
#   Con varios instrumentos, conviene mirar el F conjunto.
#
# Regla de oro:
#   F > 10 -> los instrumentos no parecen débiles.
#   F <= 10 -> sospecha de instrumentos débiles.
#
# OJO:
#   Esta regla es orientativa. En examen suele bastar con decir:
#   "El instrumento debe estar correlacionado con la variable endógena.
#    Se comprueba en la primera etapa."


# Test conjunto de instrumentos en primera etapa:
linearHypothesis(primera_etapa, c("z1 = 0", "z2 = 0"))


# ------------------------------------------------------------
# Paso 3: MC2E / IV
# ------------------------------------------------------------
# Sintaxis de ivreg:
#   ivreg(y ~ endog + exog | instrumentos + exog)
#
# Las variables exógenas deben aparecer a ambos lados de la barra.
# Esto es porque las variables exógenas se instrumentan a sí mismas.

modelo_iv <- ivreg(
  y ~ x_endog + x_exog1 + x_exog2 |
      z1 + z2 + x_exog1 + x_exog2,
  data = datos
)

summary(modelo_iv, diagnostics = TRUE)
robust_HC1(modelo_iv)

# Interpretación de summary(..., diagnostics = TRUE):
#
# 1) Weak instruments:
#      H0: los instrumentos son débiles.
#      Si F es grande, especialmente > 10, mejor.
#
# 2) Wu-Hausman:
#      H0: la variable sospechosa es exógena.
#      Si p < 0.05 -> rechazamos exogeneidad -> preferimos IV.
#      Si p >= 0.05 -> no hay evidencia de endogeneidad -> MCO es más eficiente.
#
# 3) Sargan:
#      H0: los instrumentos son válidos / exógenos.
#      Solo aparece si el modelo está sobreidentificado.
#      Si p >= 0.05 -> no rechazamos validez de instrumentos.
#      Si p < 0.05 -> problema: al menos un instrumento puede no ser válido.


# ============================================================
# 3) EJEMPLO TIPO EDUCACIÓN Y SALARIOS
# ============================================================
# y = wage
# endógena = educ
# instrumentos = sibs, meduc
# control = exper

# datos <- read_excel("RUTA/Práctica 2_1 Datos wage2.xls")

modelo_mco_wage <- lm(wage ~ educ + exper, data = datos)
summary(modelo_mco_wage)

primera_etapa_wage <- lm(educ ~ sibs + meduc + exper, data = datos)
summary(primera_etapa_wage)
linearHypothesis(primera_etapa_wage, c("sibs = 0", "meduc = 0"))

modelo_iv_wage <- ivreg(
  wage ~ educ + exper |
         sibs + meduc + exper,
  data = datos
)
summary(modelo_iv_wage, diagnostics = TRUE)
robust_HC1(modelo_iv_wage)

# Frase de examen:
#   "La educación puede ser endógena porque factores no observados como
#    la habilidad afectan tanto a la educación como al salario. Usamos
#    instrumentos como sibs o meduc, que deben explicar educación pero no
#    afectar directamente al salario una vez controlada la educación."


##############################################################
##############################################################
####### TEMA 3: VARIABLE DEPENDIENTE LIMITADA ################
##############################################################
##############################################################

# ============================================================
# 1) ÁRBOL DE DECISIÓN DEL TEMA 3
# ============================================================
#
# Pregunta 1: ¿Cómo es Y?
#
# A) Y es binaria: 0/1
#      -> MPL como referencia.
#      -> Logit/Probit como modelos principales.
#      -> Coeficientes de Logit/Probit NO son efectos marginales.
#
# B) Y tiene más de dos categorías sin orden:
#      -> Logit multinomial.
#
# C) Y es continua con muchos ceros:
#      -> Preguntar: ¿por qué hay ceros?
#      -> Si los ceros son latentes/censura: Tobit.
#      -> Si hay dos decisiones: Two-part.
#      -> Si hay selección endógena: Heckman.
#
# D) Y solo se observa para una submuestra:
#      -> Heckman si la selección no es aleatoria.


##############################################################
################### TEMA 3.1: MPL / LOGIT / PROBIT ###########
##############################################################

# ============================================================
# 1) VARIABLE BINARIA
# ============================================================
# Ejemplo: inlf = 1 si trabaja, 0 si no trabaja.
#
# Modelo de Probabilidad Lineal:
#   inlf = beta0 + beta1*x1 + ... + u
#
# Interpretación MPL:
#   beta_j = cambio en la probabilidad de que Y=1
#            ante un aumento unitario de x_j.
#
# Problemas MPL:
#   1) Puede predecir probabilidades fuera de [0,1].
#   2) Heterocedasticidad.
#   3) Efecto marginal constante.
#
# Logit/Probit:
#   P(Y=1|X)=G(X beta)
#   Garantizan probabilidades entre 0 y 1.
#   Los coeficientes indican signo, no magnitud directa.
#   Para magnitud: efectos marginales.


# datos <- read_excel("RUTA/Práctica 3_1 Datos mrozv1.xls")

# ------------------------------------------------------------
# Paso 1: MPL
# ------------------------------------------------------------

modelo_mpl <- lm(
  inlf ~ kidslt6 + kidsge6 + age + educ + exper + nwifeinc + expersq,
  data = datos
)

summary(modelo_mpl)
robust_HC1(modelo_mpl)

# Interpretación:
#   Si beta_kidslt6 = -0.25:
#   "Un hijo adicional menor de 6 años reduce la probabilidad de trabajar
#    en 25 puntos porcentuales, ceteris paribus."
#
# Ojo:
#   En MPL beta sí se interpreta directamente como cambio en probabilidad.


# ------------------------------------------------------------
# Paso 2: comprobar predicciones fuera de [0,1]
# ------------------------------------------------------------

datos$phat_mpl <- predict(modelo_mpl)

summary(datos$phat_mpl)
sum(datos$phat_mpl < 0 | datos$phat_mpl > 1, na.rm = TRUE)

# Interpretación:
#   Si hay muchas predicciones fuera de [0,1], el MPL tiene una limitación clara.
#   Eso justifica Logit/Probit.


# ------------------------------------------------------------
# Paso 3: Logit
# ------------------------------------------------------------

modelo_logit <- glm(
  inlf ~ kidslt6 + kidsge6 + age + educ + exper + nwifeinc + expersq,
  family = binomial(link = "logit"),
  data = datos
)

summary(modelo_logit)

# Interpretación:
#   - Signo positivo: aumenta la probabilidad de Y=1.
#   - Signo negativo: reduce la probabilidad de Y=1.
#   - NO decir que beta es el cambio directo en probabilidad.


# ------------------------------------------------------------
# Paso 4: Probit
# ------------------------------------------------------------

modelo_probit <- glm(
  inlf ~ kidslt6 + kidsge6 + age + educ + exper + nwifeinc + expersq,
  family = binomial(link = "probit"),
  data = datos
)

summary(modelo_probit)


# ------------------------------------------------------------
# Paso 5: Probabilidades predichas
# ------------------------------------------------------------

datos$phat_logit  <- predict(modelo_logit, type = "response")
datos$phat_probit <- predict(modelo_probit, type = "response")

summary(datos$phat_logit)
summary(datos$phat_probit)

# Interpretación:
#   Estas sí son probabilidades estimadas entre 0 y 1.


# ------------------------------------------------------------
# Paso 6: Efectos marginales
# ------------------------------------------------------------

slopes(modelo_logit)
slopes(modelo_probit)

# Interpretación:
#   Los efectos marginales indican cuánto cambia P(Y=1)
#   ante un cambio en X.
#
# En Logit/Probit los efectos marginales dependen del individuo.
# Por eso solemos mirar:
#   - efectos marginales medios,
#   - o efectos marginales en un perfil concreto.


# ------------------------------------------------------------
# Paso 7: clasificación simple
# ------------------------------------------------------------

datos$pred_logit <- ifelse(datos$phat_logit > 0.5, 1, 0)
table(Real = datos$inlf, Predicho = datos$pred_logit)
mean(datos$pred_logit == datos$inlf, na.rm = TRUE)

# Interpretación:
#   Porcentaje de acierto.
#   No sustituye a la interpretación económica, pero ayuda como medida predictiva.


# ------------------------------------------------------------
# Paso 8: Log-verosimilitud y pseudo-R2 de McFadden
# ------------------------------------------------------------

logLik(modelo_logit)
logLik(modelo_probit)

modelo_logit_null <- glm(inlf ~ 1, family = binomial(link = "logit"), data = datos)
pseudo_R2_logit <- 1 - as.numeric(logLik(modelo_logit)) / as.numeric(logLik(modelo_logit_null))
pseudo_R2_logit

# Interpretación:
#   En Logit/Probit no usamos R2 normal como en MCO.
#   Podemos usar pseudo-R2, log-verosimilitud y porcentaje correctamente predicho.


##############################################################
################### TEMA 3.2: LOGIT MULTINOMIAL #############
##############################################################

# ============================================================
# Variable dependiente con más de dos categorías sin orden.
# Ejemplo:
#   FUMA = 0 fuma
#          1 exfumador
#          2 nunca ha fumado
#
# Modelo: logit multinomial.
#
# Idea:
#   Se elige una categoría base.
#   Los coeficientes comparan cada categoría frente a la base.
#   NO son efectos marginales directos sobre probabilidades absolutas.
# ============================================================

# datos <- read.table("RUTA/Habitos_Tabaco.txt", header = TRUE, sep = "\t")

datos$FUMA <- factor(datos$FUMA)

# Elegimos categoría base:
datos$FUMA <- relevel(datos$FUMA, ref = "0")  # 0 = fuma, por ejemplo

modelo_mult <- multinom(
  FUMA ~ EDAD + SUENO + ALCOHOL + EJERTL,
  data = datos
)

summary(modelo_mult)

# ------------------------------------------------------------
# P-valores aproximados
# ------------------------------------------------------------

res_mult <- summary(modelo_mult)
z_mult <- res_mult$coefficients / res_mult$standard.errors
p_mult <- 2 * (1 - pnorm(abs(z_mult)))
p_mult

# Interpretación:
#   Cada bloque de coeficientes compara una categoría frente a la base.
#
# Ejemplo:
#   Si EDAD es positivo en categoría "1" frente a "0":
#   "A mayor edad, aumenta la probabilidad relativa de ser exfumador
#    frente a fumador."


# ------------------------------------------------------------
# Probabilidades predichas
# ------------------------------------------------------------

prob_mult <- predict(modelo_mult, type = "probs")
head(prob_mult)

clase_predicha <- predict(modelo_mult)
table(Real = datos$FUMA, Predicho = clase_predicha)
mean(clase_predicha == datos$FUMA)

# Interpretación:
#   predict(..., type="probs") da la probabilidad estimada de cada categoría.


##############################################################
########### TEMA 3.4: VARIABLE CUALITATIVA EXPLICATIVA #######
##############################################################

# ============================================================
# Este bloque no es exactamente variable dependiente limitada.
# Pero entra en el tema porque usa variables cualitativas.
#
# Ejemplo:
#   apt = beta0 + beta1 read + beta2 math + dummies(prog) + u
#
# R crea dummies automáticamente si prog es factor.
# ============================================================

# datos <- read_excel("RUTA/Práctica 3_4 Datos.xls")

datos$prog <- as.factor(datos$prog)

# Elegimos categoría base:
datos$prog <- relevel(datos$prog, ref = "academic")

modelo_apt <- lm(apt ~ read + math + prog, data = datos)
summary(modelo_apt)
robust_HC1(modelo_apt)

# Interpretación:
#   - read: efecto de lectura sobre aptitud.
#   - math: efecto de matemáticas sobre aptitud.
#   - proggeneral: diferencia media entre programa general y academic.
#   - progvocational: diferencia media entre vocational y academic.


# ------------------------------------------------------------
# Contraste entre dos categorías no base
# ------------------------------------------------------------

linearHypothesis(modelo_apt, "proggeneral = progvocational")

# Interpretación:
#   H0: no hay diferencia entre general y vocational.
#   Si p < 0.05, sí hay diferencia significativa entre ambas categorías.


# ------------------------------------------------------------
# Diagnóstico típico de MCO
# ------------------------------------------------------------

bptest(modelo_apt)                 # H0: homocedasticidad
jarque.bera.test(residuals(modelo_apt))  # H0: normalidad de residuos

# Interpretación:
#   Si bptest p < 0.05 -> heterocedasticidad.
#   Si Jarque-Bera p < 0.05 -> residuos no normales.
#   En muestras grandes, la no normalidad suele preocupar menos que
#   heterocedasticidad para la inferencia.


##############################################################
#################### TEMA 3.4: TOBIT #########################
##############################################################

# ============================================================
# Cuándo usar Tobit:
#   Y es continua, pero tiene acumulación en cero.
#   Pensamos que hay una variable latente y*:
#
#      y* = X beta + u
#
#   Observamos:
#      y = 0    si y* <= 0
#      y = y*   si y* > 0
#
# Intuición:
#   El cero representa una censura o solución de esquina.
#
# Tobit es mezcla de:
#   - Probit: decisión y > 0.
#   - MCO: cantidad observada para y > 0.
#
# Cuidado:
#   Los coeficientes Tobit son efectos sobre y*, no necesariamente
#   efectos directos sobre y observada.
# ============================================================

# datos <- read_excel("RUTA/Práctica 3_3 Datos apple.xls")

# ------------------------------------------------------------
# Exploración: muchos ceros
# ------------------------------------------------------------

table(datos$ecolbs == 0)
mean(datos$ecolbs == 0, na.rm = TRUE)

hist(datos$ecolbs, main = "Distribución de ecolbs", xlab = "ecolbs")


# ------------------------------------------------------------
# MCO como referencia
# ------------------------------------------------------------

modelo_mco_apple <- lm(
  ecolbs ~ ecoprc + regprc + faminc + hhsize,
  data = datos
)

summary(modelo_mco_apple)
robust_HC1(modelo_mco_apple)

# Interpretación:
#   MCO estima E(y|x) aproximadamente.
#   Pero puede predecir valores negativos y mantiene efectos marginales constantes.


# ------------------------------------------------------------
# Tobit con censura izquierda en 0
# ------------------------------------------------------------

modelo_tobit <- tobit(
  ecolbs ~ ecoprc + regprc + faminc + hhsize,
  left = 0,
  data = datos
)

summary(modelo_tobit)

# Interpretación:
#   Coeficientes:
#     beta_j afecta a la variable latente y*.
#   Pero normalmente interesa:
#     1) efecto sobre y*,
#     2) efecto sobre E(y | y > 0, x),
#     3) efecto sobre E(y | x), incluyendo ceros.


# ------------------------------------------------------------
# Efecto marginal aproximado sobre E(y|x)
# ------------------------------------------------------------
# Fórmula general:
#   dE(y|x)/dx_j = Phi(x beta / sigma) * beta_j
#
# Es decir:
#   efecto marginal observado = beta_j * probabilidad de estar por encima de 0

beta_tobit <- coef(modelo_tobit)[names(coef(modelo_tobit)) != "Log(scale)"]
sigma_tobit <- modelo_tobit$scale
X_tobit <- model.matrix(modelo_tobit)

xb_tobit <- as.vector(X_tobit %*% beta_tobit)
Phi_tobit <- pnorm(xb_tobit / sigma_tobit)

# Ejemplo para ecoprc:
mean(Phi_tobit * beta_tobit["ecoprc"], na.rm = TRUE)

# Interpretación:
#   Este valor aproxima el efecto medio de ecoprc sobre el consumo esperado
#   observado, incluyendo ceros.


##############################################################
#################### TEMA 3.5: TWO-PART / BARRERA ###########
##############################################################

# ============================================================
# Cuándo usar Two-part:
#   Hay dos decisiones distintas:
#      1) Participar o no: y > 0
#      2) Cuánto consumir/gastar si y > 0
#
# Diferencia con Tobit:
#   Tobit impone que el mismo mecanismo determina participar y cuánto.
#   Two-part permite que una variable afecte a la probabilidad, pero no
#   a la cantidad, o incluso que afecte en sentido contrario.
#
# Supuesto clave:
#   Los errores de la ecuación de participación y de cantidad no deben
#   estar correlacionados.
#
# Si están correlacionados -> problema de selección -> Heckman.
# ============================================================

datos$consume_pos <- ifelse(datos$ecolbs > 0, 1, 0)

# Primera parte: probabilidad de consumir algo.
modelo_participacion <- glm(
  consume_pos ~ ecoprc + regprc + faminc + hhsize,
  family = binomial(link = "probit"),
  data = datos
)

summary(modelo_participacion)
slopes(modelo_participacion)

# Segunda parte: cuánto consume, solo positivos.
datos_pos <- subset(datos, ecolbs > 0)

modelo_cantidad <- lm(
  ecolbs ~ ecoprc + regprc + faminc + hhsize,
  data = datos_pos
)

summary(modelo_cantidad)
robust_HC1(modelo_cantidad)

# Interpretación:
#   Parte 1:
#     "Qué afecta a la probabilidad de consumir".
#   Parte 2:
#     "Qué afecta a la cantidad consumida entre quienes consumen".
#
# Ojo:
#   Si la submuestra positiva no es aleatoria por inobservables,
#   la segunda parte puede estar sesgada. En ese caso, pensar en Heckman.


##############################################################
#################### TEMA 3.6: HECKMAN #######################
##############################################################

# ============================================================
# Cuándo usar Heckman:
#   Solo observamos la variable de resultado para una submuestra
#   seleccionada, y esa selección puede ser no aleatoria.
#
# Ejemplo:
#   Queremos estudiar pensiones.
#   Hay muchas pensiones iguales a cero.
#   Podemos estudiar:
#      1) quién tiene pensión positiva,
#      2) cuánto cobra entre quienes tienen pensión positiva,
#   corrigiendo que quienes tienen pensión positiva pueden no ser
#   una muestra aleatoria.
#
# Ecuación de selección:
#   pension_pos = 1 si pension > 0
#
# Ecuación de resultado:
#   pension = beta0 + beta1 male + beta2 edad + ...
#
# Variable de exclusión:
#   union entra en selección pero no en resultado.
#
# Lambda / IMR:
#   Si es significativa -> evidencia de selección endógena.
#
# Rho:
#   Correlación entre errores de selección y resultado.
#   Si rho es significativo -> hay selección endógena.
# ============================================================

# datos <- read_excel("RUTA/Práctica 3_5 Datos.xls")

datos$pension_pos <- ifelse(datos$pension > 0, 1, 0)

table(datos$pension_pos)
table(datos$union, datos$pension_pos)
aggregate(pension ~ male, data = datos, mean)
aggregate(pension ~ male, data = subset(datos, pension > 0), mean)

# ------------------------------------------------------------
# Modelo naive: OLS solo sobre pensiones positivas
# ------------------------------------------------------------

datos_pos <- subset(datos, pension > 0)

modelo_ols_pension <- lm(
  pension ~ male + edad + tenure + educ,
  data = datos_pos
)

summary(modelo_ols_pension)
robust_HC1(modelo_ols_pension)

# Interpretación:
#   El coeficiente de male mide la brecha de género entre hombres y mujeres
#   SOLO entre quienes tienen pensión positiva.
#
# No mide directamente la brecha total en toda la población,
# porque excluye los ceros.


# ------------------------------------------------------------
# Heckman en dos etapas
# ------------------------------------------------------------

modelo_heckman_2s <- heckit(
  selection = pension_pos ~ male + edad + tenure + educ + union,
  outcome   = pension ~ male + edad + tenure + educ,
  data = datos
)

summary(modelo_heckman_2s)

# Interpretación:
#   - selection: explica tener pensión positiva.
#   - outcome: explica cuantía de la pensión.
#   - union es variable de exclusión:
#       afecta a tener pensión positiva,
#       pero se excluye de la cuantía de pensión.
#   - Si lambda / IMR es significativa:
#       hay selección muestral endógena.


# ------------------------------------------------------------
# Heckman por máxima verosimilitud
# ------------------------------------------------------------

modelo_heckman_ml <- selection(
  selection = pension_pos ~ male + edad + tenure + educ + union,
  outcome   = pension ~ male + edad + tenure + educ,
  data = datos,
  method = "ml"
)

summary(modelo_heckman_ml)

# Interpretación:
#   - ML exige supuestos más fuertes, pero es más eficiente si se cumplen.
#   - Mirar rho:
#       rho distinto de 0 -> errores de selección y resultado correlacionados.
#       Hay selección endógena.


# ------------------------------------------------------------
# Comparación de brecha de género
# ------------------------------------------------------------

coef(modelo_ols_pension)["male"]
coef(modelo_heckman_2s)
coef(modelo_heckman_ml)

# Interpretación:
#   Si el coeficiente de male cambia mucho entre OLS y Heckman,
#   la selección estaba afectando la estimación de la brecha.


##############################################################
##############################################################
################ TEMA 4-5: DATOS DE PANEL ####################
##############################################################
##############################################################

# ============================================================
# 1) IDEA TEÓRICA
# ============================================================
# Datos de panel:
#   Observamos individuos i = 1,...,N durante periodos t = 1,...,T.
#
# Modelo básico:
#   y_it = alpha_i + beta x_it + u_it
#
# alpha_i recoge características propias del individuo:
#   - empresa: cultura financiera, calidad de gestión, sector...
#   - trabajador: habilidad, personalidad...
#   - país: instituciones, geografía...
#
# Tres modelos:
#
# 1) Pooling:
#      y_it = alpha + beta x_it + u_it
#      Ignora efectos individuales.
#
# 2) Efectos fijos:
#      y_it = alpha_i + beta x_it + u_it
#      Cada individuo tiene su propia constante.
#      Permite que alpha_i esté correlacionado con X.
#      Usa variación "dentro del individuo".
#
# 3) Efectos aleatorios:
#      y_it = alpha + beta x_it + v_it
#      v_it = alpha_i + u_it
#      Trata alpha_i como parte del error.
#      Exige que cov(alpha_i, X_it)=0.
#      Es más eficiente si el supuesto se cumple.
#
# Frase clave:
#   Efectos fijos permite correlación entre heterogeneidad individual
#   y regresores. Efectos aleatorios exige que no exista esa correlación.
#
# Coeficientes:
#   En modelos lineales de panel, los coeficientes se interpretan como MCO.
#   En efectos fijos, la interpretación es intraindividuo:
#      "cuando una misma empresa cambia X respecto a su media,
#       cómo cambia Y".
# ============================================================


# ============================================================
# 2) PLANTILLA PANEL ESTÁTICO
# ============================================================

# datos <- read_excel("RUTA/archivo_panel.xlsx")

# Crear estructura de panel:
# index = c("id_individuo", "id_tiempo")
pdata <- pdata.frame(datos, index = c("id", "time"))

# Comprobar dimensiones:
pdim(pdata)

# Interpretación:
#   Balanced Panel: todos los individuos tienen el mismo número de periodos.
#   Unbalanced Panel: faltan observaciones para algún individuo-periodo.


# ------------------------------------------------------------
# Modelo pooling
# ------------------------------------------------------------

modelo_pool <- plm(
  y ~ x1 + x2 + x3,
  data = pdata,
  model = "pooling"
)

summary(modelo_pool)

# Interpretación:
#   Ignora heterogeneidad individual.
#   Solo sería razonable si no hay efectos propios relevantes.


# ------------------------------------------------------------
# Modelo de efectos fijos
# ------------------------------------------------------------

modelo_fe <- plm(
  y ~ x1 + x2 + x3,
  data = pdata,
  model = "within"
)

summary(modelo_fe)

# Interpretación:
#   Controla por características fijas no observadas de cada individuo.
#   Si x1 cambia dentro del individuo, beta1 mide cómo cambia y dentro del mismo individuo.
#
# Ojo:
#   Las variables constantes en el tiempo desaparecen.
#   Ejemplo: sexo, raza, región fija, educación si no cambia, sector fijo.
#   No es un error: quedan absorbidas por alpha_i.


# ------------------------------------------------------------
# Modelo de efectos aleatorios
# ------------------------------------------------------------

modelo_re <- plm(
  y ~ x1 + x2 + x3,
  data = pdata,
  model = "random"
)

summary(modelo_re)

# Interpretación:
#   Reconoce heterogeneidad individual, pero supone que no está
#   correlacionada con las variables explicativas.
#   Si ese supuesto falla, RE es inconsistente.


# ------------------------------------------------------------
# Test 1: Pooling vs efectos fijos
# ------------------------------------------------------------

pFtest(modelo_fe, modelo_pool)

# H0: todas las constantes individuales son iguales.
#     No hacen falta efectos fijos.
#
# H1: algún alpha_i es diferente.
#     Hay efectos individuales.
#
# Si p < 0.05:
#   rechazamos H0 -> preferimos efectos fijos frente a pooling.
#
# Si p >= 0.05:
#   no rechazamos H0 -> pooling podría ser suficiente.


# ------------------------------------------------------------
# Test 2: Pooling vs efectos aleatorios
# ------------------------------------------------------------

plmtest(modelo_pool, type = "bp")
# A veces también se ve:
# plmtest(modelo_re, type = "bp")

# H0: var(alpha_i) = 0.
#     No hay efectos aleatorios.
#
# H1: var(alpha_i) > 0.
#     Hay efectos aleatorios.
#
# Si p < 0.05:
#   rechazamos H0 -> RE mejora frente a pooling.
#
# Si p >= 0.05:
#   pooling podría valer.


# ------------------------------------------------------------
# Test 3: Hausman - efectos fijos vs efectos aleatorios
# ------------------------------------------------------------

phtest(modelo_fe, modelo_re)

# H0: cov(alpha_i, X_it) = 0.
#     RE es consistente y eficiente.
#
# H1: cov(alpha_i, X_it) != 0.
#     RE es inconsistente. FE es preferible.
#
# Si p < 0.05:
#   rechazamos H0 -> usar efectos fijos.
#
# Si p >= 0.05:
#   no rechazamos H0 -> efectos aleatorios puede usarse y es más eficiente.


# ------------------------------------------------------------
# Errores robustos en panel
# ------------------------------------------------------------

# Robustos agrupados por individuo:
coeftest(modelo_fe, vcov = vcovHC(modelo_fe, type = "HC1", cluster = "group"))

# También para RE:
coeftest(modelo_re, vcov = vcovHC(modelo_re, type = "HC1", cluster = "group"))

# Interpretación:
#   No cambia coeficientes.
#   Cambia errores estándar y p-valores.
#   Útil si hay heterocedasticidad o dependencia dentro del individuo.


# ------------------------------------------------------------
# Efectos fijos de dos vías
# ------------------------------------------------------------

modelo_fe_tw <- plm(
  y ~ x1 + x2 + x3,
  data = pdata,
  model = "within",
  effect = "twoways"
)

summary(modelo_fe_tw)

# Interpretación:
#   Controla:
#      - efectos fijos individuales,
#      - efectos fijos temporales.
#
# Útil si hay shocks comunes a todos los individuos en un año concreto.


# ============================================================
# 3) PLANTILLA CON VARIABLES DE LAS PRÁCTICAS
# ============================================================

# Ejemplo empresas:
# pdata <- pdata.frame(datos, index = c("Empresa", "Anio"))
#
# modelo_pool <- plm(Empleo ~ Salarios + Capital + Produccion,
#                    data = pdata, model = "pooling")
#
# modelo_fe <- plm(Empleo ~ Salarios + Capital + Produccion,
#                  data = pdata, model = "within")
#
# modelo_re <- plm(Empleo ~ Salarios + Capital + Produccion,
#                  data = pdata, model = "random")
#
# pFtest(modelo_fe, modelo_pool)
# plmtest(modelo_pool, type = "bp")
# phtest(modelo_fe, modelo_re)
#
# Conclusión tipo:
#   "Los contrastes rechazan pooling. Hausman rechaza RE.
#    Por tanto, elegimos efectos fijos."


##############################################################
##############################################################
############### TEMA 6: MODELOS DINÁMICOS ####################
##############################################################
##############################################################

# ============================================================
# 1) IDEA TEÓRICA
# ============================================================
# Modelo dinámico:
#
#   y_it = gamma y_i,t-1 + beta X_it + alpha_i + u_it
#
# La variable y_i,t-1 capta persistencia:
#   si gamma > 0, valores altos de y en el pasado tienden a asociarse
#   con valores altos de y en el presente.
#
# Problema:
#   y_i,t-1 está correlacionada con alpha_i y con el error transformado.
#   Por tanto, hay endogeneidad.
#
# Efectos fijos no basta cuando T es pequeño:
#   al transformar el modelo, y_i,t-1 queda correlacionada con el error.
#
# Solución:
#   usar variables instrumentales con retardos más antiguos:
#      y_i,t-2
#      y_i,t-2 - y_i,t-3
#
# Estos instrumentos se justifican porque:
#   - están correlacionados con y_i,t-1,
#   - bajo el modelo no están correlacionados con el error actual.
#
# En la práctica:
#   No siempre se hace test de weak instruments o Sargan,
#   porque los instrumentos vienen de la estructura dinámica.
#   Pero:
#     - comprobar inclusión/relevancia es recomendable,
#     - Sargan solo procede si hay sobreidentificación.
# ============================================================


# ============================================================
# 2) PANEL DINÁMICO EN LA FORMA DE LA PRÁCTICA
# ============================================================

# datos <- read_excel("RUTA/empresas.xlsx")
# pdata <- pdata.frame(datos, index = c("Empresa", "Anio"))

# Crear retardos:
pdata$y_1 <- lag(pdata$y, 1)
pdata$y_2 <- lag(pdata$y, 2)
pdata$y_3 <- lag(pdata$y, 3)

# Instrumento alternativo:
pdata$VI <- pdata$y_2 - pdata$y_3

# ------------------------------------------------------------
# Modelo dinámico con instrumento y_2
# ------------------------------------------------------------

modelo_fe_dyn_y2 <- plm(
  y ~ y_1 + x1 + x2 + x3 |
      y_2 + x1 + x2 + x3,
  data = pdata,
  model = "within"
)

summary(modelo_fe_dyn_y2)

# Interpretación:
#   y_1 es endógena.
#   y_2 actúa como instrumento.
#   x1, x2, x3 son exógenas y se instrumentan a sí mismas.
#
# Si y_1 es significativo:
#   hay evidencia de persistencia dinámica.
#
# Si y_1 no es significativo:
#   no hay evidencia clara de dinámica propia.
#   El modelo estático puede ser suficiente.


# ------------------------------------------------------------
# Modelo dinámico con instrumento VI = y_2 - y_3
# ------------------------------------------------------------

modelo_fe_dyn_VI <- plm(
  y ~ y_1 + x1 + x2 + x3 |
      VI + x1 + x2 + x3,
  data = pdata,
  model = "within"
)

summary(modelo_fe_dyn_VI)

# Interpretación:
#   VI usa más información retardada.
#   Inconveniente: se pierde un periodo adicional por usar y_3.
#
# Elección entre y_2 y VI:
#   depende de correlaciones y resultados.
#   Si ambos dan resultados parecidos, aumenta confianza.
#   Si difieren mucho, cuidado con debilidad o especificación.


# ------------------------------------------------------------
# Modelo dinámico con efectos aleatorios e instrumentos
# ------------------------------------------------------------

modelo_re_dyn_y2 <- plm(
  y ~ y_1 + x1 + x2 + x3 |
      y_2 + x1 + x2 + x3,
  data = pdata,
  model = "random"
)

summary(modelo_re_dyn_y2)

# Interpretación:
#   Solo sería razonable si los efectos individuales no están correlacionados
#   con las explicativas.
#   En la práctica, si Hausman prefería FE, el modelo dinámico FE suele ser
#   más defendible.


# ============================================================
# 3) EJEMPLO CON VARIABLES DE EMPRESAS
# ============================================================

# pdata <- pdata.frame(datos, index = c("Empresa", "Anio"))
#
# pdata$Empleo_1 <- lag(pdata$Empleo, 1)
# pdata$Empleo_2 <- lag(pdata$Empleo, 2)
# pdata$Empleo_3 <- lag(pdata$Empleo, 3)
# pdata$VI <- pdata$Empleo_2 - pdata$Empleo_3
#
# modelo_ef_re1 <- plm(
#   Empleo ~ Empleo_1 + Salarios + Capital + Produccion |
#            Empleo_2 + Salarios + Capital + Produccion,
#   data = pdata,
#   model = "within"
# )
#
# summary(modelo_ef_re1)
#
# modelo_ef_re2 <- plm(
#   Empleo ~ Empleo_1 + Salarios + Capital + Produccion |
#            VI + Salarios + Capital + Produccion,
#   data = pdata,
#   model = "within"
# )
#
# summary(modelo_ef_re2)


# ============================================================
# 4) EJEMPLO CON VARIABLES DE AEROLÍNEAS
# ============================================================

# pdatos2 <- pdata.frame(datos2, index = c("aerolinea", "anio"))
#
# pdatos2$Coste_1 <- lag(pdatos2$Coste, 1)
# pdatos2$Coste_2 <- lag(pdatos2$Coste, 2)
# pdatos2$Coste_3 <- lag(pdatos2$Coste, 3)
# pdatos2$VI <- pdatos2$Coste_2 - pdatos2$Coste_3
#
# modelo_ef_coste_y2 <- plm(
#   Coste ~ Coste_1 + Produccion + PrCombust + Carga |
#           Coste_2 + Produccion + PrCombust + Carga,
#   data = pdatos2,
#   model = "within"
# )
#
# summary(modelo_ef_coste_y2)
#
# modelo_ef_coste_VI <- plm(
#   Coste ~ Coste_1 + Produccion + PrCombust + Carga |
#           VI + Produccion + PrCombust + Carga,
#   data = pdatos2,
#   model = "within"
# )
#
# summary(modelo_ef_coste_VI)


# ============================================================
# 5) VERSIÓN EN PRIMERAS DIFERENCIAS + IVREG
# ============================================================
# Esta versión es más explícita:
#   1) diferencia para eliminar alpha_i,
#   2) instrumenta Delta y_{t-1} con y_{t-2}.
#
# Modelo:
#   Delta y_it = gamma Delta y_i,t-1 + beta Delta X_it + Delta u_it
#
# Problema:
#   Delta y_i,t-1 está correlacionada con Delta u_it.
#
# Instrumento:
#   y_i,t-2.
# ============================================================

# Supongamos que tenemos un data.frame normal datos_iv con:
#   d_y, d_L1_y, d_x1, d_x2, d_x3, L2_y

modelo_iv_diff <- ivreg(
  d_y ~ d_L1_y + d_x1 + d_x2 + d_x3 |
        L2_y + d_x1 + d_x2 + d_x3,
  data = datos_iv
)

summary(modelo_iv_diff, diagnostics = TRUE)
robust_HC1(modelo_iv_diff)

# Interpretación:
#   - IV corrige endogeneidad de d_L1_y.
#   - HC1 corrige errores estándar ante heterocedasticidad.
#   - Los coeficientes no cambian con HC1; cambian los errores estándar.
#
# Diagnostics:
#   Weak instruments:
#      mira si L2_y explica d_L1_y.
#   Wu-Hausman:
#      mira si realmente hay endogeneidad.
#   Sargan:
#      solo si hay más instrumentos excluidos que variables endógenas.


# ------------------------------------------------------------
# Primera etapa explícita en modelo dinámico
# ------------------------------------------------------------

primera_etapa_dyn <- lm(
  d_L1_y ~ L2_y + d_x1 + d_x2 + d_x3,
  data = datos_iv
)

summary(primera_etapa_dyn)

# Interpretación:
#   Si L2_y es significativo, el instrumento es relevante.
#   Si F de instrumentos es alto, menos preocupación por debilidad.


# ------------------------------------------------------------
# Si tengo más de un instrumento, puedo sobreidentificar
# ------------------------------------------------------------

modelo_iv_diff_over <- ivreg(
  d_y ~ d_L1_y + d_x1 + d_x2 + d_x3 |
        L2_y + VI + d_x1 + d_x2 + d_x3,
  data = datos_iv
)

summary(modelo_iv_diff_over, diagnostics = TRUE)
robust_HC1(modelo_iv_diff_over)

# Interpretación:
#   Aquí hay 1 endógena y 2 instrumentos excluidos.
#   Modelo sobreidentificado.
#   Sargan puede aparecer.
#
# Sargan:
#   H0: todos los instrumentos son válidos.
#   Si p >= 0.05: no rechazo validez conjunta.
#   Si p < 0.05: sospecha de instrumento inválido.


##############################################################
####################### RESÚMENES DE TESTS ###################
##############################################################

# ============================================================
# TESTS DE MCO
# ============================================================

# bptest(modelo)
#   H0: homocedasticidad.
#   p < 0.05 -> heterocedasticidad.
#
# jarque.bera.test(residuals(modelo))
#   H0: normalidad.
#   p < 0.05 -> no normalidad.
#
# coeftest(modelo, vcov = vcovHC(modelo, type="HC1"))
#   Errores robustos a heterocedasticidad.
#   No cambia coeficientes; cambia SE, t/z y p-valores.


# ============================================================
# TESTS DE IV
# ============================================================

# summary(ivreg(...), diagnostics = TRUE)
#
# Weak instruments:
#   H0: instrumentos débiles.
#   Regla: F > 10 suele ser razonable.
#
# Wu-Hausman:
#   H0: variable sospechosa es exógena.
#   p < 0.05 -> hay endogeneidad -> usar IV.
#   p >= 0.05 -> MCO puede ser preferible por eficiencia.
#
# Sargan:
#   H0: instrumentos válidos.
#   Solo si modelo sobreidentificado.
#   p < 0.05 -> problema con validez de instrumentos.
#   p >= 0.05 -> no rechazo validez.


# ============================================================
# TESTS DE LOGIT/PROBIT
# ============================================================

# logLik(modelo)
#   Cuanto mayor, mejor. Ojo: suele ser negativo; mayor = más cerca de cero.
#
# Pseudo-R2:
#   No se interpreta como R2 de MCO.
#
# LR test:
#   Compara modelo restringido vs no restringido.
#   H0: restricciones válidas.
#   p < 0.05 -> las variables excluidas importan.
#
# Efectos marginales:
#   slopes(modelo)
#   En Logit/Probit, usar efectos marginales para hablar de probabilidades.


# ============================================================
# TESTS DE PANEL
# ============================================================

# pFtest(modelo_fe, modelo_pool)
#   H0: todas las constantes individuales son iguales.
#   p < 0.05 -> usar FE frente a pooling.
#
# plmtest(modelo_pool, type="bp")
#   H0: var(alpha_i)=0.
#   p < 0.05 -> RE mejora frente a pooling.
#
# phtest(modelo_fe, modelo_re)
#   H0: cov(alpha_i, X)=0.
#   p < 0.05 -> usar FE.
#   p >= 0.05 -> RE puede usarse.
#
# vcovHC(..., cluster="group")
#   Errores robustos agrupados por individuo.


# ============================================================
# TESTS DE HECKMAN
# ============================================================

# Heckman 2 etapas:
#   Mirar lambda / IMR.
#   Si significativo -> selección endógena.
#
# Heckman ML:
#   Mirar rho.
#   Si rho significativo -> selección endógena.
#
# Variable de exclusión:
#   Debe estar en selección y no en resultado.
#   Debe explicar selección y poder excluirse razonablemente del resultado.


##############################################################
################# PLANTILLAS DE CONCLUSIÓN ###################
##############################################################

# ============================================================
# CONCLUSIÓN IV
# ============================================================
# "Se estima inicialmente por MCO, pero se sospecha endogeneidad de X
#  porque puede estar correlacionada con factores no observados del error.
#  Por ello se usa Z como instrumento. La primera etapa permite comprobar
#  la restricción de inclusión. El test de Wu-Hausman contrasta si realmente
#  existe endogeneidad. Si se rechaza exogeneidad, se prefiere IV/MC2E.
#  Si el modelo está sobreidentificado, Sargan permite contrastar la validez
#  conjunta de los instrumentos."


# ============================================================
# CONCLUSIÓN LOGIT/PROBIT
# ============================================================
# "Como la variable dependiente es binaria, el MPL puede usarse como
#  referencia, pero tiene problemas de predicciones fuera de [0,1] y
#  heterocedasticidad. Por eso se estiman modelos Logit/Probit, que
#  garantizan probabilidades válidas. Los coeficientes indican el signo
#  del efecto, pero la magnitud sobre la probabilidad debe interpretarse
#  mediante efectos marginales."


# ============================================================
# CONCLUSIÓN TOBIT
# ============================================================
# "Como la variable dependiente es continua pero presenta muchas observaciones
#  en cero, se compara MCO con Tobit. Tobit es adecuado si los ceros representan
#  censura o valores latentes negativos no observados. Sus coeficientes afectan
#  a la variable latente, por lo que para interpretar el efecto sobre la variable
#  observada conviene calcular efectos marginales sobre E(y|x)."


# ============================================================
# CONCLUSIÓN TWO-PART
# ============================================================
# "Si se considera que hay dos decisiones distintas, participar y cuánto
#  consumir condicionado a participar, el modelo de barrera es más flexible
#  que Tobit. Primero se estima un Probit para la probabilidad de y>0 y después
#  un MCO para la submuestra positiva. Este modelo es adecuado si los errores
#  de ambas ecuaciones no están correlacionados; si lo están, habría selección
#  endógena y convendría Heckman."


# ============================================================
# CONCLUSIÓN HECKMAN
# ============================================================
# "Se estima primero un OLS naive sobre la submuestra observada, pero esta
#  estimación puede estar sesgada si la selección no es aleatoria. Heckman
#  modeliza la selección mediante una ecuación Probit y corrige la ecuación
#  de resultado mediante la IMR/lambda. Si lambda o rho son significativos,
#  existe evidencia de selección muestral endógena y Heckman es preferible."


# ============================================================
# CONCLUSIÓN PANEL
# ============================================================
# "En datos de panel, el modelo pooling ignora la heterogeneidad individual.
#  Los efectos fijos permiten controlar características propias de cada individuo
#  constantes en el tiempo, incluso si están correlacionadas con las explicativas.
#  Los efectos aleatorios son más eficientes, pero solo son válidos si los efectos
#  individuales no están correlacionados con los regresores. El test de Hausman
#  decide entre FE y RE: si rechaza, se prefieren efectos fijos."


# ============================================================
# CONCLUSIÓN PANEL DINÁMICO
# ============================================================
# "El modelo dinámico incluye la variable dependiente retardada para captar
#  persistencia. Sin embargo, este retardo es endógeno en panel porque está
#  correlacionado con los efectos individuales y con el error transformado.
#  Por eso se usan instrumentos internos basados en retardos más antiguos,
#  como y_{t-2} o y_{t-2}-y_{t-3}. Si el coeficiente del retardo es significativo,
#  hay evidencia de persistencia dinámica. Si no lo es, el modelo estático puede
#  ser suficiente."


##############################################################
####################### FIN DE PLANTILLA #####################
##############################################################
