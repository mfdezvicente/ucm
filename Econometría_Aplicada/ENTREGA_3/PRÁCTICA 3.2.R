##############################################################
############   ENTREGA 3 - PRÁCTICA 3.2 (TABACO) ##############
##############################################################
# Objetivo:
# Estudiar el hábito de tabaquismo (FUMA) y realizar una predicción.
# FUMA: 0 = fuma, 1 = exfumador, 2 = nunca ha fumado
#
# Variables explicativas (según enunciado):
# EDAD, SUENO, ALCOHOL, EJERTL (y en el dataset también puede haber EJERTRA, ESTUDIO, etc.)
#
# Método:
# Logit multinomial (multinom, paquete nnet)
##############################################################

rm(list = ls())

# ------------------------------------------------------------
# 1) Lectura de datos
# ------------------------------------------------------------
datos <- read.table(
  "C:/Users/Usuario/GitHub/ucm/Econometría_Aplicada/ENTREGA_3/Práctica 3_2 Habitos_Tabaco.txt",
  header = TRUE, sep = "\t"
)
head(datos)
# Variable dependiente como factor
datos$FUMA <- factor(datos$FUMA)  # niveles: "0","1","2" (según tu salida)

# ------------------------------------------------------------
# 2) Estimación logit multinomial
# ------------------------------------------------------------
library(nnet)

# Por defecto toma como referencia el primer nivel (normalmente "0")
modelo_mult <- multinom(FUMA ~ EDAD + SUENO + ALCOHOL + EJERTL, data = datos)
summary(modelo_mult)

# Si quieres cambiar la categoría de referencia, por ejemplo "2" (nunca ha fumado):
datos$FUMA_ref2 <- relevel(datos$FUMA, ref = "2")
modelo_mult_ref2 <- multinom(FUMA_ref2 ~ EDAD + SUENO + ALCOHOL + EJERTL, data = datos)
summary(modelo_mult_ref2)

# ------------------------------------------------------------
# 3) Significatividad individual (z y p-valores)
# ------------------------------------------------------------
res <- summary(modelo_mult)
z <- res$coefficients / res$standard.errors

# p-valor bilateral correcto
p <- 2 * (1 - pnorm(abs(z)))

# Tabla bonita
tabla_z <- cbind(Coef = res$coefficients,
                 SE = res$standard.errors,
                 z = z,
                 p_value = p)
tabla_z

# ------------------------------------------------------------
# 4) Predicción: probabilidades y clase predicha
# ------------------------------------------------------------
# Probabilidades por clase (type = "probs")
prob_hat <- predict(modelo_mult, type = "probs")   # matriz NxK
head(prob_hat)

# Clase predicha (la de probabilidad máxima)
clase_hat <- predict(modelo_mult)  # factor con niveles 0/1/2

# Matriz de confusión SIN caret
mconf <- table(Real = datos$FUMA, Predicho = clase_hat)
mconf

# Accuracy (tasa de acierto)
acc <- mean(clase_hat == datos$FUMA)
acc

# ------------------------------------------------------------
# 5) Predicción para un nuevo individuo
# ------------------------------------------------------------
nuevo <- data.frame(EDAD = 30, SUENO = 7, ALCOHOL = 0, EJERTL = 2)

pred_prob <- predict(modelo_mult, newdata = nuevo, type = "probs")
pred_prob

pred_clase <- predict(modelo_mult, newdata = nuevo)
pred_clase