##############################################################
############   ENTREGA 3 - PRÁCTICA 3.4 (APTITUD) ###########
##############################################################
# Objetivo:
# Comparar las capacidades académicas de estudiantes matriculados
# en distintos programas educativos, usando como variable dependiente:
#   - apt: coeficiente de aptitud (200 a 800)
#
# Variables explicativas:
#   - read: puntuación en lectura
#   - math: puntuación en matemáticas
#   - prog: tipo de programa educativo
#
# Idea econométrica:
# Estimamos un modelo lineal:
# apt = beta0 + beta1*read + beta2*math + dummies de programa + u
#
# En lugar de crear dummies manualmente, usamos directamente el factor prog.
##############################################################

# ------------------------------------------------------------
# 0) Preparación
# ------------------------------------------------------------
rm(list = ls())

library(readxl)
library(lmtest)    # bptest
library(tseries)   # jarque.bera.test
library(car)       # linearHypothesis

# ------------------------------------------------------------
# 1) Lectura de datos
# ------------------------------------------------------------
datos <- read_excel("C:/Users/Usuario/GitHub/ucm/Econometría_Aplicada/ENTREGA_3/Práctica 3_4 Datos.xlsx")

# Vista rápida
print(head(datos))
str(datos)

# ------------------------------------------------------------
# 2) Preparación de variables
# ------------------------------------------------------------
# Aseguramos que prog sea factor
datos$prog <- as.factor(datos$prog)

# Vemos las categorías
levels(datos$prog)
table(datos$prog)

# IMPORTANTE:
# Para interpretar bien, fijamos "academic" como categoría de referencia
# (si en tu archivo aparece exactamente así).
datos$prog <- relevel(datos$prog, ref = "academic")

# Comprobamos niveles de nuevo
levels(datos$prog)

# ------------------------------------------------------------
# 3) Análisis exploratorio
# ------------------------------------------------------------
# Resúmenes básicos
summary(datos[, c("apt", "read", "math")])

# Media de apt por programa
aggregate(apt ~ prog, data = datos, mean)

# Boxplot de aptitud por programa
boxplot(apt ~ prog, data = datos,
        main = "Aptitud por tipo de programa",
        xlab = "Programa",
        ylab = "Aptitud")

# Relación apt con lectura
plot(datos$read, datos$apt,
     main = "Aptitud y lectura",
     xlab = "read",
     ylab = "apt",
     pch = 19, col = "blue")

# Relación apt con matemáticas
plot(datos$math, datos$apt,
     main = "Aptitud y matemáticas",
     xlab = "math",
     ylab = "apt",
     pch = 19, col = "darkgreen")

# ------------------------------------------------------------
# 4) Estimación del modelo lineal
# ------------------------------------------------------------
# Modelo:
# La categoría base es academic
# Entonces los coeficientes de proggeneral y progvocational
# se interpretan respecto a academic.
modelo <- lm(apt ~ read + math + prog, data = datos)
summary(modelo)

# ------------------------------------------------------------
# 5) Interpretación del modelo
# ------------------------------------------------------------
coef(modelo)

# Comentario para ti:
# - Intercepto: apt esperada para un alumno del programa academic con read=0 y math=0
# - read: efecto marginal de una unidad más en lectura, ceteris paribus
# - math: efecto marginal de una unidad más en matemáticas, ceteris paribus
# - proggeneral: diferencia media entre general y academic, controlando por read y math
# - progvocational: diferencia media entre vocational y academic, controlando por read y math

# ------------------------------------------------------------
# 6) Contrastes de interés
# ------------------------------------------------------------
# (a) ¿General difiere de Academic?
# Basta mirar el p-valor de proggeneral en summary(modelo)

# (b) ¿Vocational difiere de Academic?
# Basta mirar el p-valor de progvocational en summary(modelo)

# (c) ¿General y Vocational difieren entre sí?
linearHypothesis(modelo, "proggeneral = progvocational")

# ------------------------------------------------------------
# 7) Validación básica del modelo
# ------------------------------------------------------------
# Heterocedasticidad
bptest(modelo)

# Normalidad de residuos
residuos <- residuals(modelo)
jarque.bera.test(residuos)

# QQ-plot
qqnorm(residuos, main = "QQ-plot residuos")
qqline(residuos, col = "red")

# Histograma de residuos
hist(residuos,
     main = "Histograma de residuos",
     xlab = "Residuos")

# ------------------------------------------------------------
# 8) Predicción para un alumno medio según programa
# ------------------------------------------------------------
media_read <- mean(datos$read, na.rm = TRUE)
media_math <- mean(datos$math, na.rm = TRUE)

nuevos_datos <- data.frame(
  read = c(media_read, media_read, media_read),
  math = c(media_math, media_math, media_math),
  prog = factor(c("academic", "general", "vocational"),
                levels = levels(datos$prog))
)

nuevos_datos

predicciones <- predict(modelo, newdata = nuevos_datos, interval = "confidence", level = 0.95)
predicciones

# Unimos datos y predicciones
cbind(nuevos_datos, predicciones)

# ------------------------------------------------------------
# 9) Modelo alternativo con dummies manuales (solo por aprendizaje)
# ------------------------------------------------------------
# No hace falta usarlo, pero te lo dejo por si en examen te lo piden así.

datos$general <- ifelse(datos$prog == "general", 1, 0)
datos$vocational <- ifelse(datos$prog == "vocational", 1, 0)

modelo_dummies <- lm(apt ~ read + math + general + vocational, data = datos)
summary(modelo_dummies)

# Debe dar esencialmente lo mismo que el modelo con factor.