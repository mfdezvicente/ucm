##############################################################
############   ENTREGA 3 - PRÁCTICA 3.5 (PENSION) ###########
##############################################################
# Objetivo:
# Estudiar la brecha de género en las pensiones bajo dos supuestos:
# 1) Sin tener en cuenta la afiliación sindical.
# 2) Utilizando la afiliación sindical como identificador de truncamiento.
#
# Variables del fichero:
# - pension : pensión
# - male    : 1 si hombre, 0 si mujer
# - edad    : edad
# - tenure  : antigüedad
# - educ    : educación
# - union   : afiliación sindical
#
# Idea econométrica:
# - Hay muchas pensiones iguales a 0.
# - Si estimamos sin corregir selección, podemos sesgar la brecha de género.
# - Usamos union como variable de exclusión en la ecuación de selección.
##############################################################

# ------------------------------------------------------------
# 0) Preparación
# ------------------------------------------------------------
rm(list = ls())

library(readxl)
library(lmtest)
library(sandwich)

# Para Heckman:
# install.packages("sampleSelection")
# install.packages("mvtnorm")
library(sampleSelection)

# ------------------------------------------------------------
# 1) Lectura de datos
# ------------------------------------------------------------
datos <- read_excel("C:/Users/Usuario/GitHub/ucm/Econometría_Aplicada/ENTREGA_3/Práctica 3_5 Datos.xls")

# Vista rápida
print(head(datos))
str(datos)
summary(datos)

# ------------------------------------------------------------
# 2) Preparación de variables
# ------------------------------------------------------------
# Indicador de pensión positiva (selección)
datos$pension_pos <- ifelse(datos$pension > 0, 1, 0)

# Comprobar tamaños
table(datos$pension_pos)
table(datos$union)
table(datos$male)

# Tabla cruzada útil: pensión positiva y afiliación sindical
table(datos$union, datos$pension_pos)

# ------------------------------------------------------------
# 3) Exploración inicial
# ------------------------------------------------------------
# Histograma de pension
hist(datos$pension,
     main = "Histograma de pension",
     xlab = "pension")

# Boxplot de pension por sexo
boxplot(pension ~ male, data = datos,
        main = "Pensión por sexo",
        xlab = "male (0 = mujer, 1 = hombre)",
        ylab = "pension")

# Media de pensión por sexo (muestra completa)
aggregate(pension ~ male, data = datos, mean)

# Media de pensión positiva por sexo
aggregate(pension ~ male, data = subset(datos, pension > 0), mean)

# ------------------------------------------------------------
# 4) SUPUESTO 1: ignorar afiliación sindical / ignorar selección
# ------------------------------------------------------------
# Opción natural: estimar solo sobre quienes tienen pensión positiva.
# Esto equivale a ignorar que la entrada en ese subgrupo puede no ser aleatoria.

datos_pos <- subset(datos, pension > 0)

modelo_ols <- lm(pension ~ male + edad + tenure + educ, data = datos_pos)
summary(modelo_ols)

# Errores robustos por si quieres reportarlos
coeftest(modelo_ols, vcov = vcovHC(modelo_ols, type = "HC1"))

# Comentario:
# El coeficiente de male mide la brecha de género "naive"
# entre hombres y mujeres, condicionando por edad, tenure y educ,
# pero solo entre quienes tienen pensión positiva.

# ------------------------------------------------------------
# 5) SUPUESTO 2: Heckman usando union como identificador
# ------------------------------------------------------------
# Ecuación de selección:
# pension_pos = 1 si pension > 0
# Variables: male, edad, tenure, educ, union
#
# Ecuación principal (resultado):
# pension = beta0 + beta1*male + beta2*edad + beta3*tenure + beta4*educ + u
#
# La variable union entra en la selección y NO en la ecuación principal.
# Esa es la restricción de exclusión / identificador.

# ------------------------------------------------------------
# 5A) Heckman en dos etapas
# ------------------------------------------------------------
modelo_heckman_2s <- heckit(
  selection = pension_pos ~ male + edad + tenure + educ + union,
  outcome   = pension ~ male + edad + tenure + educ,
  data = datos
)

summary(modelo_heckman_2s)

# ------------------------------------------------------------
# 5B) Heckman por máxima verosimilitud
# ------------------------------------------------------------
modelo_heckman_ml <- selection(
  selection = pension_pos ~ male + edad + tenure + educ + union,
  outcome   = pension ~ male + edad + tenure + educ,
  data = datos,
  method = "ml"
)

summary(modelo_heckman_ml)

# ------------------------------------------------------------
# 6) Comparación de brecha de género
# ------------------------------------------------------------
# Comparamos el coeficiente de male en:
# - OLS (sin corregir selección)
# - Heckman 2-step
# - Heckman ML

coef(modelo_ols)["male"]

# Para heckit / selection, los nombres pueden salir algo distintos.
coef(modelo_heckman_2s)
coef(modelo_heckman_ml)

# ------------------------------------------------------------
# 7) Interpretación de la selección
# ------------------------------------------------------------
# Si el término de corrección (lambda / inverse Mills ratio) es significativo,
# hay evidencia de sesgo de selección.
#
# En el modelo ML también puedes fijarte en rho:
# - si rho es distinto de 0, hay correlación entre errores
#   de selección y de resultado, y por tanto sesgo de selección.

# ------------------------------------------------------------
# 8) Predicción ilustrativa
# ------------------------------------------------------------
# Predicción de pensión esperada para dos perfiles medios:
# mujer (male=0) y hombre (male=1), manteniendo el resto en medias

media_edad   <- mean(datos$edad, na.rm = TRUE)
media_tenure <- mean(datos$tenure, na.rm = TRUE)
media_educ   <- mean(datos$educ, na.rm = TRUE)
moda_union   <- as.numeric(names(which.max(table(datos$union))))

nuevos_datos <- data.frame(
  male   = c(0, 1),
  edad   = c(media_edad, media_edad),
  tenure = c(media_tenure, media_tenure),
  educ   = c(media_educ, media_educ),
  union  = c(moda_union, moda_union),
  pension_pos = c(1, 1)   # solo para que no moleste si algún método lo pide
)

nuevos_datos

# Predicción OLS (entre quienes tienen pensión positiva)
predict(modelo_ols, newdata = nuevos_datos, interval = "confidence")

# En Heckman, la predicción puede depender del método.
# Si te interesa solo comparar coeficientes, no hace falta complicarlo más.