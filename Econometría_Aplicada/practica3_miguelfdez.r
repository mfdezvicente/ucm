##############################################################
############      PRÁCTICA 3 - ECONOMETRÍA   #################
############      MIGUEL FERNÁNDEZ VICENTE   #################
##############################################################


#ENUNCIUADO: 
#Se desea conocer qué factores influyen en el precio de una casa utilizando el fichero
# hprice1 (Wooldridge). 

#OBJETIVO: Queremos estudiar la rentabilidad de conservar el vino en función de las variables disponibles.
#VARIABLES: Cuantitativas: pr, lluvinv, tempmed, lluvcos, edad
# lnpr = beta0 + beta1*lluvinv + beta2*tempmed + beta3*lluvcos + beta4*edad + u
# Queremos estudiar del beta4 (edad) si es rentable conservar el vino. 
# El beta4 nos indica el incremento porcentual del precio del vino por cada año que pasa.
#TAMAÑO MUESTRAL: 30 observaciones
#MÉTODO DE ESTIMACION: Mínimos Cuadrados
#FASES
#1) ESPECIFICACIÓN
#2) ESTIMACIÓN
#3) VALIDACIÓN
#4) PREDICCIÓN


# 1) Especificación: 
# Price = b_0 + b_1 * assets + b_2 * bdrms +.......+ b_n*lsqrft + error
# Usaremos un método de regresión simple. 
# stimación de los coeficientes del modelo 

# install.packages ("readxl")
library (readxl)

datos = read_excel ("Datos Hprice1.xls")
datos
modelo = lm(Price~Assess + Bdrms + Lotsize + sqrft + colonial, data =datos)
summary (modelo)


library(car)
vif(modelo)

# Si una de las variables está entre 4 y 10 tenemos sospechas de que el modelo tenga multicolinealidad
# Si la multicolinealidad tiene más de 10 entonces habría multicolinealidad 

install.packages("olsrr")
library(olsrr)
ols_coll_diag(modelo)



modelo_2 = lm(Price~ + Bdrms + Lotsize + sqrft + colonial, data =datos)
summary(modelo_2)

# Quito el que tiene el p-valor más grande 


modelo_3 = lm(Price~  Bdrms + Lotsize + sqrft, data =datos)
summary(modelo_3)


modelo_4 = lm(Price~  Bdrms + Lotsize + sqrft +0, data =datos)
summary(modelo_4)

residuos = residuals(modelo_4)
jarque.bera.test(residuos)
qqnorm(residuals(modelo_4))
qqline(residuals(modelo_4, col ="red"))


res_ordenados4 <- sort(abs(residuos), decreasing = TRUE)
head(res_ordenados4)

datos$o76 <- 0
datos$o76[76] <- 1


datos$o42 <- 0
datos$o42[42] <- 1

datos$o73 <- 0
datos$o73[73] <- 1
modelo_5 = lm(Price~  Bdrms + Lotsize + sqrft +0 + o76 + o42 + o73, data =datos)
summary(modelo_5)


# Ahora sí que tenemos normalidad en los residuos lo que nos permite validar el modelo. 
residuos = residuals(modelo_5)
jarque.bera.test(residuos)
qqnorm(residuals(modelo_5))
qqline(residuals(modelo_5, col ="red"))