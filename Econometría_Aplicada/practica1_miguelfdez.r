##############################################################
############      PRÁCTICA 1 - ECONOMETRÍA   #################
############      MIGUEL FERNÁNDEZ VICENTE   #################
##############################################################


#ENUNCIUADO: 
#Se ha recopilado información sobre la situación salarial de 40 empleados del sector bancario, recogida en las variables Salario (Y, en euros mensuales) y Experiencia laboral (X, en años de experiencia). Estimar un modelo que explique el nivel salarial en función de la experiencia.

#
# 1) OBJETIVO: Queremos saber cuál es la relación que se establece entre el salario y los años de experiencia en el sector bancario. 
# 2) VARIABLES: Cuantitativas y Cualitativas
# 3) TAMAÑO MUESTRAL
# 4) MÉTODO DE ESTIMACION: Mínimos Cuadrados o otro modelo. 


# FASES 
# 1) ESPECIFICACIÓN
# 2) ESTIMACIÓN 
# 3) VALIDACIÓN 
# 4) PREDICCIÓN
rm(list = ls())
#install.packages("readxl")
library(readxl)

df <- read_excel("/Users/miguelfernandez/GitHub/ucm/Econometría_Aplicada/Pr1_Datos.xlsx")      
df
# Calculamos la Regresión Lineal mediante lm
regresion = lm(y ~ x, data = df)
summary(regresion) # Esto sirve para extraer el resultado

#Estimaciones por el método de los mínimos cuadrados. 
coeficientes = coef(regresion)
coeficientes[1] #Intercepto. Sería el salario que recbiríamos si tuvieramos 0 años de experiencia. 
coeficientes[2] #Pendiente. Con esto podemos ver que por cada unidad de experiencia, el salario aumentaría en 116.86

# Vamos a ver un contraste de hipótesis de los residuos de J-B. El H0 es la normalidad. Por tanto, si el p-valor es menor que 0.05 rechazamos la H0 y concluimos que no hay normalidad en los residuos. 

#install.packages("tseries")
library(tseries)
jarque.bera.test(residuals(regresion))

# El p-valor es muy pequeño 1.58e-05 por tanto concluimos que no hay normalidad de los residuos. 

# Dibujamos una nube de puntos 
plot(df$x, df$y, main = "NUBE DE PUNTOS", xlab= "Años", ylab = "Salario", col = "red")
abline(regresion, col = "blue")
