#PRÁCTICA 2.3

#0) Borrar espacio
rm(list = ls())

#1) OBJETIVO
#Estudiar el efecto del consumo del tabaco de las madres en el peso de los bebés al nacer

#2) VARIABLES
library(readxl)
datos=read_excel("C:/Users/Jose/Dropbox/PC/Documents/EcoMates/Economía/Quinto/Econometría Aplicada/Tema 2/Práctica 2_3 bwght.xls",na=".")

#Explicativas:
#X1: Paquetes de tabaco fumados al días durante el embarazo (packs)
#X2: Indicador de si el bebé es niño (male)
#X3: Orden de nacimiento del bebé (parity)
#X4: Ingresos mensuales de la familia en log (lfaminc)

#Dependiente:
#Y: Peso del bebé (lbwght)


#3) TAMAÑO MUESTRAL
#Muestra de trabajadores mayores de 22 años de la región a estudiar
n=length(datos$lbwght)
n #1388

#4) MODELO

#A)
#1)Especificación
#Y=b0+b1X1+b2X2+b3X3+b4X3+u 
#2)Estimación de los coeficientes
modelo1=lm(lbwght~packs+male+parity+lfaminc, data=datos)
summary(modelo1)

#3) Validación del modelo

#Los coefs significativos
#R^2 bastante bajo, explica un 18,76%

library(tseries)
jarque.bera.test(residuals(modelo1))
#p-value < 2.2e-16, los errores no son normales, puede ser por endogeneidad

#Vamos a interpretar que la cov entre los packs y el error es distinto de cero

#pensamos en el instrumento cigprice, precio del cigarro
#regla de inclusión, el precio del cigarro correlacionado con los paquetes que se fuma la madre
#regla de exclusión, que no esté correlacionado con el peso del bebe directamente

modelo_biet=ivreg(lbwght~packs+male+parity+lfaminc|cigprice+male+parity+lfaminc,data=datos)
summary(modelo_biet,diagnostics='T')

#es un desastre, ninguna variable es significativa, los instrumentos son débiles

#nos ha faltado la primera etapa:

modelo_1e=lm(packs~cigprice+male+parity+lfaminc,data=datos)
summary(modelo_1e) #aquí podemos ver que la variable cigprice no tiene coeficiente significativo con packs
#mal instrumento cigprice

#si hicieramos el histograma de los residuos
hist(residuals(modelo1)) #colas bastante largas

#El modelo original estimado por MCO para predecir el logaritmo del peso del bebé (lbwght) 
#en función de los paquetes fumados (packs) presentaba problemas de no normalidad en los 
#residuos, lo que sugiere una posible endogeneidad por variable omitida. 

#Existen factores inobservables (como hábitos de salud generales de la madre, 
#nutrición o estrés) que están correlacionados tanto con la decisión de fumar como con 
#el peso final del bebé, provocando que Cov(packs, u) =! 0.Para solucionar este sesgo 
#de MCO, se planteó un modelo de Mínimos Cuadrados Bietápicos (MC2E) buscando instrumentos 
#que cumplieran la regla de relevancia y exclusión. Se propuso el precio del tabaco 
#(cigprice).Sin embargo, al estimar la Primera Etapa, 
#los resultados demuestran un claro problema de Instrumento Débil. 
#El precio del tabaco (p-valor = 0.84) no
#resulta ser variable estadísticamente significativa para explicar la cantidad de 
#paquetes fumados por las madres embarazadas en esta muestra. La demanda de tabaco en este 
#grupo poblacional se muestra totalmente inelástica al precio.

#Por lo tanto, la aplicación 
#de MC2E con estos instrumentos no es estadísticamente válida. Los instrumentos débiles 
#inflan drásticamente las varianzas de los estimadores en la segunda etapa, haciendo que 
#cualquier inferencia causal sea imprecisa. En conclusión, no disponemos de instrumentos 
#válidos en esta base de datos para aislar la exogeneidad del consumo de tabaco, debiendo 
#interpretar los resultados iniciales de MCO con cautela, asumiendo su potencial sesgo



