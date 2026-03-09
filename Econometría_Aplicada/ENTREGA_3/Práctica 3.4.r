# Especificación 

# apt = b_0 + b_1 * read + b_2 * math + b_3 academicao + b_4 * general
rm (list = ls())
library(readxl)
datos <- read_excel("/Users/miguelfernandez/GitHub/ucm/Econometría_Aplicada/ENTREGA_3/PrÃ¡ctica 3_4 Datos.xlsx")
datos

# Especificación 
# Nuestro objetivo es saber cuales son las variables que hacen variar la cantidad de las manzanas ecologicas 
# Para ello usaremos como variables regresoras: ecoprc, regprc, hhsize (tamaño del hogar esperando que está pueda influir en el número de manzadas que se consumen,


#Estimación
academico <- ifelse(datos$prog == "vocational", 1, 0)
general <- ifelse(datos$prog == "general", 1, 0)
modelo_inicial <- lm(apt ~ read + math + academico + general,data=datos)
summary(modelo_inicial)
