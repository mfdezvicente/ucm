##############
# PRÁCTICA 4 #
##############

#Con el fin de explicar el gasto en formación profesional de los trabajadores en un
#determinado sector económico, se ha recopilado la siguiente información para 150
#empresas de dicho sector:
#FORMA: Gasto en miles de euros que las empresas dedican a la formación profesional
#de sus trabajadores.
#INGRE: Ingresos de las empresas en miles de euros.
#TRABA: Nº de trabajadores de las empresas.
#PRODU: Índice de productividad de los trabajadores.
#La última vez que se realizó un estudio similar, se constató que la varianza del término
#de error del modelo era directamente proporcional al índice de productividad de los
#trabajadores. 


# FASES
# 1) Especificación del modelo.
# En este caso, es una regresión simple.
# 2) Estimación de los coeficientes del modelo.
# 3) Validación del modelo.
# 4) Predicción. 


# install.packages ("readxl")
library (readxl)

datos = read_excel ("Datos_pr4.xlsx")
datos


modelo = lm(FORMA~INGRE + PRODU + TRABA, data =datos)
summary (modelo)
# cOMO LA PRODUCTIVIDAD NOS ESTÁ DICIENDO QUE NO ES SIGNIFICATIVA, LA PODEMOS ELIMINAR 

modelo = lm(FORMA~INGRE + PRODU + TRABA, data =datos)
summary (modelo)

# QUEREMOS VER SI HAY HETEROCEDASTICIDAD. PARA ELLO USAMOS EL TEST DE HETEROCEDASTICIDAD DE BPO WHITE

library(lmtest)
bptest(modelo)

# PODEMOS RECHAZAR LA HIPOTESIS NULA: AFIRMANDO LA EXISTENCIA DE HETEROCESATICIDAD
# ES POR ELLO QUE COGEMOS LA PONDERACIÓN QUE NOS DIO EN EL EJECICIO.

modelo_p <- lm(FORMA ~ INGRE + PRODU + TRABA , data = datos, weights = 1/PRODU)
summary (modelo_p)
bptest(modelo_p)

# Ahora podemos ver que el test de BP nos da que el p-valor es 0.5 por tanto no podemos rechazar la hipotesis nula. 