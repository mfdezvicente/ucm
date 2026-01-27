##############################################################
############      PRÁCTICA 2 - ECONOMETRÍA   #################
############      MIGUEL FERNÁNDEZ VICENTE   #################
##############################################################


#ENUNCIUADO: 
#Dadas las características de los grandes vinos de reserva, puede tener sentido
#considerarlos como una inversión, aunque el vino realmente sea un bien de consumo.
#Estos vinos se almacenan durante bastante tiempo antes de ser consumidos, lo que
#incrementa su precio debido al coste de almacenaje, que supone un coste de
#oportunidad dada la posibilidad de inversiones alternativas. El fichero Bordeaux.csv
#contiene datos de la subasta de algunos vinos tintos de reserva de Burdeos de añadas
#entre 1952 y 1980. En concreto, las variables disponibles son: pr (precio del vino),
#lluvinv (Cantidad de lluvia caída en el invierno anterior a la cosecha), tempmed
#(Temperatura media durante el período de maduración de la uva), lluvcos (Cantidad
#de lluvia caída durante el período de maduración de la uva), edad (Número de años
#transcurridos desde la cosecha). Estudiar la rentabilidad de conservar el vino.


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


rm(list = ls())
df <- read.csv("/Users/miguelfernandez/GitHub/ucm/Econometría_Aplicada/Pr2_Datos Bordeaux.csv", sep = ";", dec= ",", na = "NA")
df
df$lpr = log(df$pr) # Creamos la variable log(pr)
modelo_mco = lm(lpr ~ lluvinv + tempmed + lluvcos + edad, data = df)
summary(modelo_mco) # Esto sirve para extraer el resultado
# edad         2.385e-02  7.167e-03   3.328  0.00306 ** 
# El coeficiente de la variable edad es positivo y significativo (p-valor 0.00306 < 0.05)
# Por tanto, concluimos que es rentable conservar el vino, ya que por cada año que pasa, el precio del vino aumenta un 2.385%

# Vamos a realizar un contraste de hipótesis para ver si los residuos son normales
# H0: Los residuos siguen una distribución normal
# H1: Los residuos no siguen una distribución normal
# Si el p-valor es menor que 0.05, rechazamos la H0
#install.packages("tseries")
library(tseries)
jarque.bera.test(residuals(modelo_mco))
# X-squared = 1.1023, df = 2, p-value = 0.5763
# El p-valor es 0.5763 > 0.05, por tanto no rechazamos la H0 y concluimos que los residuos siguen una distribución normal.
