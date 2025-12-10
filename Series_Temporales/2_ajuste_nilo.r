#############################################
#############################################
#############################################
#             AJUSTE SERIE NILO             #
#############################################
#############################################
#############################################

# Se limpia el espacio de trabajo:

rm(list = ls())

# Se asigna el directorio donde están los datos
setwd("/Users/miguelfernandez/GitHub/ucm/Series_Temporales/")
datos <- read.csv("nilo.csv")

# Carga de datos

# Se reservan los 10 últimos años para hacer la predicción

datos.train <- subset(datos, ANYO<1275)

# Se convierte en un objeto de tipo timeseries

datos.train.ts <- as.ts(datos.train$NIVEL_NILO) # el dolar es para acceder a una columna concreta del dataframe

datos.test <- subset(datos, ANYO>=1275)
datos.test.ts <- as.ts(datos.test$NIVEL_NILO)

##############################################
##############################################
# [1]. Se representan gráficamente los datos #
##############################################
##############################################

#install.packages("ggplot2")
library(ggplot2)
ggplot(aes(x= ANYO, y = NIVEL_NILO), data = datos.train) + geom_line(color = '#d84519') + 
  xlab('ANYO') + ylab('NIVEL NILO')

#install.packages("plotly")
library(plotly)

graficoSerie <- ggplot(aes(x= ANYO, y = NIVEL_NILO), data = datos.train) +
  geom_line(color = '#d84519',
            size = 1) + 
  xlab('ANYO') + ylab('NIVEL NILO')
ggplotly(graficoSerie)

##############################################################
##############################################################
# [2]. Se estudia la estacionariedad en varianza de la serie #
##############################################################
##############################################################

# Se evalúa la necesidad de transformar la serie para hacerla
# estacionaria en varianza

#install.packages("MASS")
library(MASS)

box_cox <- boxcox(NIVEL_NILO ~ ANYO,
                  data = datos.train,
                  lambda = c(0, 0.5, 1),plotit=FALSE)

# If plotit = TRUE plots log-likelihood vs lambda and 
# indicates a 95% confidence interval about the maximum 
# observed value of lambda.

# El valor más alto de la verosimilitud se obtiene para lambda = 0 -> logaritmo

lambda <- box_cox$x[which.max(box_cox$y)]
lambda

datos.train$LOG_NIVEL_NILO=log(datos.train$NIVEL_NILO)
datos.train.ts <- as.ts(datos.train$LOG_NIVEL_NILO)

datos.test$LOG_NIVEL_NILO=log(datos.test$NIVEL_NILO)
datos.test.ts <- as.ts(datos.test$LOG_NIVEL_NILO)

#############################################
#############################################
# [3]. Ajuste de un modelo ARIMA a la serie #
#############################################
#############################################

# install.packages("forecast")
library(forecast)

graphics.off()

# Funciones de autocorrelación de la serie 

Acf(datos.train.ts, lag.max = 48, xlab = "Retardo",
    main= "Función de autocorrelación simple")

# SI LA ACF SE CORTA EN SECO DESPUES DEL LAG Q ES UN MA(Q)
# SI LA ACF SE DESVANECE ES UN AR(P)

# MA INFINITO

Pacf(datos.train.ts, lag.max = 48, xlab = "Retardo",
     main = "Función de autocorrelación parcial")

# AR(1)

ajuste_AR1 <- Arima(datos.train.ts,order = c(1,0,0))

# install.packages("lmtest")
library(lmtest)

coeftest(ajuste_AR1)

# install.packages("caschrono")
library(caschrono)

# Esta función se utilizará para calcular las correlaciones
# entre parámetros y también para realizar el test de ruido blanco

# Instrucciones si no se instala caschrono en tu versión de R
# Este paquete es el que permite instalar una versión antigua del repositorio de R

# install.packages("remotes")
# library(remotes)

# Instalar estos paquetes si dan problemas
# install.packages("zoo")
# install.packages("Hmisc")

# Indicar el paquete y la versión que se quiere instalar.
# Esta es la última disponible. Para saber cuál es se consulta aquí:
# https://cran.r-project.org/src/contrib/Archive/caschrono/

# install_version("caschrono", version = "2.4")

# En general puedes consultar todos los paquetes y versiones que han 
# existido en R aquí:
# https://cran.r-project.org/src/contrib/Archive/

library(caschrono)

cor.arma(ajuste_AR1)

# Funciones de autocorrelación del residuo tras el ajuste de un AR(1)

Acf(ajuste_AR1$residuals, lag.max = 48, xlab = "Retardo",
    main= "Función de autocorrelación simple")

Box.test.2(residuals(ajuste_AR1),
           nlag = c(6,12,18,24,30,36,42,48),
           type="Ljung-Box")

# No hay ruido blanco

graphics.off()

# Funciones de autocorrelación del residuo tras el ajuste de un AR(1)

Acf(ajuste_AR1$residuals, lag.max = 48, xlab = "Retardo",
    main= "Función de autocorrelación simple")

# MA(1) -> ARMA(1,1)

ajuste_AR1_MA1 <- Arima(datos.train.ts,order = c(1,0,1))

coeftest(ajuste_AR1_MA1)

cor.arma(ajuste_AR1_MA1)

# Correlación alta entre parámetros!

Acf(ajuste_AR1_MA1$residuals, lag.max = 48, xlab = "Retardo",
    main= "Función de autocorrelación simple")

Box.test.2(residuals(ajuste_AR1_MA1),
           nlag = c(6,12,18,24,30,36,42,48),
           type="Ljung-Box")

# Ruido blanco para alfa = 0,01

graphics.off()

# Funciones de autocorrelación del residuo tras el ajuste de un ARMA(1,1)

Acf(ajuste_AR1_MA1$residuals, lag.max = 48, xlab = "Retardo",
    main= "Función de autocorrelación simple")

# MA(2) -> ARMA(1,2)

Pacf(ajuste_AR1_MA1$residuals, lag.max = 48, xlab = "Retardo",
     main = "Función de autocorrelación parcial")

# O AR(2) -> ARMA(2,1)

# Empezamos con el ARMA(1,2)

ajuste_AR1_MA2 <- Arima(datos.train.ts,order = c(1,0,2))

coeftest(ajuste_AR1_MA2)

cor.arma(ajuste_AR1_MA2)

# Podría sugerirse un orden de diferenciación
# Las correlaciones entre los parámetros no son altas

Acf(ajuste_AR1_MA2$residuals, lag.max = 48, xlab = "Retardo",
    main= "Función de autocorrelación simple")

Box.test.2(residuals(ajuste_AR1_MA2),
           nlag = c(6,12,18,24,30,36,42,48),
           type="Ljung-Box")

# Ruido blanco

graphics.off()

# Probamos el ARMA(2,1)

ajuste_AR2_MA1 <- Arima(datos.train.ts,
                        order = c(2,0,1),
                        method = "ML")

coeftest(ajuste_AR2_MA1)

cor.arma(ajuste_AR2_MA1)

# Las correlaciones entre los parámetros son muy altas

Acf(ajuste_AR2_MA1$residuals, lag.max = 48, xlab = "Retardo",
    main= "Función de autocorrelación simple")

Box.test.2(residuals(ajuste_AR2_MA1),
           nlag = c(6,12,18,24,30,36,42,48),
           type="Ljung-Box")

# Se tiene ruido blanco, pero las correlaciones altas
# nos llevan a decantarnos por el ARMA(1,2)

graphics.off()

# También se puede tomar la decisión a la vista de los errores de predicción

###################
###################
# [4]. Predicción #
###################
###################

#############
# ARMA(1,2) #
#############

# Error sobre training

pred <- as.data.frame(ajuste_AR1_MA2$fitted)
names(pred) <- "prediccion"
errorTraining <- cbind(datos.train,pred)
errorTraining$APEDiario <- abs(100*(errorTraining$NIVEL_NILO-exp(errorTraining$prediccion))/errorTraining$NIVEL_NILO)
mean(errorTraining$APEDiario)

# 4.577018 #

# Error sobre test

prediccionTest <- as.data.frame(predict(ajuste_AR1_MA2, n.ahead=10))

# Cálculo de errores

errorTest <- cbind(datos.test,prediccionTest)
errorTest$APEDiario <- abs(100*(errorTest$NIVEL_NILO-exp(errorTest$pred))/errorTest$NIVEL_NILO)
mean(errorTest$APEDiario)

# 5.290345. A horizonte 1: 4.8874394 #

#############
# ARMA(2,1) #
#############

# Error sobre training

pred <- as.data.frame(ajuste_AR2_MA1$fitted)
names(pred) <- "prediccion"
errorTraining <- cbind(datos.train,pred)
errorTraining$APEDiario <- abs(100*(errorTraining$NIVEL_NILO-exp(errorTraining$prediccion))/errorTraining$NIVEL_NILO)
mean(errorTraining$APEDiario)

# 4.548331 < 4.577018 #

# Error sobre test

prediccionTest <- as.data.frame(predict(ajuste_AR2_MA1, n.ahead=10))

# Cálculo de errores

errorTest <- cbind(datos.test,prediccionTest)
errorTest$APEDiario <- abs(100*(errorTest$NIVEL_NILO-exp(errorTest$pred))/errorTest$NIVEL_NILO)
mean(errorTest$APEDiario)

# 5.437536>5.290345. A horizonte 1: 4.677131<4.8874394 #

# A horizonte 1, parece mejor el ARMA(2,1) porque proporciona #
# mejores errores tanto en TRAINING como en TEST #
# Promediando los errores de los 12 horizontes, el mejor #
# modelo parece ser el ARMA(1,2)

# Representación gráfica de la predicción del ARMA(2,1)

# Límites de confianza al 95%

U <- exp(prediccionTest$pred + 1.96*prediccionTest$se)
L <- exp(prediccionTest$pred - 1.96*prediccionTest$se)

# Se destransforma usando Nelson para que la predicción quede centrada en el intervalo

datos.pred <- data.frame(ANYO = datos.test$ANYO, 
                         Prediccion = exp(prediccionTest$pred+0.5*prediccionTest$se^2),
                         LimSup = U, LimInf =L)

datos.real.pred <- merge(datos, datos.pred, by = "ANYO", all.x = T)

library(plotly)

datos.real.pred$NIVEL_NILO[datos.real.pred$ANYO>1975] <- NA

graficoPrediccion <- ggplot(data = datos.real.pred) +
  geom_line(aes(x= ANYO, y = NIVEL_NILO), color = 'steelblue',
            alpha = 0.8, size = 0.8) +
  geom_line(aes(x= ANYO, y = Prediccion), color = 'darkred',
            size = 1)   +
  geom_line(aes(x= ANYO, y = LimSup), color = 'orange',
            size = 1)  +
  geom_line(aes(x= ANYO, y = LimInf), color = 'orange',
            size = 1) +
  xlab('ANYO') + ylab('NIVEL NILO')

ggplotly(graficoPrediccion)

######################################################
# Resultado de ajustar un modelo de forma automática #
######################################################

ajusteAutomatico <- auto.arima(datos.train.ts,
                               max.d=1,
                               max.p=2,
                               max.q=2,
                               ic="aic")

ajusteAutomatico

# Sale el ARIMA(2,1,1)
# No es ninguno de los modelos candidatos porque incluye diferenciación

ajusteAutomatico <- auto.arima(datos.train.ts,
                               max.d=0,
                               max.p=2,
                               max.q=2,
                               ic="aic")
ajusteAutomatico

# Si no permitimos diferenciación sale el modelo ARIMA(1,0,2) with non-zero mean
# que era uno de los que habíamos propuesto
# Sin embargo, debe tenerse en cuenta que esto no va a ser necesariamente así
# El modelo seleccionado es el que tiene menor AIC (no MAPE) en la muestra de 
# entrenamiento, sin atender a que satisfaga las propiedades estadísticas
# (significatividad de parámetros, estacionariedad e invertivilidad de los
# parámetros, ausencia de correlación entre ellos y superar el test de ruido blanco)