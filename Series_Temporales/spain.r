rm(list = ls())

# Se asigna el directorio donde están los datos

setwd("/Users/miguelfernandez/GitHub/ucm/Series_Temporales/")

# Carga de datos

datos <- read.csv("Spain2.csv",sep=",")
datos$FECHA <- as.Date(datos$fecha, "%m/%d/%Y")
head(datos)

#hola
datos$FECHA <- as.Date(datos$Date, "%Y-%m-%d")
datos$DEMANDA_GAS <- (datos$Price..EUR.MWh.)
head(datos)

##############################################
# [1]. Se representan gráficamente los datos #
##############################################

# install.packages("ggplot2")
library(ggplot2)

ggplot(aes(x= FECHA, y = DEMANDA_GAS), data = datos) + geom_line(color = '#d84519') + 
  xlab('FECHA') + ylab('Demanda de gas') + scale_x_date(date_breaks = '4 months', date_labels = "%m/%y")


#################################################################
# [2]. Se convierten los datos en un objeto de tipo time series #
#      para poder aplicar sobre ellos las funciones de R        #
#################################################################

datos.ts <- ts(datos$DEMANDA_GAS, frequency=7)
class(datos.ts)

###########################################################
# [3]. Se divide la muestra en entrenamiento y validación #
###########################################################

datos.train <- subset(datos, "2020-01-01">FECHA)
datos.train.ts <- ts(datos.train$DEMANDA_GAS, frequency = 7)

datos.validate <- subset(datos, FECHA<="2020-01-01")
datos.validate.ts <- ts(datos.validate$DEMANDA_GAS, frequency=7)


#############################################################
# [4]. Se estudia la estacionariedad en media y en varianza #
#      de la serie                                          #
#############################################################

# install.packages("tseries")
library(tseries)

# [4.1] ¿Es estacionaria en varianza?

# Se evalúa la necesidad de transformar la serie para hacerla
# estacionaria en varianza

# install.packages("MASS")
library(MASS)

box_cox <- boxcox(DEMANDA_GAS ~ FECHA,
                  data = datos.train,
                  lambda = c(0, 0.5, 1))

lambda <- box_cox$x[which.max(box_cox$y)]


## PROTIPS 

library(forecast)

mejor_mod   <- NULL
mejor_aicc  <- Inf
resultados  <- data.frame()

for (p in 0:2) {
  for (q in 0:2) {
    for (P in 0:1) {
      for (Q in 0:1) {
        mod <- try(
          Arima(datos.train.ts,
                order    = c(p,1,q),
                seasonal = list(order = c(P,1,Q), period = 7)),
          silent = TRUE
        )
        if (inherits(mod, "try-error")) next
        
        # Test de Ljung–Box sobre residuos
        lb <- Box.test(mod$residuals, lag = 24, type = "Ljung-Box")
        pval <- lb$p.value
        
        resultados <- rbind(resultados,
                            data.frame(p,q,P,Q,
                                       AICc = mod$aicc,
                                       p_LB = pval))
        
        # Solo consideramos modelos con residuos ~ ruido blanco
        if (pval > 0.05 && mod$aicc < mejor_aicc) {
          mejor_aicc <- mod$aicc
          mejor_mod  <- mod
        }
      }
    }
  }
}

mejor_mod
resultados[order(resultados$AICc), ][1:10, ]


ajuste1 <- Arima(datos.train.ts,
                 order = c(0,1,2),
                 seasonal = list(order = c(0,1,1), period = 7))

ajuste1

Acf(ajuste1$residuals, lag.max=25, xlab="Retardo", main="")
Pacf(ajuste1$residuals, lag.max=25, xlab="Retardo", main="")

coeftest(ajuste1)

cor.arma(ajuste1)


Box.test.2(residuals(ajuste1),
           nlag = c(6,12,18,24,30,36,42,48),
           type="Ljung-Box")






### (1,1,2) (0,1,1)
ajuste2 <- Arima(datos.train.ts,
                 order = c(1,1,2),
                 seasonal = list(order = c(0,1,1), period = 7))

ajuste2

Acf(ajuste2$residuals, lag.max=25, xlab="Retardo", main="")
Pacf(ajuste2$residuals, lag.max=25, xlab="Retardo", main="")

coeftest(ajuste2)

cor.arma(ajuste2)


Box.test.2(residuals(ajuste2),
           nlag = c(6,12,18,24,30,36,42,48),
           type="Ljung-Box")
