############################################################################
# Empleo de la metodología Box-Jenkins para el ajuste de un modelo SARIMA  #
# a la serie que mide la demanda de gas diaria en zona de la C.Valenciana  #
############################################################################

# Se limpia el espacio de trabajo:

rm(list = ls())

# Se asigna el directorio donde están los datos

setwd("/Users/miguelfernandez/GitHub/ucm/Series_Temporales/")

# Carga de datos

datos <- read.csv("demandaGasIndustrial.csv",sep=";")

# Se convierte la FECHA de factor a objeto de tipo FECHA

datos$FECHA <- as.Date(datos$FECHA, "%m/%d/%Y")
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

datos.train <- subset(datos, "1996-07-01"<=FECHA & FECHA<="1999-06-30")
datos.train.ts <- ts(datos.train$DEMANDA_GAS, frequency = 7)

datos.validate <- subset(datos, FECHA>"1999-06-30")
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
lambda

# -> No es necesario transformar los datos

# [4.2] ¿Es estacionaria en media? (¿Hay que diferenciarla?)

# Test de Dickey-Fuller

# install.packages("urca")
library(urca)

# Función para obtener p-valores de ADF

adf_all_tests <- function(y, max_lags = "AIC") {
  types <- c("none", "drift", "trend")  # Tipos de prueba
  names_types <- c("Sin Intercepto", "Con Intercepto", "Con Intercepto y Tendencia")
  
  # Lista para guardar los p-valores
  p_values <- numeric(3)
  
  for (i in 1:3) {
    # Ejecutar el test ADF con el tipo correspondiente
    test <- ur.df(y, type = types[i], selectlags = max_lags)
    
    # Obtener el resumen del test
    summary_test <- summary(test)
    
    # Inspeccionar el nombre exacto del coeficiente y p-valor
    coef_table <- summary_test@testreg$coefficients
    print(coef_table)  # Para ver cómo están organizados los coeficientes
    
    # Acceder al p-valor asociado al coeficiente 'y.lag.1'
    p_values[i] <- coef_table[1, "Pr(>|t|)"]  # Ajusta el índice si es necesario
  }
  
  # Crear un data.frame con los resultados
  data.frame(Test = names_types, P_value = p_values)
}

adf_p_values <- adf_all_tests(datos.train.ts)
print(adf_p_values)

# p-valor bajo < 0.05 -> es estacionaria en media

# [4.3] ¿Es estacionaria en covarianza? (¿Hay que diferenciarla estacionalmente?)

adf_seasonal <- ur.df(datos.train.ts, type = "drift", lags = 7)
summary(adf_seasonal)

# Como el p-valor es bajo, podría no ser necesario hacer una diferencia estacional

#############################################
# [5]. Ajuste de un modelo ARIMA a la serie #
#############################################

# [5.1]. Funciones de autocorrelación simple y parcial

# install.packages("forecast")
library(forecast)

Acf(datos.train.ts, lag.max = 25, xlab = "Retardo",
    main= "Función de autocorrelación simple")

Pacf(datos.train.ts, lag.max = 25, xlab = "Retardo",
     main = "Función de autocorrelación parcial")

# [5.2]. Ajuste de modelo ARIMA

# [5.2.1]. AR(1)

# install.packages("forecast")
library(forecast)

ajuste1 <- Arima(datos.train.ts,
                 order = c(1,0,0))
ajuste1

# Para ver p-valores de los parámetros

# install.packages("lmtest")
library(lmtest)

coeftest(ajuste1)

# Matriz de correlación de parámetros estimados

# install.packages("caschrono")
library(caschrono)

cor.arma(ajuste1)

# Test de ruido blanco

graphics.off()

Acf(ajuste1$residuals, lag.max = 25, xlab = "Retardo", main="")
Pacf(ajuste1$residuals, lag.max = 25, xlab = "Retardo", main="")

Box.test.2(residuals(ajuste1),
           nlag = c(6,12,18,24,30,36,42,48),
           type="Ljung-Box")

# [5.2.2]. SAR(1)x(1)[7]

ajuste2 <- Arima(datos.train.ts,
                 order = c(1,0,0),
                 seasonal = list(order = c(1,0,0), period = 7))

ajuste2
coeftest(ajuste2)

# Matriz de correlación de parámetros estimados

cor.arma(ajuste2)

# Test de ruido blanco

Acf(ajuste2$residuals, lag.max=25, xlab="Retardo", main="")
Pacf(ajuste2$residuals, lag.max=25, xlab="Retardo", main="")

Box.test.2(residuals(ajuste2),
           nlag = c(6,12,18,24,30,36,42,48),
           type="Ljung-Box")

# [5.2.3]. SARIMA(1,0,0)x(1,0,1)[7]

graphics.off()

ajuste3 <- Arima(datos.train.ts,
                 order = c(1,0,0),
                 seasonal = list(order = c(1,0,1), period = 7))

# Problema de convergencia. Cambiamos el método de estimación de parámetros

ajuste3 <- Arima(datos.train.ts,
                 order = c(1,0,0),
                 method="ML",
                 seasonal = list(order = c(1,0,1), period = 7))

ajuste3
coeftest(ajuste3)

# Raíz unitaria -> Se reemplaza AR(7) por diferencia estacional

# [5.2.4]. SARIMA(1,0,0)x(0,1,1)[7]

graphics.off()

ajuste4 <- Arima(datos.train.ts,
                 order = c(1,0,0),
                 seasonal = list(order = c(0,1,1), period = 7),
                 include.constant = T)

coeftest(ajuste4)

# Sí es significativa

# Matriz de correlación de parámetros estimados

cor.arma(ajuste4)

# Test de ruido blanco

graphics.off()

Acf(ajuste4$residuals, lag.max=25, xlab="Retardo", main="")
Pacf(ajuste4$residuals, lag.max=25, xlab="Retardo", main="")

Box.test.2(residuals(ajuste4),
           nlag = c(6,12,18,24,30,36,42,48),
           type="Ljung-Box")

# [5.2.5]. SARIMA(1,0,0)x(1,1,1)[7]

ajuste5 <- Arima(datos.train.ts,
                 order = c(1,0,0),
                 seasonal = list(order = c(1,1,1), period = 7),
                 include.constant = T)

ajuste5
coeftest(ajuste5)

# Matriz de correlación de parámetros estimados

cor.arma(ajuste5)

# Test de ruido blanco

graphics.off()

Acf(ajuste5$residuals, lag.max=25, xlab="Retardo", main="")
Pacf(ajuste5$residuals, lag.max=25, xlab="Retardo", main="")

Box.test.2(residuals(ajuste5),
           nlag = c(6,12,18,24,30,36,42,48),
           type="Ljung-Box")

# [5.2.6]. SARIMA(1,0,2)x(1,1,1)[7]

ajuste6 <- Arima(datos.train.ts,
                 order = c(1,0,2),
                 seasonal = list(order = c(1,1,1), period=7),
                 include.constant = T)

ajuste6
coeftest(ajuste6)

# Matriz de correlación de parámetros estimados

cor.arma(ajuste6)

# Test de ruido blanco

Acf(ajuste6$residuals, lag.max=25, xlab="Retardo", main="")
Pacf(ajuste6$residuals, lag.max=25, xlab="Retardo", main="")

Box.test.2(residuals(ajuste6),
           nlag = c(6,12,18,24,30,36,42,48),
           type="Ljung-Box")

# MODELO ALTERNATIVO: Se cambia el AR(1) por una diferencia regular

graphics.off()

ajusteAlt <- Arima(datos.train.ts,
                   order = c(0,1,2),
                   seasonal = list(order = c(1,1,1), period=7),
                   include.constant = T)


# ajusteAlt <- Arima(datos.train.ts,
#                    order = c(0,1,2),
#                    seasonal = list(order = c(1,1,1), period=7),
#                    include.mean = T)

# ajusteAlt <- Arima(datos.train.ts,
#                    order = c(0,1,2),
#                    seasonal = list(order = c(1,1,1), period=7),
#                    include.drift = T)

# Aquí , ni con el include.constant ni con el include.mean, ni con el include.drift
# deja meter las diferencias porque considera que son "demasiadas diferencias"
# para que el parámetro sea signifactivo
# Aviso: No drift term fitted as the order of difference is 2 or more.

ajusteAlt
coeftest(ajusteAlt)

# Matriz de correlación de parámetros estimados

cor.arma(ajusteAlt)

# Test de ruido blanco

Acf(ajusteAlt$residuals, lag.max=25, xlab="Retardo", main="")
Pacf(ajusteAlt$residuals, lag.max=25, xlab="Retardo", main="")

Box.test.2(residuals(ajusteAlt),
           nlag = c(6,12,18,24,30,36,42,48),
           type="Ljung-Box")

#############################################
# [6]. Inclusión de variables de calendario #
#############################################

# Se definen los regresores (festividades y temperaturas)
# Dejamos FECHA, TMAX y TMIN

calendario <- datos[,c(-2)]

# Se crean el día de la semana, el mes, etc, y a partir de
# ellos se definen los festivos

# install.packages("lubridate")
library(lubridate)

calendario$diaSemana <- wday(calendario$FECHA)
calendario$diaMes <- day(calendario$FECHA)
calendario$mes <- month(calendario$FECHA)

# Se definen los festivos nacionales

calendario$p_01ene <- ifelse(calendario$diaMes==1 & calendario$mes==1& !calendario$diaSemana %in% c(1, 7), 1, 0)
calendario$p_06ene <- ifelse(calendario$diaMes==6 & calendario$mes==1 & !calendario$diaSemana %in% c(1, 7), 1, 0)
calendario$p_19mar <- ifelse(calendario$diaMes==19 & calendario$mes==3 & !calendario$diaSemana %in% c(1, 7), 1, 0)
calendario$p_01may <- ifelse(calendario$diaMes==1 & calendario$mes==5 & !calendario$diaSemana %in% c(1, 7), 1, 0)
calendario$p_15ago <- ifelse(calendario$diaMes==15 & calendario$mes==8 & !calendario$diaSemana %in% c(1, 7), 1, 0)
calendario$p_09oct <- ifelse(calendario$diaMes==9 & calendario$mes==10 & !calendario$diaSemana %in% c(1, 7),1, 0)
calendario$p_12oct <- ifelse(calendario$diaMes==12 & calendario$mes==10 & !calendario$diaSemana %in% c(1, 7),1, 0)
calendario$p_01nov <- ifelse(calendario$diaMes==1 & calendario$mes==11 & !calendario$diaSemana %in% c(1, 7), 1 ,0)
calendario$p_06dic <- ifelse(calendario$diaMes==6 & calendario$mes==12 & !calendario$diaSemana %in% c(1, 7), 1 ,0)
calendario$p_08dic <- ifelse(calendario$diaMes==8 & calendario$mes==12 & !calendario$diaSemana %in% c(1, 7), 1 ,0)
calendario$p_25dic <- ifelse(calendario$diaMes==25 & calendario$mes==12 & !calendario$diaSemana %in% c(1, 7), 1 ,0)

#######################################
#     Cálculo de la Semana Santa      #
#######################################

# Función Easter de timeDate

# # install.packages("timeDate")
library(timeDate)

domingoResurrecion <- as.Date(Easter(year(min(calendario$FECHA)):year(max(calendario$FECHA))))
juevesSanto <- domingoResurrecion-3
juevesSanto <- data.frame(FECHA=juevesSanto, juevesSanto=rep(1,length(juevesSanto)))
viernesSanto <- domingoResurrecion-2
viernesSanto <- data.frame(FECHA=viernesSanto, viernesSanto=rep(1,length(viernesSanto)))
lunesPascua <- domingoResurrecion+1
lunesPascua <- data.frame(FECHA=lunesPascua, lunesPascua=rep(1,length(lunesPascua)))

# Se añaden a la tabla maestra de fechas

calendario <- merge(x = calendario, y = juevesSanto, by = "FECHA", all.x = TRUE)
calendario <- merge(x = calendario, y = viernesSanto, by = "FECHA", all.x = TRUE)
calendario <- merge(x = calendario, y = lunesPascua, by = "FECHA", all.x = TRUE)

# Se reemplazan los NAs por 0

calendario$juevesSanto[is.na(calendario$juevesSanto)] <- 0
calendario$viernesSanto[is.na(calendario$viernesSanto)] <- 0
calendario$lunesPascua[is.na(calendario$lunesPascua)] <- 0

variablesExcluidas <- names(calendario) %in% c("diaSemana", "diaMes", "mes", "anyo") 
calendario <- calendario[!variablesExcluidas]

calendario.train <- subset(calendario, "1996-07-01"<=FECHA & FECHA<="1999-06-30")
calendario.train <- as.matrix(calendario.train[,4:ncol(calendario.train)])

# Se añaden los festivos al Arima ajustado:

ajuste6ConFestivos <- Arima(datos.train.ts,
                            order = c(1,0,2),
                            seasonal = list(order = c(1,1,1), period=7),
                            method="ML",
                            xreg = calendario.train,
                            include.constant = T)

ajuste6ConFestivos
coeftest(ajuste6ConFestivos)

Acf(ajuste6ConFestivos$residuals, lag.max=25, xlab="Retardo", main="")
Pacf(ajuste6ConFestivos$residuals, lag.max=25, xlab="Retardo", main="")

Box.test.2(residuals(ajuste6ConFestivos),
           nlag = c(6,12,18,24,30,36,42,48),
           type="Ljung-Box")

# El MA(2) y el Jueves Santo no son significativos (está en la columna 14)
# Pero teniendo signo negativo, se podría dejar

# Si se quiere quitar el Jueves Santo

# ajuste6ConFestivos <- Arima(datos.train.ts,
#                             order = c(1,0,1),
#                             seasonal = list(order = c(1,1,1), period=7),
#                             xreg = calendario.train[,c(1:13,15:16)])

ajuste6ConFestivos <- Arima(datos.train.ts,
                            order = c(1,0,1),
                            seasonal = list(order = c(1,1,1), period=7),
                            xreg = calendario.train,
                            include.constant = T)

ajuste6ConFestivos
coeftest(ajuste6ConFestivos)

Acf(ajuste6ConFestivos$residuals, lag.max=25, xlab="Retardo", main="")
Pacf(ajuste6ConFestivos$residuals, lag.max=25, xlab="Retardo", main="")

Box.test.2(residuals(ajuste6ConFestivos),
           nlag = c(6,12,18,24,30,36,42,48),
           type="Ljung-Box")

##############################################
# [7]. Inclusión de variables de temperatura #
##############################################

calendario$TMAX_1=(dplyr::lag(calendario$TMAX, 1))
calendario$TMAX_2=(dplyr::lag(calendario$TMAX, 2))
calendario$TMAX_1[is.na(calendario$TMAX_1)] <- 0
calendario$TMAX_2[is.na(calendario$TMAX_2)] <- 0

calendario$TMIN_1=(dplyr::lag(calendario$TMIN, 1))
calendario$TMIN_2=(dplyr::lag(calendario$TMIN, 2))
calendario$TMIN_1[is.na(calendario$TMIN_1)] <- 0
calendario$TMIN_2[is.na(calendario$TMIN_2)] <- 0

variablesExcluidas <- names(calendario) %in% c("diaSemana", "diaMes", "mes", "anyo") 
calendario <- calendario[!variablesExcluidas]

calendario.train <- subset(calendario, "1996-07-01"<=FECHA & FECHA<="1999-06-30")
calendario.train <- as.matrix(calendario.train[,2:ncol(calendario.train)])
names(as.data.frame(calendario.train))

# Se añaden las temperaturas al Arima ajustado:

ajuste6ConFestivosYTemperaturas <- Arima(datos.train.ts,
                                         order = c(1,0,1),
                                         seasonal = list(order = c(1,1,1), period=7),
                                         xreg = calendario.train,
                                         include.constant = T)

ajuste6ConFestivosYTemperaturas
coeftest(ajuste6ConFestivosYTemperaturas)

# Solo es significativa TMAX, TMIN y TMAX_1
# Quitamos TMAX_2, TMIN_1 y TMIN_2

ajuste6ConFestivosYTemperaturas <- Arima(datos.train.ts,
                                         order = c(1,0,1),
                                         seasonal = list(order = c(1,1,1), period=7),
                                         xreg = calendario.train[,c(3:17)],
                                         include.constant = T)

ajuste6ConFestivosYTemperaturas
coeftest(ajuste6ConFestivosYTemperaturas)

# Y ahora ni siquiera TMAX_1

ajuste6ConFestivosYTemperaturas <- Arima(datos.train.ts,
                                         order = c(1,0,1),
                                         seasonal = list(order = c(1,1,1), period=7),
                                         xreg = calendario.train[,c(3:16)],
                                         include.constant = T)
ajuste6ConFestivosYTemperaturas
coeftest(ajuste6ConFestivosYTemperaturas)

Acf(ajuste6ConFestivosYTemperaturas$residuals, lag.max=25, xlab="Retardo", main="")
Pacf(ajuste6ConFestivosYTemperaturas$residuals, lag.max=25, xlab="Retardo", main="")

Box.test.2(residuals(ajuste6ConFestivosYTemperaturas),
           nlag = c(6,12,18,24,30,36,42,48),
           type="Ljung-Box")

# Comparativa del test de ruido blanco con el modelo alternativo

ajuste6AltConFestivosYTemperaturas <- Arima(datos.train.ts,
                                            order = c(0,1,2),
                                            seasonal = list(order = c(1,1,1), period=7),
                                            xreg = calendario.train)

ajuste6AltConFestivosYTemperaturas
coeftest(ajuste6AltConFestivosYTemperaturas)

# El MA(2) se vuelve a perder

ajuste6AltConFestivosYTemperaturas <- Arima(datos.train.ts,
                                            order = c(0,1,1),
                                            seasonal = list(order = c(1,1,1), period=7),
                                            xreg = calendario.train[,c(1:17)])

# Nuevamente siquiera TMAX_1 no es significativa

ajuste6AltConFestivosYTemperaturas <- Arima(datos.train.ts,
                                            order = c(0,1,1),
                                            seasonal = list(order = c(1,1,1), period=7),
                                            xreg = calendario.train[,c(1:16)])

ajuste6AltConFestivosYTemperaturas
coeftest(ajuste6AltConFestivosYTemperaturas)

Acf(ajuste6AltConFestivosYTemperaturas$residuals, lag.max=25, xlab="Retardo", main="")
Pacf(ajuste6AltConFestivosYTemperaturas$residuals, lag.max=25, xlab="Retardo", main="")

Box.test.2(residuals(ajuste6AltConFestivosYTemperaturas),
           nlag = c(6,12,18,24,30,36,42,48),
           type="Ljung-Box")

# Este test de RB parece mejor

###################################
# [8]. Identificación de outliers #
###################################

# install.packages("tsoutliers")
library(tsoutliers)

# Esto identifica outliers

listaOutliers <- locate.outliers(ajuste6AltConFestivosYTemperaturas$residuals,
                                 pars = coefs2poly(ajuste6ConFestivosYTemperaturas),
                                 types = c("AO", "LS", "TC"),cval=5)
listaOutliers

# Para incluir los outliers. No es necesario porque ya se tenía RB

outliers <- outliers(c("AO", "TC"), c(906, 177))
outliersVariables <- outliers.effects(outliers, length(ajuste6AltConFestivosYTemperaturas$residuals))
calendarioMasOutliers <- cbind(calendario.train[,c(1:16)],outliersVariables)
ajuste6AltConFestivosYTemperaturasYOutliers <- 
                        Arima(datos.train.ts,
                              order = c(0,1,1),
                              seasonal = list(order = c(1,1,1), period=7),
                              xreg = calendarioMasOutliers,
                              include.constant = T)


ajuste6AltConFestivosYTemperaturasYOutliers
coeftest(ajuste6AltConFestivosYTemperaturasYOutliers)

Acf(ajuste6AltConFestivosYTemperaturasYOutliers$residuals, lag.max=25, xlab="Retardo", main="")
Pacf(ajuste6AltConFestivosYTemperaturasYOutliers$residuals, lag.max=25, xlab="Retardo", main="")

Box.test.2(residuals(ajuste6AltConFestivosYTemperaturasYOutliers),
           nlag = c(6,12,18,24,30,36,42,48),
           type="Ljung-Box")

# Al 0.01 es Ruido blanco

###########################
# [9]. Fase de predicción #
###########################

horizontePrediccion <- 7

calendario.validate <- subset(calendario,FECHA>"1999-06-30" & FECHA<= "1999-07-07")
outliersVariablesValidate <- outliersVariables[(nrow(outliersVariables)-horizontePrediccion+1):
                                                nrow(outliersVariables),]
calendarioMasOutliersValidate <- as.matrix(cbind(calendario.validate[,c(2:17)],outliersVariablesValidate))

# [9.1]. Sin explicativas de ningún tipo

prediccion <- as.data.frame(predict(ajusteAlt, n.ahead=horizontePrediccion))

# Límites de confianza al 95%

U <- prediccion$pred + 2*prediccion$se
L <- prediccion$pred - 2*prediccion$se

ts.plot(tail(datos.train.ts,30), prediccion$pred, U, L, col=c(1,2,4,4), lty = c(1,1,2,2))
legend("topleft", c("Actual", "Forecast", "Error Bounds (95% Confidence)"),
       col=c(1,2,4), lty=c(1,1,2))

# [9.2]. Incluyendo festivos, temperaturas y outliers

prediccionConExplicativas <- as.data.frame(
  predict(ajuste6AltConFestivosYTemperaturasYOutliers,
  newxreg = calendarioMasOutliersValidate,
  n.ahead= horizontePrediccion))

# Límites de confianza al 95%

U <- prediccionConExplicativas$pred + 2*prediccionConExplicativas$se
L <- prediccionConExplicativas$pred - 2*prediccionConExplicativas$se

ts.plot(tail(datos.train.ts,30), prediccionConExplicativas$pred, U, L, col=c(1,2,4,4), lty = c(1,1,2,2))
legend("topleft", c("Actual", "Forecast", "Error Bounds (95% Confidence)"),
       xjust = 0, yjust = 1,
       col=c(1,2,4), lty=c(1,1,2))

par(mfrow=c(2,1))
ts.plot(tail(datos.train.ts,30), prediccion$pred, U, L, col=c(1,2,4,4), lty = c(1,1,2,2))
ts.plot(tail(datos.train.ts,30), prediccionConExplicativas$pred, U, L, col=c(1,2,4,4), lty = c(1,1,2,2))

###################################
# [10]. Cálculo de errores (MAPE) #
###################################

# [10.1]. Error in-sampling

# Sin incluir explicativas

pred <- as.data.frame(ajusteAlt$fitted)
names(pred) <- "DEMANDA_GAS_PRED"
realYPrediccion <- cbind(datos.train,pred)
realYPrediccion$MAPEDiario <- abs(100*(realYPrediccion$DEMANDA_GAS-realYPrediccion$DEMANDA_GAS_PRED)/realYPrediccion$DEMANDA_GAS)

mean(realYPrediccion$MAPEDiario)

# 3.020346

# Incluyendo festivos, temperaturas y outliers

predConExplicativas <- as.data.frame(ajuste6AltConFestivosYTemperaturasYOutliers$fitted)
names(predConExplicativas) <- "DEMANDA_GAS_PRED"
realYPrediccionConExplicativas <- cbind(datos.train,predConExplicativas)
realYPrediccionConExplicativas$MAPEDiario <- abs(100*(realYPrediccionConExplicativas$DEMANDA_GAS-realYPrediccionConExplicativas$DEMANDA_GAS_PRED)/realYPrediccionConExplicativas$DEMANDA_GAS)

mean(realYPrediccionConExplicativas$MAPEDiario)

# 2.324161

# El error in-sampling global disminuye

# [10.2]. Error out-of-sampling

# Sin explicativas

predFutura <- as.data.frame(predict(ajusteAlt, n.ahead=horizontePrediccion))
names(predFutura) <- c("DEMANDA_GAS_PRED","SE")
realYPrediccionFut <- cbind(datos.validate[1:horizontePrediccion,],predFutura)
realYPrediccionFut$MAPEDiario <- abs(100*(realYPrediccionFut$DEMANDA_GAS-realYPrediccionFut$DEMANDA_GAS_PRED)/realYPrediccionFut$DEMANDA_GAS)

# MAPE global 

mean(realYPrediccionFut[,"MAPEDiario"])

# 4.943279

# MAPE a 1 día a futuro:

mean(realYPrediccionFut[1:1,"MAPEDiario"])

# 6.329467 (Anecdótico, poca muestra. Meter en bucle)

# Incluyendo festivos, temperaturas y outliers

names(prediccionConExplicativas) <- c("DEMANDA_GAS_PRED","SE")
realYPrediccionFutConExplicativas <- cbind(datos.validate[1:horizontePrediccion,],prediccionConExplicativas)
realYPrediccionFutConExplicativas$MAPEDiario <- abs(100*(realYPrediccionFutConExplicativas$DEMANDA_GAS-realYPrediccionFutConExplicativas$DEMANDA_GAS_PRED)/realYPrediccionFutConExplicativas$DEMANDA_GAS)

# MAPE global 

mean(realYPrediccionFutConExplicativas[,"MAPEDiario"])

# 6.244997>5.289403 (puede ser porque en la ventana de predicción no hay festivos)

# MAPE a 1 día a futuro:

mean(realYPrediccionFutConExplicativas[1:1,"MAPEDiario"])

# 6.893383 (Anecdótico, poca muestra. Meter en bucle)