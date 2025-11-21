###########################################################################
# Empleo de la metodología Box-Jenkins para el ajuste de un modelo SARIMA #
# a la serie que mide las matriculaciones mensuales de turismo en España  #
###########################################################################




# Se limpia el espacio de trabajo:

rm(list = ls())

# Se asigna el directorio donde están los datos

setwd("/Users/miguelfernandez/GitHub/ucm/Series_Temporales/")

# Carga de datos

datos <- read.csv("series_matriculaciones_tur_paro.csv",sep=",")

datos$fecha=as.Date(datos$fecha,format='%m/%d/%Y')
datos.train <- subset(datos, fecha<=as.Date('12/01/2016',format='%m/%d/%Y'))
datos.test <- subset(datos, fecha>as.Date('12/01/2016',format='%m/%d/%Y'))

# [1]. Se convierten los datos en un objeto de tipo time series
#      para poder aplicar sobre ellos las funciones de R
################################################################

datos.train.ts <- as.ts(datos.train$target, frequency = 12)
datos.test.ts <- as.ts(datos.test$target, frequency=12)

# [2]. Se representan gráficamente los datos
################################################################

# install.packages("ggplot2")
library(ggplot2)

names(datos.train)

graficoInicial <- ggplot(aes(x= fecha, y = target), data = datos.train) +
  geom_line(color = '#d84519',
            size = 1) + 
  xlab('FECHA') + ylab('Matriculaciones')

names(datos.train)
ggplot(aes(x= fecha, y = target), data = datos.train) + geom_line(color = '#d84519') + 
  xlab('FECHA') + ylab('IPI')



# [3]. Se estudia la estacionariedad en media y en varianza
#      de la serie
################################################################

# install.packages("tseries")
library(tseries)

# [3.1] ¿Es estacionaria en varianza?

# Se evalúa la necesidad de transformar la serie para hacerla
# estacionaria en varianza

# install.packages("MASS")
library(MASS)

box_cox <- boxcox(target ~ fecha,
                  data = datos.train,
                  lambda = c(0, 0.5, 1))

lambda <- box_cox$x[which.max(box_cox$y)]
lambda

# -> Se toma logaritmo

datos.train$log_target=log(datos.train$target)
datos.test$log_target=log(datos.test$target)
datos.train.ts <- as.ts(datos.train$log_target, frequency = 12)
datos.test.ts <- as.ts(datos.test$log_target, frequency=12)

# [3.2] ¿Es estacionaria en media? (¿Hay que diferenciarla?)
# El profe prefiere ajustar y si el parámetro se le v a uno luego diferencia.

# Test de Dickey-Fuller

#install.packages("urca")
library(urca)

# Función para obtener p-valores de ADF

adf_all_tests <- function(y, max_lags = "AIC") {
  types <- c("none", "drift", "trend")  # Tipos de prueba
  names_types <- c("Sin Intercepto", "Con Intercepto", "Con Intercepto y Tendencia")
  
  # Lista para guardar los p-valores
  p_values <- numeric(3)
  # TEST DE RAÍCES UNITARIAS ES POCO POTENTE
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

# Como hay p-valores bajos, podría no ser necesario hacer una diferencia regular

# [3.3] ¿Es estacionaria en covarianza? (¿Hay que diferenciarla estacionalmente?)

adf_seasonal <- ur.df(datos.train.ts, type = "drift", lags = 12)
summary(adf_seasonal)

# Como el p-valor es bajo, podría no ser necesario hacer una diferencia estacional

# [4]. Ajuste de un modelo ARIMA a la serie

#install.packages("forecast")
library(forecast)

graphics.off()

Acf(datos.train.ts, lag.max = 48, xlab = "Retardo",
    main= "Función de autocorrelación simple")

Pacf(datos.train.ts, lag.max = 48, xlab = "Retardo",
     main = "Función de autocorrelación parcial")

# Se propone un SARIMA(1,0,0)x(0,0,0)12 + MU

ajuste1 <- Arima(datos.train.ts,
                 order = c(1,0,0),
                 seasonal = list(order = c(0,0,0), 
                 period = 12))
ajuste1

# install.packages("lmtest")
library(lmtest)

coeftest(ajuste1)

# Instrucciones si no se instala caschrono en tu versión de R
# Este paquete es el que permite instalar una versión antigua del repositorio de R

#install.packages("remotes")
# library(remotes)

# Instalar estos paquetes si dan problemas
#install.packages("zoo")
#install.packages("Hmisc")

# Indicar el paquete y la versión que se quiere instalar.
# Esta es la última disponible. Para saber cuál es se consulta aquí:
# https://cran.r-project.org/src/contrib/Archive/caschrono/

#install_version("caschrono", version = "2.4")

# En general puedes consultar todos los paquetes y versiones que han 
# existido en R aquí:
# https://cran.r-project.org/src/contrib/Archive/



cor.arma(ajuste1)

graphics.off()

Acf(ajuste1$residuals, lag.max = 48, xlab = "Retardo",
    main= "Función de autocorrelación simple")

Pacf(ajuste1$residuals, lag.max = 48, xlab = "Retardo",
     main = "Función de autocorrelación parcial")

Box.test.2(residuals(ajuste1),
           nlag = c(6,12,18,24,30,36,42,48),
           type="Ljung-Box")

# Se propone un SARIMA(1,0,0)x(1,0,0)12 + MU

ajuste2 <- Arima(datos.train.ts,
                 order = c(1,0,0),
                 seasonal = list(order = c(1,0,0), period = 12))

coeftest(ajuste2)

cor.arma(ajuste2)

graphics.off()

Acf(ajuste2$residuals, lag.max = 48, xlab = "Retardo",
    main= "Función de autocorrelación simple")

Pacf(ajuste2$residuals, lag.max = 48, xlab = "Retardo",
     main = "Función de autocorrelación parcial")

Box.test.2(residuals(ajuste2),
           nlag = c(6,12,18,24,30,36,42,48),
           type="Ljung-Box")

# Se propone un SARIMA(1,0,1)x(1,0,0)12 + MU

ajuste3 <- Arima(datos.train.ts,
                 order = c(1,0,1),
                 seasonal = list(order = c(1,0,0), period = 12))

coeftest(ajuste3)

cor.arma(ajuste3)

graphics.off()

Acf(ajuste3$residuals, lag.max = 48, xlab = "Retardo",
    main= "Función de autocorrelación simple")

Pacf(ajuste3$residuals, lag.max = 48, xlab = "Retardo",
     main = "Función de autocorrelación parcial")

Box.test.2(residuals(ajuste3),
           nlag = c(6,12,18,24,30,36,42,48),
           type="Ljung-Box")

# Se propone un SARIMA(1,0,1)x(1,0,1)12 + MU

ajuste4 <- Arima(datos.train.ts,
                 order = c(1,0,1),
                 seasonal = list(order = c(1,0,1), period = 12))

# (el método de estimación de parámetros  por defecto es CSS-ML:
# Primero ajusta vía Condicional Least Square para obtener 
# buenas estimaciones iniciales.
# Luego refina los parámetros usando ML)
# Como no funciona, probamos ML

ajuste4 <- Arima(datos.train.ts,
                 order = c(1,0,1),
                 seasonal = list(order = c(1,0,1), period = 12),method="ML")

coeftest(ajuste4)

cor.arma(ajuste4)

graphics.off()

Acf(ajuste4$residuals, lag.max = 48, xlab = "Retardo",
    main= "Función de autocorrelación simple")

Pacf(ajuste4$residuals, lag.max = 48, xlab = "Retardo",
     main = "Función de autocorrelación parcial")

Box.test.2(residuals(ajuste4),
           nlag = c(6,12,18,24,30,36,42,48),
           type="Ljung-Box")

# Se propone un SARIMA(1,0,1)x(0,1,1)12 + MU
# Esto es porque he visto que el sar se hacerca mucho al 1

ajuste5 <- Arima(datos.train.ts,
                 order = c(1,0,1),
                 seasonal = list(order = c(0,1,1), period = 12))

# Aquí el método de estimación por defecto vuelve a funcionar

coeftest(ajuste5)

cor.arma(ajuste5)

# Automáticamente ha quitado la constante

ajuste5 <- Arima(datos.train.ts,
                 include.constant = TRUE,
                 order = c(1,0,1),
                 seasonal = list(order = c(0,1,1), period = 12))

coeftest(ajuste5)

# Efectivamente, no era significativa. Está bien quitada

# Se propone un SARIMA(0,1,1)x(0,1,1)12 + MU

ajuste6 <- Arima(datos.train.ts,
                 order = c(0,1,1),
                 seasonal = list(order = c(0,1,1), period = 12))

coeftest(ajuste6)

ajuste_def <- auto.arima(datos.train.ts,
                     seasonal = TRUE)  # detecta solo si hay estacionalidad
ajuste


# Peligro de no invertibilidad 
# Cambiemos el método de estimación 

jarque.bera.test(residuals(ajuste6))

# Obsérvese que los residuos no son normales, con lo cual, no
# está justificado el empleo de ML

ajuste6 <- Arima(datos.train.ts,
                 order = c(0,1,1),
                 seasonal = list(order = c(0,1,1), period = 12),
                 method = "CSS")

coeftest(ajuste6)

cor.arma(ajuste6)

graphics.off()

Acf(ajuste6$residuals, lag.max = 48, xlab = "Retardo",
    main= "Función de autocorrelación simple")

Pacf(ajuste6$residuals, lag.max = 48, xlab = "Retardo",
     main = "Función de autocorrelación parcial")

Box.test.2(residuals(ajuste6),
           nlag = c(6,12,18,24,30,36,42,48),
           type="Ljung-Box")

# [5]. Predicción

prediccionAFuturo <- as.data.frame(predict(ajuste6, n.ahead=12))

# Límites de confianza al 95%

U <- exp(prediccionAFuturo$pred + 1.96*prediccionAFuturo$se)
L <- exp(prediccionAFuturo$pred - 1.96*prediccionAFuturo$se)

# Se aplica la corrección de Nelson para que quede centrada la predicción

datos.pred <- data.frame(fecha = datos.test$fecha, Prediccion = exp(prediccionAFuturo$pred+0.5*prediccionAFuturo$se^2),
                         LimSup = U, LimInf =L)

datos.real.pred <- merge(datos[,c("fecha","target")], datos.pred, by = "fecha", all.x = T)

# Gráfico 1: Real vs Predicción

library(ggplot2)

ggplot(datos.real.pred, aes(x = fecha)) +
  geom_line(aes(y = target),
            color = "steelblue", size = 1, alpha = 0.8,
            na.rm = TRUE) +
  geom_line(aes(y = Prediccion),
            color = "darkred", size = 1, linetype = 2, alpha = 0.9,
            na.rm = TRUE) +
  labs(x = "FECHA", y = "Matriculaciones") +
  theme_minimal(base_size = 14)

# Gráfico 2: Real + Predicción + límites

datos.real.pred$target[datos.real.pred$fecha>as.Date('12/01/2016',format='%m/%d/%Y')] <- NA

grafico2 <- ggplot(datos.real.pred, aes(x = fecha)) +
  
  # Banda de predicción
  geom_ribbon(aes(ymin = LimInf, ymax = LimSup),
              fill = "orange", alpha = 0.2, na.rm = TRUE) +
  
  # Predicción
  geom_line(aes(y = Prediccion),
            color = "darkred", size = 1, na.rm = TRUE) +
  
  # Serie real
  geom_line(aes(y = target),
            color = "steelblue", size = 0.8, alpha = 0.8,
            na.rm = TRUE) +
  
  labs(x = "FECHA", y = "Matriculaciones") +
  theme_minimal(base_size = 14)

grafico2

# [6]. Fase de validación

# Sobre histórico

prediccionAPasado <- as.data.frame(ajuste6$fitted)
# Sin corrección de Nelson
prediccionAPasado <- exp(prediccionAPasado)
names(prediccionAPasado) <- "pred_target"
realYPrediccion <- cbind(datos.train,prediccionAPasado)
realYPrediccion$MAPE <- 
abs(100*(realYPrediccion$target-realYPrediccion$pred_target)/realYPrediccion$target)
mean(realYPrediccion$MAPE)

# Error in-sampling: 7.035763

# A futuro

names(prediccionAFuturo) <- c("pred_target","SE")
realYPrediccionFut <- cbind(datos.test,prediccionAFuturo)
# Con corrección de Nelson
realYPrediccionFut$MAPE <- 
abs(100*(realYPrediccionFut$target-exp(realYPrediccionFut$pred_target+0.5*realYPrediccionFut$SE^2))/realYPrediccionFut$target)
mean(realYPrediccionFut[,"MAPE"])

# Error out-of-sampling: 3.829567 
# Al haber errores hasta a 12 meses, no es comparable con 7.035763

# MAPE a 1 mes a futuro (error del mes de enero)

mean(realYPrediccionFut[1:1,"MAPE"])

# 2.023595 (Al ser un solo dato, no es comparable con 7.035763)
# La solución pasaría por hacer 12 predicciones a futuro a 1 mes. Meter en un bucle

datosLog<-datos[,c("fecha","target")]
datosLog$log_target=log(datosLog$target)
datosLog.ts <- as.ts(datosLog$log_target, frequency = 12)

simulaPredicciones_Horizonte1 = function(data){
  #Extraemos los ordenes del modelo 

  # Inicializamos las variables necesarias
  vector_real = numeric()
  vector_prediccion = numeric()
  vector_mape = numeric()
  data_aux = data[1:144,]
  data_aux = ts(data_aux$log_target,frequency = 12)

  for(i in 1:12){

    modelo <- Arima(data_aux,
                     order = c(0,1,1),
                     seasonal = list(order = c(0,1,1), period = 12),
                     method = "CSS")
    
    prediccion <- as.data.frame(predict(modelo, 
                                        n.ahead=1))
    
    vector_real[i] = exp(data[dim(datosLog)[1]-12+i,c("log_target")])
    vector_prediccion[i] = exp(prediccion$pred+0.5*prediccion$se^2)
    # Calculamos mape
    vector_mape[i] = abs(vector_real[i] - vector_prediccion[i])*100/vector_real[i]
    # Recalculamos los datos_aux con un nuevo valor
    unaFilaMas=length(data_aux)+1
    data_aux = data[1:unaFilaMas,]
    data_aux = ts(data_aux$log_target, frequency = 12)

  }
  
  return(list(reales= vector_real,
              predicciones = vector_prediccion,
              error_mape = vector_mape))  


errores_a_horizonte1_a_futuro<-simulaPredicciones_Horizonte1(datosLog)
}
mean(errores_a_horizonte1_a_futuro$error_mape)

# MAPE out-of-sampling(h=1):5.173219 Este valor sí es comparable a 7.035763

