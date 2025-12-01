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

# Se convierten los datos en un objeto de tipo time series
# para poder aplicar sobre ellos las funciones de R

datos.train.ts <- ts(datos.train$target, frequency = 12)
datos.test.ts <- ts(datos.test$target, frequency=12)

# Se representan gráficamente los datos

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

# Se estudia la estacionariedad en media y en varianza
# de la serie

# install.packages("tseries")
library(tseries)

###########################
###########################
###########################
# Metodología Box-Jenkins #
###########################
###########################
###########################

#################################
#################################
# ¿Es estacionaria en varianza? #
#################################
#################################

# Se evalúa la necesidad de transformar la serie para hacerla
# estacionaria en varianza

# install.packages("MASS")
library(MASS)

box_cox <- boxcox(target ~ fecha,
                  data = datos.train,
                  lambda = c(0, 0.5, 1))

lambda <- box_cox$x[which.max(box_cox$y)]
lambda

# 0.2828283 -> Se toma logaritmo por interpretabilidad (se podría tomar raíz cuadrada)

datos.train$log_target=log(datos.train$target)
datos.test$log_target=log(datos.test$target)
datos.train.ts <- ts(datos.train$log_target, frequency = 12)
datos.test.ts <- ts(datos.test$log_target, frequency=12)

########################################################
########################################################
# ¿Es estacionaria en media? (¿Hay que diferenciarla?) #
########################################################
########################################################

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

# Como hay p-valores bajos, podría no ser necesario hacer una diferencia regular

#############################################################################
#############################################################################
# ¿Es estacionaria en covarianza? (¿Hay que diferenciarla estacionalmente?) #
#############################################################################
#############################################################################

adf_seasonal <- ur.df(datos.train.ts, type = "drift", lags = 12)
summary(adf_seasonal)

# Como el p-valor es bajo, podría no ser necesario hacer una diferencia estacional

########################################
########################################
# Ajuste de un modelo ARIMA a la serie #
########################################
########################################

# install.packages("forecast")
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

# install.packages("caschrono")
library(caschrono)

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

##############
##############
# Predicción #
##############
##############

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

#########################
#########################
# Validación de errores #
#########################
#########################

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
datosLog.ts <- ts(datosLog$log_target, frequency = 12)

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
}

errores_a_horizonte1_a_futuro<-simulaPredicciones_Horizonte1(datosLog)

mean(errores_a_horizonte1_a_futuro$error_mape)

# MAPE out-of-sampling(h=1):5.173219 Este valor sí es comparable a 7.035763

##############################
##############################
# Análisis de intervenciones #
##############################
##############################

# Se carga y llama a la función que define los festivos

# Función para crear explicativas en series mensuales

# La variable de entrada contiene todas las fechas mensuales, asumiendo que el primer día
# de cada mes es el día 1.

# Función para crear explicativas en series mensuales

# Variables de entrada:
# --variableFecha: contiene todas las fechas mensuales, asumiendo que el primer día
# de cada mes es el día 1.
# --domingosYFestivosJuntos: booleano que vale 1 si se desea que el efecto de los festivos
# sea como el de un domingo, y que en la def. de laborables no se consideren festivos.

# install.packages("lubridate")
library(lubridate)

calculoExplicativasCalendario <- function(variableFecha, domingosYFestivosJuntos){
  
  #######################################
  #     Creación de todas las fechas    #
  #######################################
  
  # Se crean las fechas a nivel diario entre una primera y una última fecha dada.
  # Para llegar al último día del mes de la última fecha, se suman días para llegar
  # al 28/29, 30 o 31 según proceda.
  
  if (month(max(variableFecha)) %in% c(1,3,5,7,8,10,12)) {
    diasHastaFinMes <- 30
  } else if (month(max(variableFecha)) %in% c(4,6,9,11)) {
    diasHastaFinMes <- 29
  } else if (year(max(variableFecha))%%4==0) {
    diasHastaFinMes <- 28
  } else {diasHastaFinMes <- 27}
  
  todasLasFechas <- data.frame(fechas=seq(min(variableFecha),
                                          max(variableFecha)+diasHastaFinMes,
                                          by="days"))
  
  #######################################
  #     Cálculo de la Semana Santa      #
  #######################################
  
  # Función Easter de timeDate
  
  # # install.packages("timeDate")
  library(timeDate)
  
  domingoResurrecion <- as.Date(Easter(year(min(variableFecha)):year(max(variableFecha))))
  lunesPascua <- domingoResurrecion+1
  sabadoSanto <- domingoResurrecion-1
  viernesSanto <- domingoResurrecion-2
  juevesSanto <- domingoResurrecion-3
  
  # Se unen y ordenan todos los días que forman la Semana Santa
  semanaSanta <- sort(c(juevesSanto, viernesSanto, sabadoSanto, domingoResurrecion, lunesPascua))
  
  # Se pone en formato data.frame y se añade un indicador
  semanaSanta <- data.frame(fechas=semanaSanta, semanaSanta=rep(1,length(semanaSanta)))
  
  # Se añaden a la tabla maestra de fechas
  todasLasFechas_2 <- merge(x = todasLasFechas, y = semanaSanta, by = "fechas", all.x = TRUE)
  
  # Se reemplazan los NAs por 0, terminando de definir así el indicador de SemanaSanta
  todasLasFechas_2$semanaSanta[is.na(todasLasFechas_2$semanaSanta)] <- 0
  
  
  ######################################
  #     Cálculo de la variable dt      #
  ######################################
  
  # 1. Definición de festivos:
  ############################
  
  calendario <- todasLasFechas
  
  calendario$diaSemana <- as.factor(wday(calendario$fecha))
  calendario$diaMes <- as.factor(day(calendario$fecha))
  calendario$mes <- as.factor(month(calendario$fecha))
  calendario$anyo <- as.factor(year(calendario$fecha))

  # Quitar los sábados y domingos dado que si no, todos los meses
  # van a tener el mismo número de festivos y esto no valdría para nada
  # De hecho la variable festivo daría error al hacer festivo-lag12(festivo)
  # Además, los sábados y domingos ya están contabilizados en la varible dt
  
  calendario$p_01ene <- ifelse(calendario$diaMes==1 & calendario$mes==1 & !calendario$diaSemana %in% c(1, 7), 1, 0)
  calendario$p_06ene <- ifelse(calendario$diaMes==6 & calendario$mes==1 & !calendario$diaSemana %in% c(1, 7), 1, 0)
  calendario$p_19mar <- ifelse(calendario$diaMes==19 & calendario$mes==3 & !calendario$diaSemana %in% c(1, 7), 1, 0)
  calendario$p_01may <- ifelse(calendario$diaMes==1 & calendario$mes==5 & !calendario$diaSemana %in% c(1, 7), 1, 0)
  calendario$p_15ago <- ifelse(calendario$diaMes==15 & calendario$mes==8 & !calendario$diaSemana %in% c(1, 7), 1, 0)
  calendario$p_12oct <- ifelse(calendario$diaMes==12 & calendario$mes==10 & !calendario$diaSemana %in% c(1, 7),1, 0)
  calendario$p_01nov <- ifelse(calendario$diaMes==1 & calendario$mes==11 & !calendario$diaSemana %in% c(1, 7), 1 ,0)
  calendario$p_06dic <- ifelse(calendario$diaMes==6 & calendario$mes==12 & !calendario$diaSemana %in% c(1, 7), 1 ,0)
  calendario$p_08dic <- ifelse(calendario$diaMes==8 & calendario$mes==12 & !calendario$diaSemana %in% c(1, 7), 1 ,0)
  calendario$p_25dic <- ifelse(calendario$diaMes==25 & calendario$mes==12 & !calendario$diaSemana %in% c(1, 7), 1 ,0)
  
  calendario$festivo <- rowSums(subset(calendario, select=p_01ene:p_25dic))
  
  # La definición de la variable dt varía según la opción domingosYFestivosJuntos.
  
  if (domingosYFestivosJuntos==0){
    
    calendario$sabado <- ifelse(calendario$diaSemana==7, 1 ,0)
    calendario$domingo <- ifelse(calendario$diaSemana==1, 1 ,0)
    
    # Días laborables: todos menos sábados y domingos
    calendario$laborable <- 1-calendario$sabado-calendario$domingo
    
  } else {
    
    calendario$sabado <- ifelse(calendario$diaSemana==7, 1 ,0)
    calendario$domingo <- ifelse(calendario$diaSemana==1, 1 ,0)
    # Domingo=1 si domingo=1 o festivo=1
    calendario$domingo <- ifelse(calendario$domingo==1 | calendario$festivo==1, 1 ,0)
    
    # Días laborables: todos menos sábados y domingos/festivos
    calendario$laborable <- 1-calendario$sabado-calendario$domingo    
  }
  
  # 2. Definición de variable dt:
  ###############################
  
  # Se filtran las columnas de interés y se añade la Semana Santa
  
  calendario_2 <- calendario[, c("fechas", "mes", "anyo", "sabado", "domingo", "laborable", "festivo")]
  
  todasLasFechasFinal <- merge(x = todasLasFechas_2, y = calendario_2,
                               by = "fechas", all.x = TRUE)
  
  # Agregamos la serie a nivel año-mes
  
  calendarioAnyoMes <- aggregate(todasLasFechasFinal[,c("sabado","domingo",
                                                        "laborable", "semanaSanta", "festivo")],
                                 by=list(mes=todasLasFechasFinal$mes,
                                         anyo=todasLasFechasFinal$anyo),
                                 "sum")
  
  # Se calcula la variable dt:
  
  calendarioAnyoMes$dt <- calendarioAnyoMes$laborable-(5/2)*(calendarioAnyoMes$sabado+calendarioAnyoMes$domingo)

  ######################################
  #     Cálculo de años bisiestos      #
  ######################################
  
  calendarioAnyoMes$anyoNum <- as.numeric(levels(calendarioAnyoMes$anyo))[calendarioAnyoMes$anyo]
  
  calendarioAnyoMes$bisiesto <- ifelse(calendarioAnyoMes$mes==2 &(calendarioAnyoMes$anyoNum %% 4)==0, 1 ,0)
  
  #######################################################
  #     Tabla final con explicativas de calendario      #
  #######################################################
  
  if (domingosYFestivosJuntos==0){
    explicativasCalendario <- cbind(fecha=variableFecha, calendarioAnyoMes[, c("semanaSanta", "dt", "bisiesto", "festivo")])
  } else {
    explicativasCalendario <- cbind(fecha=variableFecha, calendarioAnyoMes[, c("semanaSanta", "dt", "bisiesto")])
  }
  
  return(explicativasCalendario)
  
}

explicativasCalendarioTrain <- calculoExplicativasCalendario(
  datos.train$fecha,
  domingosYFestivosJuntos=0)

calendarioTrain <- 
  as.matrix(
    explicativasCalendarioTrain[,c("semanaSanta", "dt", "bisiesto","festivo")]
  )

ajuste6conCalendario <- Arima(datos.train.ts,
                              order = c(0,1,1),
                              seasonal = list(order = c(0,1,1), 
                                              period = 12),
                              xreg = calendarioTrain,
                              method = "CSS")

coeftest(ajuste6conCalendario)

# Como el festivo no es significativo, lo incluimos en la definición de la variable dt
# Otra posibilidad sería definir dt como siempre y no incluir la variable festivo

explicativasCalendarioTrain <- calculoExplicativasCalendario(
  datos.train$fecha,
  domingosYFestivosJuntos=1)

calendarioTrain <- 
  as.matrix(
    explicativasCalendarioTrain[,c("semanaSanta", "dt", "bisiesto")]
  )

ajuste6conCalendario <- Arima(datos.train.ts,
                              order = c(0,1,1),
                              seasonal = list(order = c(0,1,1), 
                                              period = 12),
                              xreg = calendarioTrain,
                              method = "CSS")

coeftest(ajuste6conCalendario)

cor.arma(ajuste6conCalendario)

Box.test.2(residuals(ajuste6conCalendario),
           nlag = c(6,12,18,24,30,36,42,48),
           type="Ljung-Box")

# Ahora sí se supera el test de ruido blanco

#############################################################
#############################################################
# Validación de errores tras la inclusión de intervenciones #
#############################################################
#############################################################

# Sobre histórico

prediccionAPasado <- as.data.frame(ajuste6conCalendario$fitted)
# Sin corrección de Nelson
prediccionAPasado <- exp(prediccionAPasado)
names(prediccionAPasado) <- "pred_target"
realYPrediccion <- cbind(datos.train,prediccionAPasado)
realYPrediccion$MAPE <- 
  abs(100*(realYPrediccion$target-realYPrediccion$pred_target)/realYPrediccion$target)
mean(realYPrediccion$MAPE)

# Error in-sampling: 5.851334<7.035763
# Si no se incluyen los festivos de ninguna manera: 5.75187 

# A futuro

# Declaramos las variables de calendario a futuro

explicativasCalendarioTest <- calculoExplicativasCalendario(
  datos.test$fecha,
  domingosYFestivosJuntos=1)

calendarioTest <- as.matrix(explicativasCalendarioTest[,c("semanaSanta", "dt", "bisiesto")])

prediccionAFuturo <- as.data.frame(predict(ajuste6conCalendario, 
                                           newxreg=calendarioTest,
                                           n.ahead=12))

names(prediccionAFuturo) <- c("pred_target","SE")
realYPrediccionFut <- cbind(datos.test,prediccionAFuturo)
# Con corrección de Nelson
realYPrediccionFut$MAPE <- 
  abs(100*(realYPrediccionFut$target-exp(realYPrediccionFut$pred_target+0.5*realYPrediccionFut$SE^2))/realYPrediccionFut$target)
mean(realYPrediccionFut[,"MAPE"])

# Error out-of-sampling: 4.146255 > 3.829567 
# El error ha crecido, pero debe tenerse en cuenta que aquí hay errores hasta horizonte 12
# Si no se incluyen los festivos de ninguna manera: 3.488759 

# Validemos a 1 mes

# MAPE a 1 mes a futuro (error del mes de enero)

mean(realYPrediccionFut[1:1,"MAPE"])

# 1.999816 < 2.023595
# Si no se incluyen los festivos de ninguna manera: 0.3098083

simulaPrediccionesConExplicativas_Horizonte1 = function(data){
  
  #Extraemos los ordenes del modelo 
  
  # Inicializamos las variables necesarias
  
  vector_real = numeric()
  vector_prediccion = numeric()
  vector_mape = numeric()
  data_aux = data[1:144,]
  data_aux = ts(data_aux$log_target,frequency = 12)
  data_train_explicativas=as.matrix(calendarioTrain[1:144,])
  
  for(i in 1:12){

    data_test_explicativas=t(as.matrix(calendarioTest[i,]))
    
    modelo <- Arima(data_aux,
                    order = c(0,1,1),
                    seasonal = list(order = c(0,1,1), 
                    period = 12),
                    xreg = data_train_explicativas,
                    method = "CSS")
    
    prediccion <- as.data.frame(predict(modelo, 
                                        newxreg=data_test_explicativas,
                                        n.ahead=1))
    
    vector_real[i] = exp(data[dim(datosLog)[1]-12+i,c("log_target")])
    vector_prediccion[i] = exp(prediccion$pred+0.5*prediccion$se^2)
    # Calculamos mape
    vector_mape[i] = abs(vector_real[i] - vector_prediccion[i])*100/vector_real[i]
    # Recalculamos los datos_aux con un nuevo valor
    unaFilaMas=length(data_aux)+1
    data_aux = data[1:unaFilaMas,]
    data_aux = ts(data_aux$log_target, frequency = 12)

    data_train_explicativas=as.matrix(rbind(data_train_explicativas,data_test_explicativas))

  }
  
  return(list(reales= vector_real,
              predicciones = vector_prediccion,
              error_mape = vector_mape))  
}

errores_a_horizonte1_a_futuro<-simulaPrediccionesConExplicativas_Horizonte1(datosLog)

mean(errores_a_horizonte1_a_futuro$error_mape)

# MAPE out-of-sampling(h=1):4.87344 < 5.173219
# Como sucedía en TRAIN, a horizonte 1 el error disminuye en TEST

# Si no se incluyen los festivos de ninguna manera: 4.18
# (sería mejor seguir con este modelo)

##############################
##############################
# Identificación de outliers #
##############################
##############################

# # install.packages("tsoutliers")
library(tsoutliers)

# Esto identifica outliers

listaOutliersTrain <- locate.outliers(ajuste6conCalendario$residuals,
                                      pars = coefs2poly(ajuste6conCalendario),
                                      types = c("AO", "LS", "TC"),cval=3)

listaOutliersTrain$abststat=abs(listaOutliersTrain$tstat)

# Cruzamos con la tabla original para obtener la fecha

datos.train$ind <- as.numeric(rownames(datos.train))
listaOutliersTrainFecha <- merge(listaOutliersTrain, datos.train[,c("ind", "fecha")], by = "ind")

listaOutliersTrainFecha

# # install.packages("dplyr")
library(dplyr)

# Se ordena del más severo (mayor abststat) el menos severo

arrange(listaOutliersTrainFecha,desc(listaOutliersTrainFecha$abststat))

# Por tener uno de cada tipología, vamos a incluir el 67, el 93 y el 58
# (nos saltamos algunos porque no interesa que estén próximos)

outliersTrain <- outliers(c("LS","TC","AO"), c(67,93,58))
outliersVariablesTrain <- outliers.effects(outliersTrain, length(ajuste6conCalendario$residuals))
calendarioMasOutliersTrain <- as.matrix(cbind(calendarioTrain,outliersVariablesTrain))

ajuste6conCalendarioYOutliers <- Arima(datos.train.ts,
                                       order = c(0,1,1),
                                       seasonal = list(order = c(0,1,1), period = 12),
                                       xreg = calendarioMasOutliersTrain,
                                       method = "CSS")

coeftest(ajuste6conCalendarioYOutliers)

cor.arma(ajuste6conCalendarioYOutliers)

Box.test.2(residuals(ajuste6conCalendarioYOutliers),
           nlag = c(6,12,18,24,30,36,42,48),
           type="Ljung-Box")

########################################################################
########################################################################
# Validación de errores tras la inclusión de intervenciones y outliers #
########################################################################
########################################################################

# Sobre histórico

prediccionAPasado <- as.data.frame(ajuste6conCalendarioYOutliers$fitted)
# Sin corrección de Nelson
prediccionAPasado <- exp(prediccionAPasado)
names(prediccionAPasado) <- "pred_target"
realYPrediccion <- cbind(datos.train,prediccionAPasado)
realYPrediccion$MAPE <- 
  abs(100*(realYPrediccion$target-realYPrediccion$pred_target)/realYPrediccion$target)
mean(realYPrediccion$MAPE)

# Error in-sampling: 4.917388<5.851334

# Si no se incluyen los festivos de ninguna manera, saldría el AO112 en lugar del AO58
# Los otros outliers, se conservarían
# En dicho caso, el error sería: 4.87

# A futuro

# Declaramos los outliers a futuro
# El LS valdrá 1 porque vale 1 a partir de la fecha que se produce
# El TC, si no está muy cercano al final de la serie valdrá 0 porque su efecto se diluye
# El AO valdrá 0 porque solo vale 1 el mes que se produce
# Lo comprobamos observando los 12 últimos valores de estas variables en TRAIN

calendarioMasOutliersTrain

# Por tanto, podemos coger los 12 últimos valores como newxreg para test
# Si acaso con el TC habría que ser algo más preciso

outliersVariablesTest <- outliersVariablesTrain[(nrow(outliersVariablesTrain)-11):
                                                  nrow(outliersVariablesTrain),]

calendarioMasOutliersTest <- as.matrix(cbind(calendarioTest,outliersVariablesTest))

prediccionAFuturo <- as.data.frame(predict(ajuste6conCalendarioYOutliers, 
                                           newxreg=calendarioMasOutliersTest,
                                           n.ahead=12))

names(prediccionAFuturo) <- c("pred_target","SE")
realYPrediccionFut <- cbind(datos.test,prediccionAFuturo)
# Con corrección de Nelson
realYPrediccionFut$MAPE <- 
  abs(100*(realYPrediccionFut$target-exp(realYPrediccionFut$pred_target+0.5*realYPrediccionFut$SE^2))/realYPrediccionFut$target)
mean(realYPrediccionFut[,"MAPE"])

# Error out-of-sampling: 3.908169 < 4.146255 

# Si no se incluyen los festivos de ninguna manera, saldría el AO112 en lugar del AO58
# Los otros outliers, se conservarían
# En dicho caso, el error sería: 3.84

# Validemos a 1 mes

# MAPE a 1 mes a futuro (error del mes de enero)

mean(realYPrediccionFut[1:1,"MAPE"])

# 2.472715 > 1.999816
# La inclusión de outliers ha empeorado la predicción de enero sobre test

# Si no se incluyen los festivos de ninguna manera, saldría el AO112 en lugar del AO58
# Los otros outliers, se conservarían
# En dicho caso, el error sería: 0.64

simulaPrediccionesConExplicativas_Horizonte1 = function(data){
  
  #Extraemos los ordenes del modelo 
  
  # Inicializamos las variables necesarias
  
  vector_real = numeric()
  vector_prediccion = numeric()
  vector_mape = numeric()
  data_aux = data[1:144,]
  data_aux = ts(data_aux$log_target,frequency = 12)
  data_train_explicativas=as.matrix(calendarioMasOutliersTrain[1:144,])
  
  for(i in 1:12){
    
    data_test_explicativas=t(as.matrix(calendarioMasOutliersTest[i,]))
    
    modelo <- Arima(data_aux,
                    order = c(0,1,1),
                    seasonal = list(order = c(0,1,1), 
                                    period = 12),
                    xreg = data_train_explicativas,
                    method = "CSS")
    
    prediccion <- as.data.frame(predict(modelo, 
                                        newxreg=data_test_explicativas,
                                        n.ahead=1))
    
    vector_real[i] = exp(data[dim(datosLog)[1]-12+i,c("log_target")])
    vector_prediccion[i] = exp(prediccion$pred+0.5*prediccion$se^2)
    # Calculamos mape
    vector_mape[i] = abs(vector_real[i] - vector_prediccion[i])*100/vector_real[i]
    # Recalculamos los datos_aux con un nuevo valor
    unaFilaMas=length(data_aux)+1
    data_aux = data[1:unaFilaMas,]
    data_aux = ts(data_aux$log_target, frequency = 12)
    
    data_train_explicativas=as.matrix(rbind(data_train_explicativas,data_test_explicativas))
    
  }
  
  return(list(reales= vector_real,
              predicciones = vector_prediccion,
              error_mape = vector_mape))  
}

errores_a_horizonte1_a_futuro<-simulaPrediccionesConExplicativas_Horizonte1(datosLog)

mean(errores_a_horizonte1_a_futuro$error_mape)

# MAPE out-of-sampling(h=1):4.603174 < 4.87344
# A pesar de que en enero el error fue peor, si se lanza los 12 meses a horizonte 1, mejora

# Si no se incluyen los festivos de ninguna manera, saldría el AO112 en lugar del AO58
# Los otros outliers, se conservarían
# En dich
# error sería: 4.39