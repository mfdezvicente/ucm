##############################################################
############      EXAMEN SERIES TEMPORALES   #################
############      MIGUEL FERNÁNDEZ VICENTE   #################
############      03203763R --- 12/12/2025   #################
##############################################################

## EN PRIMER LUGAR DECIR QUE AL SER DE GUADALAJARA, ESTOY AGRADECIDO DE QUE ME HAYA TOCADO LA SERIE DE GUADA. 


##############################################################
###############      PREPROCESAMIENTO    #####################
##############################################################


##### LIBRERIAS
## CARGAMOS LA LIBRERÍAS QUE SON PROBABLES DE SER USADAS PARA EL ANÁLISIS
library(ggplot2)
library(tseries)
library(MASS)
library(urca)
library(forecast)
library(lmtest)
library(caschrono)
library(lubridate)
library(timeDate)
library(tsoutliers)


##### CARGA DE DATOS 

# Borra la historia pasada que tengamos cargada
rm(list = ls())

# Adjudico el directorio en concreto 
setwd("/Users/miguelfernandez/GitHub/ucm/Series_Temporales/")

# Cargo los datos que provienen del csv del examen
datos <- read.csv("_21_guadalajara.csv",sep=",")
head(datos)


# Convertimos los nombres de los datos de la serie para fecha y target
names(datos)[1] <- "fecha"
names(datos)[2] <- "target"

head(datos)
datos ## Debe de tener cambiado el nombre de las variables

# Se convierta la fehca a tipo fecha
datos$fecha=as.Date(datos$fecha,format='%m/%d/%Y')
datos.train <- subset(datos, fecha<=as.Date("12/01/2018",format='%m/%d/%Y'))
datos.test <- subset(datos, fecha>as.Date("12/01/2018",format='%m/%d/%Y'))

# Se convierten los datos en un objeto de tipo time series
# para poder aplicar sobre ellos las funciones de R

# VIGILAMOS LA FRECUENCIA PORQUE ESTA INDICA LOS PERRIODOS: SEMANAL, MENSUAL
datos.train.ts <- ts(datos.train$target, frequency = 12)
datos.test.ts <- ts(datos.test$target, frequency=12)

# Se representan gráficamente los datos
graficoInicial <- ggplot(aes(x= fecha, y = target), data = datos.train) +
  geom_line(color = '#d84519',
            size = 1) + 
  xlab('FECHA') + ylab('hipotecas')

graficoInicial 


##############################################################
##   EJERCICIO 1. ESTUDIO DE ESTACIONATIEDAD/ESTACIONALIDAD ##
##############################################################


#### PRIMERO: SE ESTUDIA LA ESTACIONARIDAD EN MEDIA Y EN VARIANZA
# Metodología Box-Jenkins 

# ¿Es estacionaria en varianza? #
box_cox <- boxcox(target ~ fecha,
                  data = datos.train,
                  lambda = c(0, 0.5, 1))

lambda <- box_cox$x[which.max(box_cox$y)]
lambda

# SABEMOS QUE VALORES CERCANOS A 0 GENERARÍAN UN CAMBIO A LOG 
# LAMBDA = 0.11111 -----> LAMBDA = 0
# ES POR ELLO QUE USAMOS LOG

## SÍ HAY LOG
datos.train$log_target=log(datos.train$target) # ESTO ES PARA CREAR LAS VARIABLE Y LUEGO LAS COMPLETAMOS
datos.test$log_target=log(datos.test$target)
datos.train.ts <- ts(datos.train$log_target, frequency = 12)
datos.test.ts <- ts(datos.test$log_target, frequency=12)


# ¿Es estacionaria en media? (¿Hay que diferenciarla?) #
# Test de Dickey-Fuller

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

# LOS P-VALORES SON ALTOS SALVO EN UNO DE ELLOS. DE PRIMERAS NO VOY A DIFERENCIAR PERO LO VOY A TENER MUY PRESENTE. POSTERIORMENTE CONFIRMARÉ QUE SERÁ NECESARIA UNA DIFERENCIA Y ASÍ LO HARÉ


# ¿Es estacionaria en covarianza? (¿Hay que diferenciarla estacionalmente?) #

adf_seasonal <- ur.df(datos.train.ts, type = "drift", lags = 12)
summary(adf_seasonal)

# Como el p-valor es bajo, podría no ser necesario hacer una diferencia estacional

##############################################################
#########   AJUSTE DE MODELO ARMA/ARIMA/SARIMA   #############
##############################################################

# EN PRIMER VISUALIZAMOS CUÁLES SON LAS FUNCIONES DE AUTOCORRELACIÓN SIMPLE Y PARCIAL DE LA SERIE
Acf(datos.train.ts, lag.max = 48, xlab = "Retardo",
    main= "Función de autocorrelación simple")
Pacf(datos.train.ts, lag.max = 48, xlab = "Retardo",
     main = "Función de autocorrelación parcial")

# OBSERVAMOS QUE EL GRÁFICO DE AUTOCORRELACIÓN SIMPLE TIENE INFINITOS VALORES SUPERIORES A LAS BANDAS. 
# ADEMÁS EL GRÁFICO DE AUTOCORRELACIÓN PARCIAL SOLO TIENE CUATRO VALORES, SIN EMBARGO EL PRIMERO ES MUY GRANDE. 
# ES POR ESO QUE ELIJO INCLUIR EL PRIMERO DE TODO HACIENDO UN AR(1)

# EN PRIMER LUGAR AJUSTAMOS A UN MODELO AR(1)
ajuste1 <- Arima(datos.train.ts,
                 order = c(1,0,0),
                 seasonal = list(order = c(0,0,0), 
                 period = 12))
ajuste1
coeftest(ajuste1) # CON ESTO VEMOS LOS COEFICIENTES DEL MODELO 
cor.arma(ajuste1) # CON ESTO VEMOS LAS CORRELACIONES ENTRE LOS FACTORES DEL MODELO. CORR > 0.85 DEBERÁN DE TOMARSE CON PRECAUCIÓN. PUDIENDO GENERAR UNA DIFERENCIACIÓN

# PUEDO COMPROBAR QUE EL VALOR DE AR NO SE ACERCA A 1 Y QUE ADEMÁS NO ES MUY SIGNIFICATIVO. LAS CORRELACIONES CORRECTAS, NO SON ALTAS. 
# PROCEDO A MIRAR LOS GRA´FICOS DE LAS AUTOCORRELACIONES

Acf(ajuste1$residuals, lag.max = 48, xlab = "Retardo",
    main= "Función de autocorrelación simple")

#FUNCIÓN DE AUTOCORRELACIÓN PARCIAL. PERMITE DARNOS EL NÚMERO DE AR()
Pacf(ajuste1$residuals, lag.max = 48, xlab = "Retardo",
     main = "Función de autocorrelación parcial")

# VEO CLARAMENTE QUE EN EL ACF EL PRIMER VALOR SALE EXCESIVAMENTE, LOS DEMÁS MÁS O MENOS SE MANTIENEN EN LAS BANDAS
# ES POR ESO QUE PROPONGO UN ARMA(1,1)
# CON ESTO MIRAMOS SI ES RUIDO BLANCO O NO. PARA ELLO DEBEMOS DE TENER VALROES SUPERIORES A 0.05
Box.test.2(residuals(ajuste1),
           nlag = c(6,12,18,24,30,36,42,48),
           type="Ljung-Box")

# NO TENEMOS RUIDO BLANCO, EVIDENTEMENTE

ajuste2 <- Arima(datos.train.ts,
                 order = c(1,0,1),
                 seasonal = list(order = c(0,0,0), 
                 period = 12))
ajuste2
coeftest(ajuste2) # CON ESTO VEMOS LOS COEFICIENTES DEL MODELO 
cor.arma(ajuste2) 

# VEO QUE TODOS LOS VALORES SON SIGNIFICATIVOS. 
# SIN EMBARGO EL AR(1) ES MUY CERCANO A 1, GENERANDO PROBLEMAS DE NO ESTACIONALIDAD. DEBEMOS DE REALIZAR UNA DIFERENCIA (¡YA LO INTUIMOS POR EL TEST DE Dickey-Fuller!) 


ajuste3 <- Arima(datos.train.ts,
                 order = c(0,1,1),
                 seasonal = list(order = c(0,0,0), 
                 period = 12))
ajuste3
coeftest(ajuste3) # CON ESTO VEMOS LOS COEFICIENTES DEL MODELO 
cor.arma(ajuste3) 

Box.test.2(residuals(ajuste3),
           nlag = c(6,12,18,24,30,36,42,48),
           type="Ljung-Box")

Acf(ajuste3$residuals, lag.max = 48, xlab = "Retardo",
    main= "Función de autocorrelación simple")

#FUNCIÓN DE AUTOCORRELACIÓN PARCIAL. PERMITE DARNOS EL NÚMERO DE AR()
Pacf(ajuste3$residuals, lag.max = 48, xlab = "Retardo",
     main = "Función de autocorrelación parcial")


# MIRO LOS COEFICIENTES DEL AJUSTE, Y ME SALE MA(1) = -0.68648. ES POR ELLO QUE ESTÁ BIEN
# NO HAY COEFICIENTES DE CORRELACIÓN. 

# DA RUIDO BLANCO MEDIANTE EL MÉTODO DE LJUNG-BOX A MÁS DEL RETARDO 50. 

# EN EL ACP SOLO SE SALEN DE LA BANDA 3 VALORES Y SUPERIORES AL RETARDO 15. 
# EN LA PACF SOLO SE SALE UN VALOR Y ES EN EL 35. 

# NO HE OBSERVADO EN NINGUNO DE LOS DOS GRÁFICOS UN COMPORTAMIENTO DE SEASONAL
# SÉ QUE EN EL ENUNCIADO PONE QUE CALCULEMOS EL SARIMA, PERO EN ESTE CASO CON UN ARMA(0,1) ES SUFICIENTE
# ES POR ELLO QUE DEJARÉ UN MODELO DEL TIPO MA(1). 


# COMO EL EJERCICIO PEDÍA SARIMA, TRATO DE INTENTAR VER QUE ALGO QUE CONTEMPLE LA PARTE ESTACIONAL. 
ajuste4 <- Arima(datos.train.ts,
                 order = c(0,0,1),
                 seasonal = list(order = c(1,0,0), 
                 period = 12))
ajuste4
coeftest(ajuste4) # CON ESTO VEMOS LOS COEFICIENTES DEL MODELO 
cor.arma(ajuste4) 

Box.test.2(residuals(ajuste3),
           nlag = c(6,12,18,24,30,36,42,48),
           type="Ljung-Box")

Acf(ajuste4$residuals, lag.max = 48, xlab = "Retardo",
    main= "Función de autocorrelación simple")

#FUNCIÓN DE AUTOCORRELACIÓN PARCIAL. PERMITE DARNOS EL NÚMERO DE AR()
Pacf(ajuste4$residuals, lag.max = 48, xlab = "Retardo",
     main = "Función de autocorrelación parcial")


# HE PROBADO EL MODELO CON SARIMA PERO SE ME DISPARAN TODOS LOS VALORES DE LA ACF
# HA SIDO MÁS PROBARLO POR VER SI MEJORABA ALGO PERO NO TENÍA SENTIDO DESDE EL PRINCIPIO.



#################################################################################
#####################  MODELO FINAL: MA(1) CON UNA DIFERENCIA   #################
#################################################################################

ajuste3


##############################################################
#####################   PRED/ERROR   #########################
##############################################################

##############################################################
##################   1. PREDICCIÓN   #########################
##############################################################

# PONEMOS LA PREDICCIÓN A FUTURO DEL TIEMPO QUE NOS HEMOS DEJADO SIN MIRAR
prediccionAFuturo <- as.data.frame(predict(ajuste3, n.ahead=12))

# LÍMITES DE CONFIANZA AL 95%
U <- exp(prediccionAFuturo$pred + 1.96*prediccionAFuturo$se)
L <- exp(prediccionAFuturo$pred - 1.96*prediccionAFuturo$se)


# CORRECCIÓN DE NELSON - EXPONENCIAL
datos.pred <- data.frame(fecha = datos.test$fecha, Prediccion = exp(prediccionAFuturo$pred+0.5*prediccionAFuturo$se^2),
                         LimSup = U, LimInf =L)
datos.real.pred <- merge(datos[,c("fecha","target")], datos.pred, by = "fecha", all.x = T)


# GRÁFICO 1: REAL VS PREDICCIÓN
ggplot(datos.real.pred, aes(x = fecha)) +
  geom_line(aes(y = target),
            color = "steelblue", size = 1, alpha = 0.8,
            na.rm = TRUE) +
  geom_line(aes(y = Prediccion),
            color = "darkred", size = 1, linetype = 2, alpha = 0.9,
            na.rm = TRUE) +
  labs(x = "FECHA", y = "Matriculaciones") +
  theme_minimal(base_size = 14)


# GRÁFICO 2: REAL, PREDICCIÓN Y LÍMITES

datos.real.pred$target[datos.real.pred$fecha>as.Date("12/01/2018",format='%m/%d/%Y')] <- NA
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
  
  labs(x = "FECHA", y = "Hipotecas") +
  theme_minimal(base_size = 14)

grafico2


##############################################################
##################   2. ERRORES      #########################
##############################################################

##################   2.1 HISTÓRICO   #########################

prediccionAPasado <- as.data.frame(ajuste3$fitted)
prediccionAFuturo <- as.data.frame(predict(ajuste3, n.ahead=12))


# EN EL CASO DE LA LOGARIMICA
prediccionAPasado <- exp(prediccionAPasado)




# LUEGO VIENE ESTO 
names(prediccionAPasado) <- "pred_target"
realYPrediccion <- cbind(datos.train,prediccionAPasado)
realYPrediccion$MAPE <- 
abs(100*(realYPrediccion$target-realYPrediccion$pred_target)/realYPrediccion$target)
mean(realYPrediccion$MAPE)

# EL ERROR IN-SAMPLING ES: 22.75363

##################   2.1 FUTURO   #########################
names(prediccionAFuturo) <- c("pred_target","SE")
realYPrediccionFut <- cbind(datos.test,prediccionAFuturo)


# CON CORRECCIÓN DE NELSON PARA LOG
realYPrediccionFut$MAPE <- 
abs(100*(realYPrediccionFut$target-exp(realYPrediccionFut$pred_target+0.5*realYPrediccionFut$SE^2))/realYPrediccionFut$target)
mean(realYPrediccionFut[,"MAPE"])


## EL ERROR OUT-SAMPLING: 29.91619 

## AL HABER ERRORES SOBRE 12 MESES

# MAPE A UN MES/UN DÍA ES: 31.01834
mean(realYPrediccionFut[1:1,"MAPE"])
# ESTO SIGUE SIN SER COMPARABLE.


# LA SOLUCIÓN PASA POR HACER 12 PREDICCIONES A FUTURO A 1 MES.

datosLog<-datos[,c("fecha","target")]
datosLog$log_target=log(datosLog$target)
datosLog.ts <- ts(datosLog$log_target, frequency = 12)




simulaPredicciones_Horizonte1 = function(data){
  # Extraemos los ordenes del modelo  -> (aquí realmente no haces nada aún, solo un comentario)

  # Inicializamos las variables necesarias
  vector_real = numeric()          # vector donde irás guardando los valores reales (escala original)
  vector_prediccion = numeric()    # vector para las predicciones a horizonte 1
  vector_mape = numeric()          # vector para guardar el MAPE en cada iteración

  data_aux = data[1:192,]          # uso 192 que es el numero de datos del train 


  data_aux = ts(data_aux$log_target, frequency = 12)


  for(i in 1:12){                



    modelo <- Arima(data_aux,
                     order = c(0,1,1),
                     seasonal = list(order = c(0,0,0), period = 12))
                                   # ajustas un modelo ARIMA(0,0,1)(0,0,0)[12]

    
    prediccion <- as.data.frame(predict(modelo, 
                                        n.ahead=1))


    
    vector_real[i] = exp(data[dim(datosLog)[1]-12+i,c("log_target")])



    vector_prediccion[i] = exp(prediccion$pred+0.5*prediccion$se^2)
                                   # aquí pasas de la escala del modelo (log) a la escala original
                                   # usando la corrección de Nelson:
                                   # E(Y) = exp(mu + 0.5 * sigma^2)

    # Calculamos mape
    vector_mape[i] = abs(vector_real[i] - vector_prediccion[i])*100/vector_real[i]



    # Recalculamos los datos_aux con un nuevo valor
    unaFilaMas=length(data_aux)+1   # decides ampliar en 1 la ventana de entrenamiento
    data_aux = data[1:unaFilaMas,]  # coges desde la primera observación hasta esa nueva fila
    data_aux = ts(data_aux$log_target, frequency = 12)



  }
  
  return(list(reales= vector_real,
              predicciones = vector_prediccion,
              error_mape = vector_mape))  

}


errores_a_horizonte1_a_futuro<-simulaPredicciones_Horizonte1(datosLog)

mean(errores_a_horizonte1_a_futuro$error_mape)


# MAPE out-of-sampling(h=1): 31.73246 
# SÉ QUE ESTO NO LO HAS PEDIDO, AUN ASÍ LO HE CALCULADO PORQUE ESTE MÉTODO ES EL QUE PERMITE UNA MEJOR COMPARACIÓN, CÁLCULANDO TODOS LOS VALORES A HORIZONTE 1




##############################################################
##################   3. FESTIVOS   ###########################
##############################################################

## FUNCIÓN DE CALENDARIO PARA EL CASO DE LAS MENSUALES


calculoExplicativasCalendario <- function(variableFecha, domingosYFestivosJuntos){
  
  #######################################
  #     Creación de todas las fechas    #
  #######################################
  

  
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


  
  calendario$p_01ene <- ifelse(calendario$diaMes==1 & calendario$mes==1 & !calendario$diaSemana %in% c(1, 7), 1, 0)
  calendario$p_06ene <- ifelse(calendario$diaMes==6 & calendario$mes==1 & !calendario$diaSemana %in% c(1, 7), 1, 0)
  calendario$p_19mar <- ifelse(calendario$diaMes==19 & calendario$mes==3 & !calendario$diaSemana %in% c(1, 7), 1, 0)
  calendario$p_01may <- ifelse(calendario$diaMes==1 & calendario$mes==5 & !calendario$diaSemana %in% c(1, 7), 1, 0)
  calendario$p_31may <- ifelse(calendario$diaMes==31 & calendario$mes==5 & !calendario$diaSemana %in% c(1, 7), 1, 0) # METO LA FIESTA DE CASTILLA LA MANCHA
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



# UNA VEZ DEFINIDOS TODOS LOS VALORES QUE A PRIORI SON MEJORES, METEMOS EL AJUSTE CON ESTO: 

ajuste3conCalendario <- Arima(datos.train.ts,
                              order = c(0,1,1),
                              seasonal = list(order = c(0,0,0), 
                                              period = 12),
                              xreg = calendarioTrain)

coeftest(ajuste3conCalendario)

## VEMOS QUE BISIESTO NO ES SIGNIFICATIVO AL 0.05. 
## ESTO QUE OBSERVAMOS TIENE TODO EL SENTIDO DEL MUNDO. PUES EL MOMENTO DE CONTRATACIÓN DE UNA HIPOTECA NO TIENE VENIR INFLUÍDO POR CUANDO SI ES UN AÑO BISIESTO O NO. 
## QUITAMOS LOS BISIESTOS PORQUE NO TIENE SENTIDO METER VARIBALES QUE NO EXPLICAN 

calendarioTrain <- 
  as.matrix(
    explicativasCalendarioTrain[,c("semanaSanta", "dt", "festivo")]
  )
ajuste3conCalendario <- Arima(datos.train.ts,
                              order = c(0,1,1),
                              seasonal = list(order = c(0,0,0), 
                                              period = 12),
                              xreg = calendarioTrain)

coeftest(ajuste3conCalendario)

# VEMOS QUE TODOS LOS COEFICIENTES SON SIGNIFICATIVOS AL 0,01. DECIDIMOS DEJAR ENTONCES EL RESTO DE VARIABLES TEMPORALES


cor.arma(ajuste3conCalendario)
# NO TENEMOS NINGÚN PROBLEMA DE CORRELACIÓN ENTRE LAS VARIABLES. DE HECHO LAS CORRELACIONES SON MUY BAJAS 

Box.test.2(residuals(ajuste3),
           nlag = c(6,12,18,24,30,36,42,48),
           type="Ljung-Box")


Box.test.2(residuals(ajuste3conCalendario),
           nlag = c(6,12,18,24,30,36,42,48),
           type="Ljung-Box")
# PARA VER QUE CON ESTO MEJORA EL P-VALOR. COMPROBAMOS QUE EL RUIDO BLANCO SE MEJORA PORQUE MANTIENE MAYOR ESTABILIDAD EL P-VALOR ALTO A LO LARGO DE TODOS LOS RETARDOS DE LA SERIE



##############################################################
##################   4. OULIERS   ############################
##############################################################

# EN PRIMER LUGAR IDENTIFICAMOS LOS OUTLIERS. VEMOS A VER SI EL DE LA CRISIS APARECE PRESENTE DE PARTIDA. ES UN BUEN MÉTODO PARA IDENTIFICARLA SIN NECESIDAD DE IR A MANO
listaOutliersTrain <- locate.outliers(ajuste3conCalendario$residuals,
                                      pars = coefs2poly(ajuste3conCalendario),
                                      types = c("AO", "LS", "TC"),cval=3)

listaOutliersTrain$abststat=abs(listaOutliersTrain$tstat)

# LO CRUZAMOS CON LA ORIGINAL PARA OBTENER FECHA

datos.train$ind <- as.numeric(rownames(datos.train))
listaOutliersTrainFecha <- merge(listaOutliersTrain, datos.train[,c("ind", "fecha")], by = "ind")
listaOutliersTrainFecha

# TENEMOS UNA LISTA EN LA QUE NO APARECE EL OULIER 

# ORDENAMOS LOS OUTLIERS POR SEVERIDAD
arrange(listaOutliersTrainFecha,desc(listaOutliersTrainFecha$abststat))

# TENEMOS UNA LISTA EN LA QUE NO APARECE EL OUTLIER REFERENCIADO A LA CRISIS. (PODRÍA SER UNO DE 2007, LUEGO VEREMOS QUE SÍ) SABEMOS QUE LA CRISIS SE PRODUJO EN EL AÑO 2008
# Se representan gráficamente los datos
graficoInicial <- ggplot(aes(x= fecha, y = target), data = datos.train) +
  geom_line(color = '#d84519',
            size = 1) + 
  xlab('FECHA') + ylab('hipotecas')

graficoInicial 

# VOLVEMOS A MIRAR EL GRÁFICO INICIAL. NOS DAMOS CUENTA DEL GRAN PICO QUE SE PRODUCE EN UNA DE LAS FECHAS.
# ES POR ELLO QUE NOS VAMOS A DATOS Y VEMOS NUESTROS DATOS DE LA SERIE EN ESOS AÑOS 

datos

# AL IMPRIMIRNOS LOS DATOS VEMOS LO SIGUIENTE

# 2007-08-01    561
# 2007-09-01    772
# 2007-10-01    907
# 2007-11-01    929
# 2007-12-01    477
# 2008-01-01   1046
# 2008-02-01    504
# 2008-03-01    366
# 2008-04-01    616
# 2008-05-01    545
# 2008-06-01    612

# VEMOS CLARAMENTE QUE EN 2008-01 HAY UN SALTO. ES UN VALOR PUNTUAL. PORQUE EN LOS DEMÁS VALORES VUELVE A LA TENDENCIA ANTERIOR: ES TIPO AO
# COGERÉ ESTE QUE ES EL QUE MÁS INTERPRETABILIDAD TIENE PARA LA FECHA DE LA CRISIS. AUQNUE TAMBIÉN HAYA OTROS EN EL 2007, TAMBIÉN COMO CERCA


# AHORA COMPROBAMOS SI SU INCLUSIÓN EN EL MODELO HACE QUE ESTE OULIER RESULTE SIGNIFICATIVO


outliersTrain <- outliers(c("AO"), c(66))
outliersVariablesTrain <- outliers.effects(outliersTrain, length(ajuste3conCalendario$residuals))
calendarioMasOutliersTrain <- as.matrix(cbind(calendarioTrain,outliersVariablesTrain))

# VOLVEMOS A TIRAR EL CÓDIGO DE NUEVO
ajuste3conCalendarioYOutliers <- Arima(datos.train.ts,
                                       order = c(0,1,1),
                                       seasonal = list(order = c(0,0,0), period = 12),
                                       xreg = calendarioMasOutliersTrain)

coeftest(ajuste3conCalendarioYOutliers)

# AL INCLUIRLO EN EL MODELO VEO QUE EL PARÁMETRO A066 NO RESULTA SIGNIFICATIVO. TIENE UN P-VALOR DE 0,371

# ES POR ELLO QUE NOS DECIDIMOS PROBAR POR EL OUTLIER QUE TAMBIÉN PUEDE SER RESULTADO DE LA CRISIS AUNQUE DE MANERA PREMATURA EN GUADALAJARA QUE ES EL DE ABRIL DEL 2007
# ESTE VALOR FUE SACADO DEL RANKING DE OULIERS EN SEGUNDA POR IMPORTANCIA. 
# ES CIERTO QUE ESTE PARÁMETRO PEUDE SER INTERESANTE PUES AL SER PREVIO A LA CRISIS ES CUANDO LA CONTRATACIÓN DE LAS HIPOTECAS ERA MUY ALTA, CONCEDIENDO ALGUNAS A CLIENTES CON ALTA PROBABILIDAD DE MORA QUE LUEGO RESULTARON EN IMPAGOS. 
# HEMOS COMPROBADO MEDIANTE LA FÓRMULA DEL RANKING QUE ESTE VALOR DEL OULIER ERA DEL TIPO TC, PUES DA UN PEQUEÑO SALTO PERO LUEGO SE VA DILUYENDO EL TIEMPO. 

outliersTrain <- outliers(c("TC"), c(52))
outliersVariablesTrain <- outliers.effects(outliersTrain, length(ajuste3conCalendario$residuals))
calendarioMasOutliersTrain <- as.matrix(cbind(calendarioTrain,outliersVariablesTrain))

ajuste3conCalendarioYOutliers <- Arima(datos.train.ts,
                                       order = c(0,1,1),
                                       seasonal = list(order = c(0,0,0), period = 12),
                                       xreg = calendarioMasOutliersTrain)

coeftest(ajuste3conCalendarioYOutliers)

# EFECTIVAMENTE ESTE VALOR PARA EL OULIERS TIENE ALTA SIGNIFICATIVIDAD. 
# ES SIGNIFICATIVA AL PARA UN NIVEL DE SIGNIFICACIÓN ALFA MENOR DEL 0.001. 
# ES IMPORTANTE CAPTURAR ESTE PARÁMETRO

#ANALIZAMOS SI HAY POSIBLES CORRELACIONES AUNQUE CABE ESPERAR QUE NO
cor.arma(ajuste3conCalendarioYOutliers)
# EFECTIVAMENTE, LAS CORRELACIONES SON MUY BAJAS, DE VALORES INFERIORES AL 0,2

Box.test.2(residuals(ajuste3conCalendarioYOutliers),
           nlag = c(6,12,18,24,30,36,42,48),
           type="Ljung-Box")

# EL RUIDO BLANCO SIGUE APARECIENDO PARA TODOS LOS RETARDOS. 
# LA INTENCIÓN NO ES MEJORAR PUES EL RUIDO BLANCO SINO CAPTURAR ALGUNOS FACOTRES QUE PUDIERON SER IMPORTANTES PARA INTERPRETAR LA EVOLUCIÓN EN LA CONTRATACIÓN DE HIPOTECAS

# A CONTINUACIÓN SE NOS PIDE INCLUIR ALGÚN OUTLIER QUE SEA DE DISTINTA TIPOLOGÍA. 
# ES POR ELLO QUE VOVLEMOS A RANKEAR LOS VALORES AUNQUE AHORA PARA EL MODELO AJUSTADO CON ESTA VARIABLES

# EN PRIMER LUGAR IDENTIFICAMOS LOS OUTLIERS. VEMOS A VER SI EL DE LA CRISIS APARECE PRESENTE DE PARTIDA. ES UN BUEN MÉTODO PARA IDENTIFICARLA SIN NECESIDAD DE IR A MANO
listaOutliersTrain <- locate.outliers(ajuste3conCalendarioYOutliers$residuals,
                                      pars = coefs2poly(ajuste3conCalendarioYOutliers),
                                      types = c("AO", "LS", "TC"),cval=3)

listaOutliersTrain$abststat=abs(listaOutliersTrain$tstat)

# LO CRUZAMOS CON LA ORIGINAL PARA OBTENER FECHA

datos.train$ind <- as.numeric(rownames(datos.train))
listaOutliersTrainFecha <- merge(listaOutliersTrain, datos.train[,c("ind", "fecha")], by = "ind")
listaOutliersTrainFecha

# LA TABLA NOS MUESTRA QUE EL VALOR QUE DEBEMOS DE COMTEMPLAR ES EL A0 PARA MARZO DEL 2012, NOMBRADO COMO OBSERVACIÓN COMO 111


##########################################################################################
#################                 MODELO FINAL                 ###########################
##########################################################################################

outliersTrain <- outliers(c("TC","AO"), c(52,111))
outliersVariablesTrain <- outliers.effects(outliersTrain, length(ajuste3conCalendario$residuals))
calendarioMasOutliersTrain <- as.matrix(cbind(calendarioTrain,outliersVariablesTrain))

ajuste3conCalendarioYOutliers <- Arima(datos.train.ts,
                                       order = c(0,1,1),
                                       seasonal = list(order = c(0,0,0), period = 12),
                                       xreg = calendarioMasOutliersTrain)

# CALCULAMOS LOS COEFICIENTES
coeftest(ajuste3conCalendarioYOutliers)

# TODOS LOS COEFICIENTES ME SALEN SIGNIFICATIVOS CON P-VALORES INFERIORES A 0.001

cor.arma(ajuste3conCalendarioYOutliers)

Box.test.2(residuals(ajuste3conCalendarioYOutliers),
           nlag = c(6,12,18,24,30,36,42,48),
           type="Ljung-Box")

# EFECTIVAMENTE TERMINAMOS CON UN RUIDO BLACO MUY SIGNIFICATIVO. VEMO QUE PARA EL RETARDO 48 TENEMOS UN PVALOR DE 0.7598415

Acf(ajuste3conCalendarioYOutliers$residuals, lag.max = 48, xlab = "Retardo",
    main= "Función de autocorrelación simple")


#FUNCIÓN DE AUTOCORRELACIÓN PARCIAL. PERMITE DARNOS EL NÚMERO DE AR()
Pacf(ajuste3conCalendarioYOutliers$residuals, lag.max = 48, xlab = "Retardo",
     main = "Función de autocorrelación parcial")

# PODRÍAMOS TERMINAR VIENDO LOS GRÁFICOS FINALES
# COMPROBAMOS POR LOS GRÁFICOS QUE TENEMOS QUE PARA TODOS LOS RETARDOS MENORES DE 48 TENEMOS LOS VALORES DENTRO DE LAS BANDAS (SALVO 36). 


################################### FIN DEL EXAMEN ##################################################


