
rm(list = ls())

# Se asigna el directorio donde están los datos

setwd("/Users/miguelfernandez/GitHub/ucm/Series_Temporales/")

# Carga de datos

datos <- read.csv("Ejemplo_Series.csv",sep=",")
datos$FECHA <- as.Date(datos$fecha, "%m/%d/%Y")
head(datos)

library(ggplot2)

ggplot(aes(x= FECHA, y = target), data = datos) + geom_line(color = '#d84519') + 
  xlab('FECHA') + ylab('Demanda de gas') + scale_x_date(date_breaks = '4 months', date_labels = "%m/%y")


###########################################################
# [3]. Se divide la muestra en entrenamiento y validación #
###########################################################

datos.train <- subset(datos, "2024-10-01">=FECHA)
datos.train.ts <- ts(datos.train$target, frequency = 12)

datos.validate <- subset(datos, FECHA>"2024-10-01")
datos.validate.ts <- ts(datos.validate$target, frequency=12)

# [4.1] ¿Es estacionaria en varianza?

# Se evalúa la necesidad de transformar la serie para hacerla
# estacionaria en varianza

# install.packages("MASS")
library(MASS)

box_cox <- boxcox(target ~ FECHA,
                  data = datos.train,
                  lambda = c(0, 0.5, 1))

lambda <- box_cox$x[which.max(box_cox$y)]
lambda

# [4.2] ¿Es estacionaria en media? (¿Hay que diferenciarla?)

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

# PODEMOS DECIR QUE ES ESTACIONARIA EN MEDIA AUNQUE DEBEMOS DE CORROBORARLO POSTERIORMENTE

adf_seasonal <- ur.df(datos.train.ts, type = "drift", lags = 12)
summary(adf_seasonal)

# Como el p-valor es bajo, podría no ser necesario hacer una diferencia estacional

library(forecast)

Acf(datos.train.ts, lag.max = 25, xlab = "Retardo",
    main= "Función de autocorrelación simple")

Pacf(datos.train.ts, lag.max = 25, xlab = "Retardo",
     main = "Función de autocorrelación parcial")


# [5.2]. Ajuste de modelo ARIMA

# [5.2.1]. AR(1)

library(forecast)

ajuste1 <- Arima(datos.train.ts,
                 order = c(1,0,0))
ajuste1

library(lmtest)
coeftest(ajuste1)

library(caschrono)
cor.arma(ajuste1)

# Test de ruido blanco
Acf(ajuste1$residuals, lag.max = 50, xlab = "Retardo", main="")
Pacf(ajuste1$residuals, lag.max = 25, xlab = "Retardo", main="")

Box.test.2(residuals(ajuste1),
           nlag = c(6,12,18,24,30,36,42,48),
           type="Ljung-Box")


# [5.2.2]. SAR(1)x(1)[12]
ajuste2 <- Arima(datos.train.ts,
                 order = c(1,0,0),
                 seasonal = list(order = c(1,0,0), period = 12))

ajuste2

coeftest(ajuste2)

cor.arma(ajuste2)


Acf(ajuste2$residuals, lag.max=25, xlab="Retardo", main="")
Pacf(ajuste2$residuals, lag.max=25, xlab="Retardo", main="")


# PICO ENORME DE MA(1) PICOS ENORMES EN 12, 24,... POR ACF

ajuste10 <- Arima(datos.train.ts,
                 order = c(1,1,1),
                 seasonal = list(order = c(1,1,0), period = 12))

ajuste10

coeftest(ajuste10)

cor.arma(ajuste10)
Acf(ajuste10$residuals, lag.max=25, xlab="Retardo", main="")
Pacf(ajuste10$residuals, lag.max=25, xlab="Retardo", main="")


ajuste11 <- Arima(datos.train.ts,
                  order = c(1,1,1),
                  seasonal = list(order = c(1,1,1), period = 12))

Acf(ajuste11$residuals, lag.max = 50, main = "ACF residuos ajuste11")
Pacf(ajuste11$residuals, lag.max = 50, main = "PACF residuos ajuste11")
coeftest(ajuste11)
cor.arma(ajuste11)

Box.test.2(residuals(ajuste11),
           nlag = c(6,12,18,24,30,36,42,48),
           type="Ljung-Box")


ajuste12 <- Arima(datos.train.ts,
                  order = c(2,1,1),
                  seasonal = list(order = c(1,1,1), period = 12))

Acf(ajuste12$residuals, lag.max = 50, main = "ACF residuos ajuste11")
Pacf(ajuste21$residuals, lag.max = 50, main = "PACF residuos ajuste11")
coeftest(ajuste12)
cor.arma(ajuste12)

Box.test.2(residuals(ajuste11),
           nlag = c(6,12,18,24,30,36,42,48),
           type="Ljung-Box")

#CORRELACIONES SON ALTA 

ajuste13 <- Arima(datos.train.ts,
                  order = c(2,1,0),
                  seasonal = list(order = c(1,1,1), period = 12))

Acf(ajuste13$residuals, lag.max = 50, main = "ACF residuos ajuste11")
Pacf(ajuste13$residuals, lag.max = 50, main = "PACF residuos ajuste11")
coeftest(ajuste13)
cor.arma(ajuste13)

Box.test.2(residuals(ajuste13),
           nlag = c(6,12,18,24,30,36,42,48),
           type="Ljung-Box")

## ESTO YA SÍ QUE ES RUIDO BLENCO ENTONCES PODEMOS PROCEDER A LA SIGUIENTE FASE

# FINALMENTE 

library(forecast)

ajuste.auto <- auto.arima(
  datos.train.ts,
  seasonal      = TRUE,   # busca parte SARIMA
  stepwise      = FALSE,  # explora más combinaciones (más lento pero más exhaustivo)
  approximation = FALSE,  # sin aproximaciones: cálculo exacto
  trace         = TRUE    # muestra por pantalla todos los modelos probados
)


ajuste13

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

calendario.train <- subset(calendario, "2024-10-01">=FECHA)
calendario.train <- as.matrix(calendario.train[,4:ncol(calendario.train)])


ajuste13ConFestivos <- Arima(datos.train.ts,
                            order = c(2,1,0),
                            seasonal = list(order = c(1,1,1), period=12),
                            xreg = calendario.train)

ajuste13ConFestivos
coeftest(ajuste13ConFestivos)

Acf(ajuste13ConFestivos$residuals, lag.max=25, xlab="Retardo", main="")
Pacf(ajuste13ConFestivos$residuals, lag.max=25, xlab="Retardo", main="")

Box.test.2(residuals(ajuste6ConFestivos),
           nlag = c(6,12,18,24,30,36,42,48),
           type="Ljung-Box")