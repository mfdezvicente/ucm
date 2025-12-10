---
title: "Introducción a las Series Temporales"
output: html_document
---

# Lectura de datos

```{r setup, include=FALSE, echo=FALSE}

rm(list = ls())

# Se asigna el directorio donde están los datos

setwd("C:/Users/Usuario/GitHub/ucm/Series_Temporales/")

datos <- read.csv("demandaGasIndustrial.csv",sep=";")

```

# Representación gráfica de la serie

```{r}

# Para poder representar la serie, se convierte la FECHA # que es un factor a un objeto de tipo FECHA

datos$FECHA <- as.Date(datos$FECHA, "%m/%d/%Y")

# install.packages("ggplot2")
library(ggplot2)

ggplot(aes(x= FECHA, y = DEMANDA_GAS), data = datos) + geom_line(color = 'blue') + 
  xlab('FECHA') + ylab('Demanda de gas') + scale_x_date(date_breaks = '4 months', date_labels = "%m/%y")

```
```{r}

# Para declarar variables asociadas a las fechas: anyo, mes, día del mes, dia de la semana

# install.packages("lubridate")
library(lubridate)

datos$ANYO <- year(datos$FECHA) #Te permite agrupar por años.
datos$MES <- month(datos$FECHA) #Te permite agrupar por mes.
datos$DIAMES <- day(datos$FECHA)
datos$DIASEMANA <- wday(datos$FECHA) # 0 = domingo, 1 = lunes, etc.
# Lo que estás haciendo aquí es partir la fecha en piezas para poder analizar patrones:




# Vamos a generar un gráfico para cada año y mes

combinaciones_anyo_mes <- unique(datos[, c("ANYO", "MES")])

# Recorrer cada año-mes y generar gráfico. Con esto puedo ver como se comporta cada mes de cada año.

for (i in 1:nrow(combinaciones_anyo_mes)) {
  
  ANYO_AUX <- combinaciones_anyo_mes$ANYO[i]
  MES_AUX <- combinaciones_anyo_mes$MES[i]
  
  # Filtrar datos para cada año y mes
  datos_aux <- subset(datos, ANYO == ANYO_AUX & MES == MES_AUX)
  
  grafico_aux <- ggplot(datos_aux, aes(x = day(FECHA), y = DEMANDA_GAS)) +
    geom_line(color = "blue") +
    geom_line(aes(y = DEMANDA_GAS), color = "blue", size = 1.2) +  geom_point(aes(y = DEMANDA_GAS), color = "blue", size = 2) +    
    labs(
      title = paste("Demanda diaria de gas -", MES_AUX, ANYO_AUX),
      x = "FECHA",
      y = "Demanda de gas"
    ) +
    theme_minimal()
  
  print(grafico_aux)
  
}

```



# Separación en entrenamiento y test

```{r}

datos.train <- subset(datos, "1996-07-01"<=FECHA & FECHA<="1999-06-30")
datos.test <- subset(datos, FECHA>"1999-06-30")

```

# Ajuste de un modelo de regresión frente al tiempo

```{r}

# El tiempo (T), será un contador inicializado la primera fecha del 
# dataset de entrenamiento

datos.train$T <- as.numeric(datos.train$FECHA - min(datos.train$FECHA))+1

# Ajustamos el modelo de regresión lineal

regresionFrenteAlTiempo <- lm(DEMANDA_GAS ~ T, data = datos.train)

regresionFrenteAlTiempo
# Este paquete es para saber los coeficientes
# install.packages("lmtest")
library(lmtest)

coeftest(regresionFrenteAlTiempo)

# Gráficamente

# Para representar la fórmula sobre el gráfico

coefs_regresion <- coef(regresionFrenteAlTiempo)
formulaRegresion <- sprintf("DEMANDA_GAS = %.2f + %.2f * T", coefs_regresion[1], coefs_regresion[2])


# NUBE DE PUNTOS, CON LA REGRESIÓN
ggplot(datos.train, aes(x = FECHA, y = DEMANDA_GAS)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  annotate("text", x = min(datos.train$FECHA), y = max(datos.train$DEMANDA_GAS),
           label = formulaRegresion, hjust = 0, vjust = 1.5, size = 5, color = "black") +
  labs(title = "Demanda de gas en función del tiempo",
       x = "Fecha", y = "Demanda de gas") +
  theme_minimal()

# Podemos aplicar la fórmula a los datos, para obtener las predicciones

datos.train$PREDICCION <- predict(regresionFrenteAlTiempo, newdata = datos.train)

# Calculamos el MAPE

# install.packages("Metrics")
library(Metrics)

mape_train <- mape(datos.train$DEMANDA_GAS, datos.train$PREDICCION)

print(paste("MAPE:", round(100 * mape_train, 2), "%"))

# MAPE = 13.74 % 

```

# Aplicación modelo a test

```{r}

# Continuamos el contador de fecha, tomando nuevamente como referencia
# la mínima fecha de entrenamiento

datos.test$T  <- as.numeric(datos.test$FECHA - min(datos.train$FECHA))+1

datos.test$PREDICCION  <- predict(regresionFrenteAlTiempo, newdata = datos.test)

mape_test  <- mape(datos.test$DEMANDA_GAS, datos.test$PREDICCION)

# Unimos los dos conjuntos para hacer el gráfico
# identificando cada tabla por separado

datos.train$tipo <- "Entrenamiento"
datos.test$tipo  <- "Test"

trainMasTest <- rbind(datos.train, datos.test)

ggplot(trainMasTest, aes(x = FECHA, y = DEMANDA_GAS, color = tipo)) +
  geom_point() +
  geom_line(data = trainMasTest, aes(y = PREDICCION), color = "red", size = 1.2) +
  labs(title = "Ajuste de regresión lineal sobre datos y test",
       subtitle = paste("MAPE TRAIN =", round(100 * mape_train, 2), "%",
                        "| MAPE TEST =", round(100 * mape_test, 2), "%"),
       x = "Fecha", y = "Demanda de gas") +
  annotate("text", x = min(trainMasTest$FECHA), y = max(trainMasTest$DEMANDA_GAS),
           label = formulaRegresion, hjust = 0, vjust = 1.5, size = 5, color = "black") +
  scale_color_manual(values = c("Entrenamiento" = "blue", "Test" = "cyan")) +
  theme_minimal()

# Veamos el ajuste del último mes

fecha_max <- max(trainMasTest$FECHA)

ultimo_mes <- subset(trainMasTest, FECHA >= (fecha_max - 30))

ggplot(ultimo_mes, aes(x = FECHA)) +
  geom_line(aes(y = DEMANDA_GAS, color = tipo, group = tipo), size = 1) +  # Línea real
  geom_point(aes(y = DEMANDA_GAS, color = tipo), size = 2) +               # Puntos reales
  geom_line(aes(y = PREDICCION), color = "red", size = 1.2) +                    # Recta de regresión sobre ambos (datos + test)
  labs(title = "Zoom al último mes con regresión evaluada en test",
       subtitle = paste("MAPE train =", round(100 * mape_train, 2), "%",
                        "| MAPE test =", round(100 * mape_test, 2), "%"),
       x = "Fecha", y = "Demanda de gas") +
  scale_color_manual(values = c("Entrenamiento" = "blue", "Test" = "cyan")) +
  theme_minimal()

```

# Gráficos interactivos

```{r}

# install.packages("plotly")
library(plotly)

# Creamos el gráfico con ggplot2
library(ggplot2)

graficoPlotly <- ggplot(trainMasTest, aes(x = FECHA)) +
  geom_line(aes(y = DEMANDA_GAS, color = tipo, group = tipo), size = 1) +
  geom_point(aes(y = DEMANDA_GAS, color = tipo), size = 2) +
  geom_line(aes(y = PREDICCION), color = "red", size = 1.2, linetype = "dashed") +
  labs(title = "Demanda de gas con regresión ajustada (modelo en datos)",
       subtitle = paste("MAPE train =", round(100 * mape_train, 2), "%",
                        "| MAPE test =", round(100 * mape_test, 2), "%"),
       x = "Fecha", y = "Demanda de gas",
       color = "Tipo de dato") +
  scale_color_manual(values = c("Entrenamiento" = "blue", "Test" = "cyan")) +
  theme_minimal()

# Convertimos a plotly

ggplotly(graficoPlotly)

```

# Regresión frente al tiempo con todos los datos (sin diferenciar train y test)

```{r}

regresionFrenteAlTiempo <- lm(DEMANDA_GAS ~ T, data = trainMasTest)

regresionFrenteAlTiempo

coeftest(regresionFrenteAlTiempo)

trainMasTest$PREDICCION <- predict(regresionFrenteAlTiempo, newdata = trainMasTest)

# Calculamos el MAPE

mape_regresionFrenteAlTiempo <- mape(trainMasTest$DEMANDA_GAS, trainMasTest$PREDICCION)

print(paste("MAPE:", round(100 * mape_regresionFrenteAlTiempo, 2), "%"))

# MAPE = 13.72 % (está entre el 13.74% y el 13.44% de train y test respectivamente)

```

# Modelo AR(1): X(t) = phi_1*X(t-1)

```{r}

# Vamos a capturar la tendencia de una forma alternativa
# fijándonos en el último dato real

trainMasTest$DEMANDA_GAS_1=(dplyr::lag(trainMasTest$DEMANDA_GAS, 1))

regresion_AR_1 <- lm(DEMANDA_GAS ~ DEMANDA_GAS_1, data = trainMasTest)

regresion_AR_1

coeftest(regresion_AR_1)

trainMasTest$PREDICCION <- predict(regresion_AR_1, newdata = trainMasTest)

# Gráficamente

fecha_max <- max(trainMasTest$FECHA)
ultimo_mes <- subset(trainMasTest, FECHA >= (fecha_max - 30))

# Para representar la fórmula sobre el gráfico

coefs_regresion <- coef(regresion_AR_1)

formulaRegresion <- paste(
  "DEMANDA_GAS(t) =",
  round(coefs_regresion[1], 2),
  "+",
  round(coefs_regresion[2], 2),
  "* DEMANDA_GAS(t-1)"
)

graficoPlotly <- ggplot(ultimo_mes, aes(x = FECHA)) +
  
  geom_line(aes(y = DEMANDA_GAS), color = "blue", size = 1.2) +               geom_point(aes(y = DEMANDA_GAS), color = "blue", size = 2) +                geom_line(aes(y = PREDICCION), color = "red", size = 1.2) +
  geom_point(aes(y = PREDICCION), color = "red", size = 2) +

  labs(title = "Modelo AR(1) aplicado a la demanda de gas - Último mes",
       subtitle = formulaRegresion,
       x = "Fecha", y = "Demanda de gas") +
  theme_minimal()

ggplotly(graficoPlotly)

# Calculamos el MAPE

trainMasTest_sinNA <- trainMasTest <- na.omit(trainMasTest)

mape_regresion_AR_1 <- mape(trainMasTest_sinNA$DEMANDA_GAS, trainMasTest_sinNA$PREDICCION)

print(paste("MAPE:", round(100 * mape_regresion_AR_1, 2), "%"))

# MAPE: 6.94 % < 13.72 %

# El modelo no captura la estacionalidad

```

# Modelo AR(7): X(t) = phi_7*X(t-7)

```{r}

# Vamos a capturar solo la estacionalidad
# fijándonos en el mismo dato de la semana pasada

trainMasTest$DEMANDA_GAS_7=(dplyr::lag(trainMasTest$DEMANDA_GAS, 7))

regresion_AR_7 <- lm(DEMANDA_GAS ~ DEMANDA_GAS_7, data = trainMasTest)

regresion_AR_7

coeftest(regresion_AR_7)

trainMasTest$PREDICCION <- predict(regresion_AR_7, newdata = trainMasTest)

# Gráficamente

fecha_max <- max(trainMasTest$FECHA)
ultimo_mes <- subset(trainMasTest, FECHA >= (fecha_max - 30))

# Para representar la fórmula sobre el gráfico

coefs_regresion <- coef(regresion_AR_7)

formulaRegresion <- paste(
  "DEMANDA_GAS(t) =",
  round(coefs_regresion[1], 2),
  "+",
  round(coefs_regresion[2], 2),
  "* DEMANDA_GAS(t-7)"
)

graficoPlotly <- ggplot(ultimo_mes, aes(x = FECHA)) +
  
  geom_line(aes(y = DEMANDA_GAS), color = "blue", size = 1.2) +               geom_point(aes(y = DEMANDA_GAS), color = "blue", size = 2) +                geom_line(aes(y = PREDICCION), color = "red", size = 1.2) +
  geom_point(aes(y = PREDICCION), color = "red", size = 2) +

  labs(title = "Modelo AR(7) aplicado a la demanda de gas - Último mes",
       subtitle = formulaRegresion,
       x = "Fecha", y = "Demanda de gas") +
  theme_minimal()

ggplotly(graficoPlotly)

# Calculamos el MAPE

trainMasTest_sinNA <- trainMasTest <- na.omit(trainMasTest)

mape_regresion_AR_7 <- mape(trainMasTest_sinNA$DEMANDA_GAS, trainMasTest_sinNA$PREDICCION)

print(paste("MAPE:", round(100 * mape_regresion_AR_7, 2), "%"))

# MAPE: 7.65 % > 6.94 %

# El modelo captura la estacionalidad, pero no la tendencia

```

# Modelo autorregresivo con tendencia AR(1) y estacionalidad AR(7): X(t)= phi_1*X(t-1)+phi_7*X(t-7)

```{r}

# Vamos a capturar la tendencia y la estacionalidad

regresion_AR_1_7 <- lm(DEMANDA_GAS ~ DEMANDA_GAS_1 + DEMANDA_GAS_7, data = trainMasTest)

regresion_AR_1_7

coeftest(regresion_AR_1_7)

trainMasTest$PREDICCION <- predict(regresion_AR_1_7, newdata = trainMasTest)

# Gráficamente

fecha_max <- max(trainMasTest$FECHA)
ultimo_mes <- subset(trainMasTest, FECHA >= (fecha_max - 30))

# Para representar la fórmula sobre el gráfico

coefs_regresion <- coef(regresion_AR_1_7)

formulaRegresion <- paste(
  "DEMANDA_GAS(t) =",
  round(coefs_regresion[1], 2),
  "+",
  round(coefs_regresion[2], 2),
  "* DEMANDA_GAS(t-1)",
  "+",
  round(coefs_regresion[3], 2),
  "* DEMANDA_GAS(t-7)"
)

graficoPlotly <- ggplot(ultimo_mes, aes(x = FECHA)) +
  
  geom_line(aes(y = DEMANDA_GAS), color = "blue", size = 1.2) +               geom_point(aes(y = DEMANDA_GAS), color = "blue", size = 2) +                geom_line(aes(y = PREDICCION), color = "red", size = 1.2) +
  geom_point(aes(y = PREDICCION), color = "red", size = 2) +

  labs(title = "Modelo AR con tendencia y estacionalidad aplicado a la demanda de gas - Último mes",
       subtitle = formulaRegresion,
       x = "Fecha", y = "Demanda de gas") +
  theme_minimal()

ggplotly(graficoPlotly)

# Calculamos el MAPE

trainMasTest_sinNA <- trainMasTest <- na.omit(trainMasTest)

mape_regresion_AR_1_7 <- mape(trainMasTest_sinNA$DEMANDA_GAS, trainMasTest_sinNA$PREDICCION)

print(paste("MAPE:", round(100 * mape_regresion_AR_1_7, 2), "%"))

# MAPE: 5.85 % < 6.94 %

# El modelo es el que menor error tiene, pero falla los días frontera
# ¿Por qué?

```

# Modelo autorregresivo AR(8): X(t)= phi_1*X(t-1)+phi_7*X(t-7)+phi_8*X(t-8)

```{r}

trainMasTest$DEMANDA_GAS_8=(dplyr::lag(trainMasTest$DEMANDA_GAS, 8))

regresion_AR_1_7_8 <- lm(DEMANDA_GAS ~ DEMANDA_GAS_1 + DEMANDA_GAS_7  + DEMANDA_GAS_8, data = trainMasTest)

regresion_AR_1_7_8

coeftest(regresion_AR_1_7_8)

trainMasTest$PREDICCION <- predict(regresion_AR_1_7_8, newdata = trainMasTest)

# Gráficamente

fecha_max <- max(trainMasTest$FECHA)
ultimo_mes <- subset(trainMasTest, FECHA >= (fecha_max - 30))

# Para representar la fórmula sobre el gráfico

coefs_regresion <- coef(regresion_AR_1_7_8)

formulaRegresion <- paste(
  "DEMANDA_GAS(t) =",
  round(coefs_regresion[1], 2),
  "+",
  round(coefs_regresion[1], 2),
  "* DEMANDA_GAS(t-1)",
  "+",
  round(coefs_regresion[3], 2),
  "* DEMANDA_GAS(t-7)",
  "+",
  round(coefs_regresion[4], 2),
  "* DEMANDA_GAS(t-8)"
)

graficoPlotly <- ggplot(ultimo_mes, aes(x = FECHA)) +
  
  geom_line(aes(y = DEMANDA_GAS), color = "blue", size = 1.2) +               geom_point(aes(y = DEMANDA_GAS), color = "blue", size = 2) +                geom_line(aes(y = PREDICCION), color = "red", size = 1.2) +
  geom_point(aes(y = PREDICCION), color = "red", size = 2) +

  labs(title = "Modelo AR con tendencia y estacionalidad aplicado a la demanda de gas - Último mes",
       subtitle = formulaRegresion,
       x = "Fecha", y = "Demanda de gas") +
  theme_minimal()

ggplotly(graficoPlotly)

# Calculamos el MAPE

trainMasTest_sinNA <- trainMasTest <- na.omit(trainMasTest)

mape_regresion_AR_1_7_8 <- mape(trainMasTest_sinNA$DEMANDA_GAS, trainMasTest_sinNA$PREDICCION)

print(paste("MAPE:", round(100 * mape_regresion_AR_1_7_8, 3), "%"))

# MAPE: 3.592 % < 5.85 %

```

# Inclusión de una variable para reflejar el efecto de un día festivo: X(t)= phi_1*X(t-1)+phi_7*X(t-7)+phi_8*X(t-8)+w*dia_1999_10_12(t)

```{r}

# ¿Qué pasa en un día festivo?

fecha_max <- max(trainMasTest$FECHA)
rango_meses <- subset(trainMasTest, MES == 10 & ANYO == 1999)

graficoPlotly <- ggplot(rango_meses, aes(x = FECHA)) +
  
  geom_line(aes(y = DEMANDA_GAS), color = "blue", size = 1.2) +               geom_point(aes(y = DEMANDA_GAS), color = "blue", size = 2) +                geom_line(aes(y = PREDICCION), color = "red", size = 1.2) +
  geom_point(aes(y = PREDICCION), color = "red", size = 2) +

  labs(title = "Modelo AR con tendencia y estacionalidad aplicado a la demanda de gas - Último mes",
       subtitle = formulaRegresion,
       x = "Fecha", y = "Demanda de gas") +
  theme_minimal()

ggplotly(graficoPlotly)

dia_1999_10_12 <- subset(trainMasTest_sinNA, MES == 10 & DIAMES == 12)
mape_1999_10_12 <- mape(dia_1999_10_12$DEMANDA_GAS, dia_1999_10_12$PREDICCION)

print(paste("MAPE 12 de octubre de 1999 (sin corregir):", round(100 * mape_1999_10_12, 2), "%"))

# Declaramos una variable dummy asociada a la fecha en cuestión

trainMasTest$fiesta_1999_10_12 <- ifelse(trainMasTest$MES == 10 &
                                         trainMasTest$DIAMES == 12, 1, 0)

regresion_AR_1_7_8_FEST <- lm(DEMANDA_GAS ~ DEMANDA_GAS_1 + DEMANDA_GAS_7  + DEMANDA_GAS_8 + fiesta_1999_10_12, data = trainMasTest)

regresion_AR_1_7_8_FEST

coeftest(regresion_AR_1_7_8_FEST)

trainMasTest$PREDICCION <- predict(regresion_AR_1_7_8_FEST, newdata = trainMasTest)

# Para representar la fórmula sobre el gráfico

coefs_regresion <- coef(regresion_AR_1_7_8_FEST)

formulaRegresion <- paste(
  "DEMANDA_GAS(t) =",
  round(coefs_regresion[1], 2),
  "+",
  round(coefs_regresion[1], 2),
  "* DEMANDA_GAS(t-1)",
  "+",
  round(coefs_regresion[3], 2),
  "* DEMANDA_GAS(t-7)",
  "+",
  round(coefs_regresion[4], 2),
  "* DEMANDA_GAS(t-8)",
  "+",
  round(coefs_regresion[5], 2),
  "* fiesta_1999_10_12(t)"
)

rango_meses <- subset(trainMasTest, MES == 10 & ANYO == 1999)

graficoPlotly <- ggplot(rango_meses, aes(x = FECHA)) +
  
  geom_line(aes(y = DEMANDA_GAS), color = "blue", size = 1.2) +               geom_point(aes(y = DEMANDA_GAS), color = "blue", size = 2) +                geom_line(aes(y = PREDICCION), color = "red", size = 1.2) +
  geom_point(aes(y = PREDICCION), color = "red", size = 2) +

  labs(title = "Modelo AR con tendencia y estacionalidad aplicado a la demanda de gas - Último mes",
       subtitle = formulaRegresion,
       x = "Fecha", y = "Demanda de gas") +
  theme_minimal()

ggplotly(graficoPlotly)

# Calculamos el MAPE

trainMasTest_sinNA <- trainMasTest <- na.omit(trainMasTest)

mape_regresion_AR_1_7_8_FEST <- mape(trainMasTest_sinNA$DEMANDA_GAS, trainMasTest_sinNA$PREDICCION)

print(paste("MAPE:", round(100 * mape_regresion_AR_1_7_8_FEST, 3), "%"))

# MAPE: 3.586 % < 3.592 %

# ¿Cómo arreglar el del 13 de octubre?

```


