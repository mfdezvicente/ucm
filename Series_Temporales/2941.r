rm(list = ls())

# Se asigna el directorio donde están los datos
setwd("/Users/miguelfernandez/GitHub/ucm/Series_Temporales/")

# Carga de datos
datos <- read.csv("demandaGasIndustrial.csv",sep=";")

# Se convierte la FECHA de factor a objeto de tipo FECHA
datos$FECHA <- as.Date(datos$FECHA, "%m/%d/%Y")
head(datos)
