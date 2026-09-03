#Práctica 6.1

#Cuando usamos modelos dinamicos el modelo que usamos es Empleo_(i,t) = \beta_0 + \gamma Empleo (i, t-1)  + \beta_1 salarios_(i,t)
#+...+u_(i,t)

#El problema es que la variable Empleo (i, t-1) tiene una correlación con el error por lo que hay que usar variable instrumental.
#En concreto se suele optar por usar:

#Empleo (i,t-2) o Empleo (i,t-2) - Empleo (i,t-3) que deben cumplir la restricción de inclusión Cov (inst, Empleo_(i,t-1)) \= 0 y la 
#restrición de exclusión Cov (inst, u_(i,t)) = 0. Que si el modelo se cumple, sí que se cumple para las propuestas.

#Vamos a estudiar siete modelos dinámicos distintos:

# 1. Modelo Pool del tema anterior
# 2. Modelo con efectos fijos del tema anterior
# 3. Modelos con efectos aleatorios del tema anterior
# 4. Modelos con efectos fijos y de instrumento opcion 1
# 5. Modelos con efectos aleatorios y de instrumento opcion 1
# 6. Modelos con efectos fijos y de instrumento opcion 2
# 7. Modelos con efectos aleatorios y de instrumento opcion 2
rm(list = ls())

library(readxl)
library(dplyr)
library(plm)
library(lmtest)
library(sandwich)
library(AER)


datos <- read_excel("C:/Users/Usuario/GitHub/ucm/Econometría_Aplicada/ENTREGA_5/empresas.xlsx")

library(plm)
pdata <- pdata.frame(datos, index = c("Empresa", "Anio"))

pdim(pdata)

#En este curso no se estudia perp el no tener paneles balanceados es un problema que en cursos mas avanzados se estudia, 

#Vemos el modelo pool del tema anterior:

modelo_pool <- plm(Empleo ~ Salarios + Capital + Produccion, data = pdata, model = "pooling")
summary(modelo_pool)

#Vemos el modelo con efectos fijos del tema anterior:

modelo_ef <- plm(Empleo ~ Salarios + Capital + Produccion, data = pdata, model = "within")
summary(modelo_ef)

#Comprobación de la hipótesis de efectos fijos:

pFtest(modelo_ef, modelo_pool)

#Preferimos efectos fijos al modelo pool porque el p-valor es menor que 0.05,

#Vemos el modelo con efectos aleatorios del tema anterior:

modelo_ea <- plm(Empleo ~ Salarios + Capital + Produccion, data = pdata, model = "random")
summary(modelo_ea)


#Comprobación de la hipótesis de efectos aleatorios:

plmtest(modelo_ea, type = "bp")

#Preferimos efectos aleatorios al modelo pool porque el p-valor es menor que 0.05,

#Comprobación de la hipótesis de efectos fijos frente a efectos aleatorios:
phtest(modelo_ef, modelo_ea)

#Preferimos efectos fijos al modelo de efectos fijos porque el p-valor es menor que 0.05,

pdata$Empleo_1 <- lag(pdata$Empleo, 1)
pdata$Empleo_2 <- lag(pdata$Empleo, 2)
pdata$Empleo_3 <- lag(pdata$Empleo, 3)

pdata$VI = pdata$Empleo_2 - pdata$Empleo_3

#Estimacion pool con regresor endongeno e instrumento Empleo_2  (Esto no tiene mucha interpretacion)

modelo_pool_re <- plm(Empleo ~ Empleo_1 + Salarios + Capital + Produccion
                                |Empleo_2 + Salarios + Capital + Produccion
                                , data = pdata, model = "pooling")
summary(modelo_pool_re)

#Estimacion con efectos fijos con regresor endongeno e instrumento Empleo_2
#No hace falta mirar la restriccion de exclusion o inclusion porque ya e cumple segun el modelo.

modelo_ef_re1 <- plm(Empleo ~ Empleo_1 + Salarios + Capital + Produccion
                                |Empleo_2 + Salarios + Capital + Produccion
                                , data = pdata, model = "within")
summary(modelo_ef_re1)

#Como Empleo_1 no es significativo, se desecha el modelo con Empleo_2 y se pasa a estimar el modelo con el instrumento VI

#Estimacion con efectos fijos e instrumento VI

modelo_ef_re2 <- plm(Empleo ~ Empleo_1 + Salarios + Capital + Produccion
                                |VI + Salarios + Capital + Produccion
                                , data = pdata, model = "within")
summary(modelo_ef_re2)
#Como Empleo_1 no es significativo, se desecha el modelo con Empleo_2 

#Estimacion con efectos aleatorios con regresor endongeno e instrumento Empleo_2

modelo_ea_re1 <- plm(Empleo ~ Empleo_1 + Salarios + Capital + Produccion
                                |Empleo_2 + Salarios + Capital + Produccion
                                , data = pdata, model = "random")
summary(modelo_ea_re1)

#Estimacion con efectos aleatorios e instrumento VI

modelo_ea_re2 <- plm(Empleo ~ Empleo_1 + Salarios + Capital + Produccion
                                |VI + Salarios + Capital + Produccion
                                , data = pdata, model = "random")
summary(modelo_ea_re2)

#Como no hay variables significativas se desecha

#Por lo tanto, los dos modelos que nos pueden generar duas son en realidad el modelo de efectos fijos con regresores exogenos y de efectos 
#aleatorios con regresor endógeno de empleo_2.

# PRÁCTICA 6.2

# Cargamos las librerías necesarias
library(readxl)
library(plm)

# Cargamos la base de datos
datos2 <- read_excel("C:/Users/Usuario/GitHub/ucm/Econometría_Aplicada/ENTREGA_5/aerolineas.xlsx")

# Definimos la estructura de datos de panel
# aerolinea identifica a cada individuo y anio identifica el tiempo
pdatos2 <- pdata.frame(datos2, index = c("aerolinea", "anio"))

# Comprobamos las dimensiones del panel
pdim(pdatos2)

# El panel es balanceado: todas las aerolíneas tienen el mismo número de años


# MODELOS ESTÁTICOS

# Modelo agrupado o pooling
# No controla efectos individuales específicos de cada aerolínea
modelo_pool <- plm(Coste ~ Produccion + PrCombust + Carga,
                   data = pdatos2,
                   model = "pooling")
summary(modelo_pool)

# Modelo de efectos fijos
# Controla las características propias de cada aerolínea constantes en el tiempo
modelo_ef <- plm(Coste ~ Produccion + PrCombust + Carga,
                 data = pdatos2,
                 model = "within")
summary(modelo_ef)

# Modelo de efectos aleatorios
# Supone que los efectos individuales no están correlacionados con las explicativas
modelo_ea <- plm(Coste ~ Produccion + PrCombust + Carga,
                 data = pdatos2,
                 model = "random")
summary(modelo_ea)


# CONTRASTES DE SELECCIÓN DEL MODELO

# Test F: compara pooling frente a efectos fijos
# H0: no existen efectos individuales
# H1: existen efectos individuales
pFtest(modelo_ef, modelo_pool)

# Test de Breusch-Pagan: compara pooling frente a efectos aleatorios
# H0: no existen efectos aleatorios
# H1: existen efectos aleatorios
plmtest(modelo_ea, type = "bp")

# Test de Hausman: compara efectos fijos frente a efectos aleatorios
# H0: efectos aleatorios es consistente
# H1: efectos fijos es preferible
phtest(modelo_ef, modelo_ea)


# MODELOS DINÁMICOS CON VARIABLES INSTRUMENTALES

# Creamos los retardos del coste
pdatos2$Coste_1 <- lag(pdatos2$Coste, 1)
pdatos2$Coste_2 <- lag(pdatos2$Coste, 2)
pdatos2$Coste_3 <- lag(pdatos2$Coste, 3)

# Creamos el segundo instrumento como diferencia de retardos
pdatos2$VI <- pdatos2$Coste_2 - pdatos2$Coste_3


# Modelo dinámico pooling con Coste_2 como instrumento
# Se estima solo como ejemplo, porque no controla efectos individuales
modelo_pool_re <- plm(Coste ~ Coste_1 + Produccion + PrCombust + Carga
                      | Coste_2 + Produccion + PrCombust + Carga,
                      data = pdatos2,
                      model = "pooling")
summary(modelo_pool_re)


# Modelo dinámico de efectos fijos con Coste_2 como instrumento
# Coste_1 se instrumenta con Coste_2 para corregir endogeneidad
modelo_ef_re1 <- plm(Coste ~ Coste_1 + Produccion + PrCombust + Carga
                     | Coste_2 + Produccion + PrCombust + Carga,
                     data = pdatos2,
                     model = "within")
summary(modelo_ef_re1)


# Modelo dinámico de efectos fijos con VI como instrumento
# Al usar Coste_3 se pierde un periodo adicional de información
modelo_ef_re2 <- plm(Coste ~ Coste_1 + Produccion + PrCombust + Carga
                     | VI + Produccion + PrCombust + Carga,
                     data = pdatos2,
                     model = "within")
summary(modelo_ef_re2)


# Modelo dinámico de efectos aleatorios con Coste_2 como instrumento
# Puede fallar por problemas de singularidad de la matriz
modelo_ea_re1 <- try(
  plm(Coste ~ Coste_1 + Produccion + PrCombust + Carga
      | Coste_2 + Produccion + PrCombust + Carga,
      data = pdatos2,
      model = "random")
)

# Si el modelo se estima correctamente, mostramos el resumen
if (!inherits(modelo_ea_re1, "try-error")) {
  summary(modelo_ea_re1)
}


# Modelo dinámico de efectos aleatorios con VI como instrumento
# También puede fallar por falta de variabilidad o multicolinealidad
modelo_ea_re2 <- try(
  plm(Coste ~ Coste_1 + Produccion + PrCombust + Carga
      | VI + Produccion + PrCombust + Carga,
      data = pdatos2,
      model = "random")
)

# Si el modelo se estima correctamente, mostramos el resumen
if (!inherits(modelo_ea_re2, "try-error")) {
  summary(modelo_ea_re2)
}


# CONCLUSIÓN

# Los contrastes descartan el modelo pooling
# El test de Hausman indica que efectos fijos es preferible a efectos aleatorios
# Los modelos dinámicos pierden observaciones y generan resultados menos robustos
# Por tanto, se selecciona el modelo estático de efectos fijos

