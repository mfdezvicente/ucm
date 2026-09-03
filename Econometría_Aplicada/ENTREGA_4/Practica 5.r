#PRACTICA 5.1

#Tenemos un modelo de efectos fijos que se caracteriza por tener un número finito de niveles,
#suponemos que cada individuo tiene un efecto específico que no varía a lo largo del tiempo, y
#donde(ahora) queremos estudiar las variables que importan de modo que las betas son lo realmente 
#importante. 

#En este caso, el modelo se puede escribir como:Yit = αi +\beta xit + uit, es decir una beta 
#común para todos los individuos, y un intercepto específico para cada individuo (αi).

#El modelo de efectos aleatorios se caracteriza por tener un número infinito de niveles, donde
#lo que nos interesa es la varianza de los efectos aleatorios y no los niveles específicos. Se 
#puede escribir como: Yit = α +\beta xit + vit, donde vit = αi + uit, es decir, el efecto aleatorio 
#(αi) se considera como parte del término de error (vit).

#Por ultimo, esta el modelo pool que simplemente asume que no hay efectos individuales, es decir, 
#Yit = α +\beta xit + uit.

#El modelo pool se calcula de la manera tradicional, es decir, sin tener en cuenta la estructura de
#panel. El modelo de efectos fijos se estima utilizando la transformación de diferencias dentro
#(within transformation), que elimina los efectos individuales (αi) al restar la media de cada
# individuo. (RTD_it-media RTD_it)=beta(xit-media xit)+uit, lo que se conoce como estimador de covarianza.

#El modelo de efectos aleatorios se basa en que \sigma^2 = \sigma^2_u + \sigma^2_a, es decir,
#la varianza total del término de error (vit) es la suma de la varianza del error idiosincrático (uit) 
#y la varianza de los efectos aleatorios (αi). Esto provoca que el término de error vit esté 
#correlacionado con las variables explicativas (xit), lo que viola la suposición de homocedasticidad 
#por eso vamos a utilizar la estimación de mínimos cuadrados generalizados (GLS) para obtener estimadores 
#eficiente, aunque en 2 etapas. Primero, se estima el modelo de efectos aleatorios utilizando la 
#transformación de Swamy-Arora, que ajusta los datos para tener en cuenta la varianza de los efectos 
#aleatorios. Luego, se utiliza esta transformación para estimar el modelo de efectos aleatorios 
#utilizando GLS. Otra opcion es MV, pero los supuestos de este modelo son más restrictivos que los del 
#modelo de efectos aleatorios, ya que se asume que los errores son independientes e idénticamente 
#distribuidos (i.i.d.) y que no hay correlación entre los errores y las variables explicativas.

library(readxl)

datos <- read_excel("C:/Users/Usuario/GitHub/ucm/Econometría_Aplicada/ENTREGA_4/Endeudamiento.xls")

modelos_mco <- lm(RTD ~ TAM + ESTA + ROA  + CRE + ln_EDAD, data = datos)

summary(modelos_mco)

install.packages("plm")
library(plm)

#Hacemos la transformación de panel utilizando la función pdata.frame() del paquete plm, que nos permite
#especificar la estructura de panel de nuestros datos. En este caso, tenemos una variable "Empresa" que identifica
# a cada individuo y una variable "AÑO" que identifica el tiempo. Al convertir nuestros datos en un objeto de panel,
# podemos aplicar los modelos de efectos fijos y aleatorios correctamente.

pdata<- pdata.frame(datos, index=c("Empresa", "AÑO"))
View(pdata)
pdim(pdata)

#Empezamos por el modelo pool que es igual que el que ya hemos hecho, aunque usando la biblioteca plm, que es mas elegante

modelos_pool <- plm(RTD ~ TAM + ESTA + ROA  + CRE + ln_EDAD, data = pdata, model = "pooling")
summary(modelos_pool)

#Para poder tener una intuicion del modelo que va a ser mejor de manera grafica, podemos hacer un grafico de cada empresa
#a lo largo del tiempo, para ver si hay una tendencia clara o si cada empresa tiene un comportamiento diferente. Pero esto 
#es algo poco operativo por lo que lo hacemos con una submuestra

install.packages("dplyr")
library(dplyr)

set.seed(123)

muestra<- datos%>%
    filter(Empresa %in% sample(unique(Empresa), 10))

View(muestra)

library(ggplot2)

ggplot(muestra, aes(x = AÑO, y = RTD, group= Empresa, color = as.factor(Empresa))) +
    geom_line() +
    geom_point() +
    labs(color = "Empresa") 

#Viendo el grafico parece que hay efectos fijos ya que parece que cada empresa tiene niveles distintos.

#Modelo de efectos fijos

modelo_ef<- plm(RTD ~ TAM + ESTA + ROA  + CRE + ln_EDAD, data = pdata, model = "within")
summary(modelo_ef)

#Modelo de efectos aleatorios

modelo_ea<- plm(RTD ~ TAM + ESTA + ROA  + CRE + ln_EDAD, data = pdata, model = "random")
summary(modelo_ea)

#Vemos que los tres modelo tienen los mismos signos siendo todas significativas en las tres, ahora veamos con cual de ellos
#nos quedamos, para esto vamos a hacer el test de Hausman, que nos permite comparar los modelos de efectos fijos y aleatorios.

#Si queremos comparar pool con efectos fijos, hacemos un test donde H_0 : α1= α2=...=α1204, es decir, los efectos inviduales
#son iguales, por lo que el modelo pool es el adecuado, mientras que H_1 : αi ≠ αj para al menos un par de individuos, es decir,
#hay efectos individuales y el modelo de efectos fijos es el adecuado.

pFtest(modelo_ef, modelos_pool)

#el p-valor es menor que 0.05, por lo que rechazamos la hipótesis nula y concluimos que hay efectos individuales y el modelo de 
#efectos fijos es mas adecuado.

#Para comparar efectos fijos con efectos aleatorios, hacemos un test donde H_0 : Cov (αi, xit) = 0, es decir, los efectos individuales 
#no están correlacionados con las variables explicativas, por lo que el modelo de efectos aleatorios es el adecuado, mientras
# que H_1 : Cov (αi, xit) ≠ 0, es decir, los efectos individuales están correlacionados con las variables explicativas, por lo que 
#el modelo de efectos fijos es el adecuado. 

phtest(modelo_ef, modelo_ea)

#Como el p-valor es menor que 0.05, rechazamos la hipótesis nula y concluimos que los efectos individuales están correlacionados
#con las variables explicativas, por lo que el modelo de efectos fijos es el adecuado. 

#Para comparar pool con efectos aleatorios, hacemos un test donde H_0 : Var(αi) = 0, es decir, los efectos individuales no tienen 
#varianza, por lo que el modelo pool es el adecuado, mientras que H_1 : Var(αi) > 0, es decir, los efectos individuales tienen 
#varianza, por lo que el modelo de efectos aleatorios es el adecuado.

plmtest(modelo_ea, type = "bp")

#PRÁCTICA 5.2

#Vamos a intentar analizar si los sindicatos tienen exito en las negociaciones salariales para lo que haremos el siguiente modelo:
#lwage = β0 + β1 black + β2 exper + β3 married + β4 educ +β5 union + β6expersq + uit, y ver si union es significativo, es decir, 
#si los sindicatos tienen un efecto positivo en los salarios.

datos <- read_excel("C:/Users/Usuario/GitHub/ucm/Econometría_Aplicada/ENTREGA_4/Práctica 5_2_Datos.xlsx")

pdata <- pdata.frame(datos, index=c("nr", "year"))

pdim(pdata)

hist(pdata$hisp)

modelo_pull <- plm(lwage ~ exper + expersq + married + educ + black + union, data = pdata, model = "pooling")

summary(modelo_pull)

#Si nosotros tomamos este modelo vemos que el termino que acomapañaa union es significativo y 0,181, lo que nos indiuca que estar
#afiliado a un sindicato aumenta el salario en un 18,1%. La constante no es significativa pero, como solo es un parámetro el que nos 
#sale no significativo, no le damos mucha importancia. El resto de variables también son significativas y tienen el signo esperado. 
#Si quitaramos esa constante tendriamos el problema de que perderíamos que la media de los residuos fuera cero al obligar a la regresión
#a pasar por el origen, lo que podría sesgar nuestros resultados.

modelo_ef <- plm(lwage ~ exper + expersq + married + educ + black + union, data = pdata, model = "within")

summary(modelo_ef)

#Vemos que tanto educ como black no salen en el modelo porque son constantes en el tiempo por lo tanto al restar a cada invdividuo
#su media temporal, estas variables desaparecen. El resto de variables siguen siendo significativas y con el mismo signo que antes,
#aunque el coeficiente de union ha cambiado ligeramente, reduciendose a 0,08208, lo que nos indica que el efecto de estar afiliado
#a un sindicato en el salario es menor cuando controlamos por los efectos individuales. Si se cumplen los presupuestos de efecro 
#aleatorios podemos ver ahí los efectos de black y educ.

#Para comparar ambos modelos, hacemos el test de comparar los alfas con H_0 : α1= α2=...=α1204, es decir, los efectos inviduales
#son iguales, por lo que el modelo pool es el adecuado, mientras que H_1 : αi ≠ αj para al menos un par de individuos, es decir,
#hay efectos individuales y el modelo de efectos fijos es el adecuado.

pFtest(modelo_ef, modelo_pull)

#Como el p-valor es menor que 0.05, rechazamos la hipótesis nula y concluimos que hay efectos individuales y el modelo de efectos
#fijos es mas adecuado.

modelo_ea <- plm(lwage ~ exper + expersq + married + educ + black + union, data = pdata, model = "random")
summary(modelo_ea)

#Este modelo tiene heterocedasticidad porque los errores están correlados y por lo tanto se calcula con minimos cuadrados
#generalizados en dos etapas, quedando un coeficiente de 0.10762 para union, lo que nos indica que el efecto de estar 
#afiliado a un sindicato en el salario es del 10,762% cuando controlamos por los efectos individuales

#Para comparar Pull y efectos aleatorios hacemos un test de heterocedasticidad con H_0 : Var(αi) = 0, es decir, los efectos
#individuales no tienen varianza, por lo que el modelo pool es el adecuado, mientras que H_1 : Var(αi) > 0, es decir, los efectos
#individuales tienen varianza, por lo que el modelo de efectos aleatorios es el adecuado.

plmtest(modelo_ea, type = "bp")

#Como el p-valor es menor que 0.05, rechazamos la hipótesis nula y concluimos que los efectos individuales tienen varianza
#por lo que el modelo de efectos aleatorios es el adecuado.

#Para comparar efectos fijos con efectos aleatorios, hacemos un test donde H_0 : Cov (αi, xit) = 0, es decir, los efectos 
#individuales no están correlacionados con las variables explicativas, por lo que el modelo de efectos aleatorios es el 
#adecuado, mientras que H_1 : Cov (αi, xit) ≠ 0, es decir, los efectos individuales están correlacionados con las variables 
#explicativas, por lo que el modelo de efectos fijos es el adecuado.

phtest(modelo_ef, modelo_ea)

#Como el p-valor es menor que 0.05, rechazamos la hipótesis nula y concluimos que los efectos individuales están correlacionados
#con las variables explicativas, por lo que el modelo de efectos fijos es el adecuado

#Por tanto el estar afiliado a un sindicato aumenta el salario en un 8,208% que era el termino asociado a ese modelo.

#PRACTICA 5.4

#Vamos a analizar como afecta la variación de capital a la inversion de una empresa. Para ello usaremos los modelos vistos

#El modelo pull es inv_it = β0 + β1 value + β2 capital + uit, donde nos interesa es β2.

data <- read_excel("C:/Users/Usuario/GitHub/ucm/Econometría_Aplicada/ENTREGA_4/Grunfeld.xlsx")

pdata <- pdata.frame(data, index=c("firm", "year"))

pdim(pdata)


#Vemos como es un panel no balanceado lo que quiere decir que no hay los mismos datos para todas las empresas en todos los años,
#y lo cierto es que los modelos que usamos no son validos pero bueno los veremos aun así.

modelo_pull <- plm(inv ~ value + capital, data = pdata, model = "pooling")
summary(modelo_pull)

#Analizamos tambien el modelo de efectos fijos 

modelo_ef <- plm(inv ~ value + capital, data = pdata, model = "within")
summary(modelo_ef)

#Comparamos ambos modelos con el test de efectos individuales, con H_0 : α1= α2=...=α1204, es decir, los efectos inviduales
#son iguales, por lo que el modelo pool es el adecuado, mientras que H_1 : αi ≠ αj para al menos un par de individuos, es decir,
#hay efectos individuales y el modelo de efectos fijos es el adecuado.

pFtest(modelo_ef, modelo_pull)

#Como el p-valor es menor que 0.05, rechazamos la hipótesis nula y concluimos que hay efectos individuales y el modelo de efectos
#fijos es mas adecuado.

#Calculamos el modelo de efectos aleatorios

modelo_ea <- plm(inv ~ value + capital, data = pdata, model = "random")
summary(modelo_ea)

#Para comparar Pull y efectos aleatorios hacemos un test de heterocedasticidad con H_0 : Var(αi) = 0, es decir, los efectos
#individuales no tienen varianza, por lo que el modelo pool es el adecuado, mientras

plmtest(modelo_ea, type = "bp")

#Y ahora para comparar efectos fijos con efectos aleatorios, hacemos un test donde H_0 : Cov (αi, xit) = 0,

phtest(modelo_ef, modelo_ea)

#Como no se rechaza H0 mantenemos que los efectos individuales no están correlacionados con las variables explicativas,
#por lo que el modelo de efectos aleatorios es el adecuado.


#Practica 5.3

data3 <- read.csv("C:/Users/Usuario/GitHub/ucm/Econometría_Aplicada/ENTREGA_4/Volatilidad.csv", sep = ';', dec = ',')

pdata3 <- pdata.frame(data3, index=c("ID", "time"))

pdim(pdata3)

modelo_pull3 <- plm(sd_gdp ~ deposits_gdp + tradeopenness + log_gdp_pc, data = pdata3, model = "pooling")
summary(modelo_pull3)

modelo_ef3 <- plm(sd_gdp ~ deposits_gdp + tradeopenness + log_gdp_pc, data = pdata3, model = "within")
summary(modelo_ef3)

pFtest(modelo_ef3, modelo_pull3)

modelo_ea3 <- plm(sd_gdp ~ deposits_gdp + tradeopenness + log_gdp_pc, data = pdata3, model = "random")
summary(modelo_ea3)

plmtest(modelo_ea3, type = "bp")

phtest(modelo_ef3, modelo_ea3)

#Aqui el phtest no es del todo claro asi que vemos la interpretacion de tradeopeness, 