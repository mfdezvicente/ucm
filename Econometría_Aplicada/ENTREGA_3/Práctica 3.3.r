rm (list = ls())
library(readxl)
datos <- read_excel("/Users/miguelfernandez/GitHub/ucm/Econometría_Aplicada/ENTREGA_3/PrÃ¡ctica 3_3 Datos apple.xls")
datos

# Especificación 
# Nuestro objetivo es saber cuales son las variables que hacen variar la cantidad de las manzanas ecologicas 
# Para ello usaremos como variables regresoras: ecoprc, regprc, hhsize (tamaño del hogar esperando que está pueda influir en el número de manzadas que se consumen,


#Estimación
modelo_inicial <- lm(ecolbs ~ ecoprc + regprc + faminc + hhsize,data=datos)
summary(modelo_inicial)

# Vemos que la variable de ingresos no es significativa. Tampoco hace falta que no lo eliminemos porque hay pocas varibales yentonces nohace que aumente mucho la varianza. 
# Tenemos que mirar los signos. 
# Parece que al auemntar el precio, aumenta la demanda. Al auamentr el preico de las oras manzas aumenta el preicio de las manzandas que ono son ecologicas. 

# Deberíamos de hacer un análisis exploratorio de datos. 
# Deberíamos de hacer unas mínimas tablas estadísticas para ver por donde van los tiros 

table(datos$ecolbs)
hist(datos$ecolbs)

# Vemos que nos encontramos con una variable del tipo tobit porque es el maximo entre 0 y la y observada. eEsto lo vemos porque la muestra qse acumula muchos valroes en el cero. 
# Vamos a estiminar entonces mediante tobit
library(AER)
modelo_tobit <- tobit(ecolbs ~ ecoprc + regprc + faminc + hhsize, left = 0, data=datos)
summary(modelo_tobit)

# Vemos que los signos se mantienen. Junto con la parte de las significaciones. 
# Además podemos decir de esta salida que: 
# Un supuesto clave es que el término de error en el modelo tobit es normal con media cero y varianza sigma cuadrado.. Esa sigma cuadrado es desconocida y su estiamción es scale.
# Hay que tener cuidado con el modelo tobit porque es por MV, entonces si no es malo. 
# El modelo es globalmente significativo.
# Realmente podemos decir que aunque esa es la varibale latente, el mayor interés estaría en los que son positivos. 
# En MCO los efectos osn constantes para todfos los indiviudos ede la mnuestra
# En caso de tobit para cada indivudio va a ser difrente por el efecto marginal. 
X <- model.matrix(modelo_tobit)
beta <- coef(modelo_tobit)
sigma <- modelo_tobit$scale


xb <- as.vector(X %*% beta)
phi_i <- pnorm(xb / sigma)
me_ecoprc_i <- phi_i * beta["ecoprc"]
me_regprc_i <- phi_i * beta["regprc"]

datos$me_ecoprc <- phi_i * beta["ecoprc"]
datos$me_regprc <- phi_i * beta["regprc"]
datos$me_faminc <- phi_i * beta["faminc"]
datos$me_hhsize <- phi_i * beta["hhsize"]

# Efecto marginal medio 
mean(datos$me_ecoprc)
mean(datos$me_regprc)
mean(datos$me_faminc)
mean(datos$me_hhsize)

newdata <-  data.frame(ecoprc = mean(datos$ecoprc), regprc = mean(datos$regprc), faminc = mean(datos$faminc), hhsize=  mean(datos$hhsize) )
newdata

xb_medio <- predict(modelo_tobit, newdata = newdata, type = "link")
xb_medio
phi_medio <- pnorm(xb_medio/sigma)
phi_medio

marginales_medio <- phi_medio * beta
marginales_medio 
beta <- coef(modelo_inicial)
beta

