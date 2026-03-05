#PRÁCTICA 3.2 - María Belén Bilbao Pavón

# LECTURA DE LOS DATOS
datos <- read.table("C:/Users/maria/Desktop/QUINTO/3 ECONOMETRIA APLICADA/Práctica 3_2 Habitos_Tabaco.txt", header=TRUE,sep="\t")

# PARA ESTIMAR ESTE LOGIT MULTINOMIAL 


#Se le da este formato de factror en lugar de numérico
#install.packages("nnet")
library(nnet)

datos$FUMA <- factor(datos$FUMA)

modelo_logitmult <- multinom(FUMA ~ EDAD + SUENO + ALCOHOL + EJERTL,data=datos)
summary(modelo_logitmult)

#Para cambiar la categoria de referencia
datos$FUMAN <- relevel(datos$FUMA, ref="2")

modelo_logitmult2 <- multinom(FUMAN ~ EDAD + SUENO + ALCOHOL + EJERTL,data=datos)
summary(modelo_logitmult2)

# Contrastes de sinificación individual (ya que por defecto no aparecen)

res <- summary(modelo_logitmult)

# Estadístico de contraste 

z <- res$coefficients/res$standard.errors

p <- 2-(1-pnorm(abs(z))) #ojo no me sale

# mostrar resultados con cbind
cbind(Coef=res$coefficients, SE=res$standard.errors,z=z, p_value=p)

p

pred <- predict(modelo_logitmult, type="prob")
pred

#Así le asigna valores 0,1,2
pred <- predict(modelo_logitmult)
pred
#install.packages("caret")

#Probbilidad de acierto para validar el modelo sumando las de la diagonal 
library(caret)

confusionMatrix(as.factor(pred),as.factor(datos$FUMA))


#Para hacer una prediccion
nuevos_datos <- data.frame(EDAD=30, SUENO=7, ALCOHOL=0, EJERTL=2)

prediccion <- predict(modelo_logitmult,newdata=nuevos_datos,type="pro")
prediccion

prediccion <- predict(modelo_logitmult,newdata=nuevos_datos)
prediccion
