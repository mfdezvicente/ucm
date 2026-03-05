#Práctica 3.1 - María Belén Bilbao Pavón

#1. LECTURA DE DATOS
#install.packages("readxl")

library(readxl)

datos <- read_excel("C:/Users/maria/Desktop/QUINTO/3 ECONOMETRIA APLICADA/Práctica 3_1 Datos mrozv1.xls")

#2. Estimación del modelo de probabilidad lineal

modelo_mpl <- lm(inlf ~ kidslt6 + kidsge6 + age + educ + exper + nwifeinc + expersq, data=datos)

summary(modelo_mpl)

datos$inlfhat <- predict(modelo_mpl)

# A continuación ver los valores para fuera de rango 
fueran <- datos[datos$inlfhat>1| datos$inlfhat<0,]

#Estimación por metodos robustos para ver las t de antes porque el supuesto de normalidad no se cumple

#install.packages("sandwich")
#install.packages("lmtest")

library(sandwich)
library(lmtest)

coeftest(modelo_mpl,vcov=vcovHC(modelo_mpl,type="HC1"))

#Modelo logit

modelo_logit <-glm(inlf ~ kidslt6 + kidsge6 + age + educ + exper + nwifeinc + expersq, family= binomial(link="logit"), data=datos)
summary(modelo_logit)


#Para ver los efectos marginales
#install.packages("marginaleffects")
library(marginaleffects)

slopes(modelo_logit,newdata=datagrid(
  kidslt6=mean(datos$kidslt6),
  kidsge6=mean(datos$kidsge6),
  age=mean(datos$age),
  educ=mean(datos$educ),
  exper=mean(datos$exper),
  nwifein=mean(datos$nwifeinc), #ojo
  expersq=mean(datos$expersq)
  ))



#
nuevos_datos <- data.frame(
  kidslt6=c(0,1,2,3),
  age=c(mean(datos$age),mean(datos$age),mean(datos$age),mean(datos$age)),
  kidsge6=c(mean(datos$kidsge6),mean(datos$kidsge6),mean(datos$kidsge6),mean(datos$kidsge6)),
  age=c(age=mean(datos$age),age=mean(datos$age),age=mean(datos$age),age=mean(datos$age)),
  educ=c(mean(datos$educ),mean(datos$educ),mean(datos$educ),mean(datos$educ)),
  exper=c(mean(datos$exper),mean(datos$exper),mean(datos$exper),mean(datos$exper)),
  nwifeinc=c(mean(datos$nwifeinc),mean(datos$nwifeinc),mean(datos$nwifeinc),mean(datos$nwifeinc)),
  expersq=c(mean(datos$expersq),mean(datos$expersq),mean(datos$expersq),mean(datos$expersq))
)




predict_logit <- predict(modelo_logit,newdata=nuevos_datos,type="response")

#La porbabilidade de que trabaje tendiendo 0 hijos es 0.66 teniendo la media de otod CREO LUEGO CON2 1 HIJO
diff(predict_logit)

#Sí tiene valores fuera de 0 y 1 es el xi prima por los beta gorros 
datos$inlfhat_logit <- predict(modelo_logit,type="link") 
#No tieen valores fuera de rango es el pi gorro F aplicado a lo anteriori
datos$inlfhat_logit2 <- predict(modelo_logit,type="response")


#Ahora modelo_probit
modelo_probit <-glm(inlf ~ kidslt6 + kidsge6 + age + educ + exper + nwifeinc + expersq, family= binomial(link="probit"), data=datos)
summary(modelo_probit)


#Ahora la predicción
predict_probit <- predict(modelo_probit,newdata=nuevos_datos,type="response")

diff(predict_probit)

#Sí tiene valores fuera de 0 y 1 es el xi prima por los beta gorros 
datos$inlfhat_probit <- predict(modelo_probit,type="link") 
#No tieen valores fuera de rango es el pi gorro F aplicado a lo anteriori
datos$inlfhat_probit2 <- predict(modelo_probit,type="response")


#Umbral de calsificación 
datos$y_pre_log <-ifelse(datos$inlfhat_logit2 >= 0.5,1,0)

datos$y_pre_pro <-ifelse(datos$inlfhat_probit2 >= 0.5,1,0)

#Matriz de confusion
mconf <-table(Reak= datos$inlf,Predicho=datos$y_pre_log)
mconf

mconfpro <- table(Real= datos$inlf, Predicho=datos$y_pre_pro)
mconfpro

#lo que esta bien clasificado 