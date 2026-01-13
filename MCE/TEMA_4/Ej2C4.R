#Ejercicio 2
datos=c(18.1,8.6,4.7,7.3,3.5,12.4,13.8,1.2)
n=length(datos)

B=100
mediat=numeric(B)

set.seed(1)
for(b in 1:B){
datosb=sample(datos,replace=TRUE)
mediat[b]=mean(datosb,trim=0.25)
}
hist(mediat)
mean(datos,trim=0.25)

#Damos una media del se vía bootstrap
sd(mediat)

#Damos una medida del sesgo bootstrap como estimador de la media poblacional
mean(mediat)-mean(datos)

##################################################
set.seed(1)
B=c(25,100,200,500,1000,2000)
sesgoB=numeric(length(B))
for(i in 1:length(B)){
mediat=numeric(B[i])
for(b in 1:B[i]){
datosb=sample(datos,replace=TRUE)
mediat[b]=mean(datosb,trim=0.25)
}
sesgoB[i]=mean(mediat)-mean(datos)
}
sesgoB