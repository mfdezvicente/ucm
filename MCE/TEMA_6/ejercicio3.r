#Ejercicio 3
class=c(137,138.5,140,141,142,143.5,145,147,148.5,150,153,154,155,156.5,157,158,158.5,159,160.5,161,162,167.5)
#length(class)
alpha=0.2

#IC BP para la media
B=100
mediab=numeric(B)
set.seed(1)
for(b in 1:B){
classb=sample(class,replace=TRUE)
mediab[b]=mean(classb)
}
hist(mediab)
#quantile(mediab,alpha/2)
#quantile(mediab,1-alpha/2)
IC=quantile(mediab,c(alpha/2,1-alpha/2))
#mean(class)
points(IC[1],0,col="red",pch=16)
points(IC[2],0,col="red",pch=16)
abline(v=IC[1],col="green")
abline(v=IC[2],col="green")

ICBP=function(muestra,estadistico,B=100,alpha=0.05){
estadisticob=numeric(B)
for(b in 1:B){
muestrab=sample(muestra,replace=TRUE)
estadisticob[b]=estadistico(muestrab)
}
IC=quantile(estadisticob,c(alpha/2,1-alpha/2))
return(list(IC,estadisticob))
}

set.seed(1)
Q=ICBP(class,mean,alpha=0.2)
Q[[1]]
Q[[2]]

ICBP=function(muestra,estadistico,B=100,alpha=0.05){
estadisticob=numeric(B)
for(b in 1:B){
muestrab=sample(muestra,replace=TRUE)
estadisticob[b]=estadistico(muestrab)
}
IC=quantile(estadisticob,c(alpha/2,1-alpha/2))
return(list(IC=IC,est=estadisticob))
}

set.seed(1)
Q=ICBP(class,mean,alpha=0.2)
Q$IC
Q$est

ICBP(class,median,alpha=0.2)$IC
ICBP(class,sd,alpha=0.2)$IC



##### SOLUCION DE BOOSTRAP-T


mJack=function(x,estadistico){              # PIDE LA MUESTRA Y EL ESTÁDISTICO
    n=length(x)                             
    Jack=matrix(NA,nrow=n,ncol=(n-1))       # MATRIZ CON LA MUESTRA POR FILAS 
    for(i in 1:n){
        Jack[i,]=x[-i] # LE QUITA UNO DE LOS COMPONENTES DE LA MATRIZ
    }
    return(apply(Jack,1,estadistico))
}

#Ejercicio 3
class=c(137,138.5,140,141,142,143.5,145,147,148.5,150,153,154,155,156.5,157,158,158.5,159,160.5,161,162,167.5)
#IC BCa
alpha=0.2

set.seed(1)
B=100
mediab=numeric(B)
for(b in 1:B){
classb=sample(class,replace=TRUE)
mediab[b]=mean(classb)
}
#Calculo z0
z0=qnorm(mean(mediab<=mean(class)))
#hist(mediab)
#points(mean(class),0,col="red",pch=16)

#Calculo a
theta_i=mJack(class,mean)
an=sum((mean(theta_i)-theta_i)^3)
ad=6*sum((mean(theta_i)-theta_i)^2)^(3/2)
a=an/ad
alpha1=pnorm(z0+(z0+qnorm(alpha/2))/(1-a*(z0+qnorm(alpha/2))))
alpha2=pnorm(z0+(z0+qnorm(1-alpha/2))/(1-a*(z0+qnorm(1-alpha/2))))
IC=quantile(mediab,c(alpha1,alpha2))
IC