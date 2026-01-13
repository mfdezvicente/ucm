#########################################################################

### IC DE PERCENTILES 
# El obejtivo es crear la distribución del estadístico y ponerle unos cuantiles a derecha e izquierda

ICBP=function(muestra,estadistico,B=100,alpha=0.05){
estadisticob=numeric(B)
for(b in 1:B){
muestrab=sample(muestra,replace=TRUE)
estadisticob[b]=estadistico(muestrab)
}
IC=quantile(estadisticob,c(alpha/2,1-alpha/2))
return(list(IC=IC,est=estadisticob))
}


#########################################################################
### IC DE PERCENTILES DE ACELERADO

# Hay que sacar los valores de a y de z_0
# z_0 es muy fácil. Es la media de los valroes del estadisticob que están por debajo de estadistico(muestra)
# a tiene dos partes
### a_n : 

mJack=function(x,estadistico){
n=length(x)
Jack=matrix(NA,nrow=n,ncol=(n-1))
for(i in 1:n){
Jack[i,]=x[-i]
}
return(apply(Jack,1,estadistico))
}

ICBCa=function(muestra,estadistico,B=100,alpha=0.05){
estadisticob=numeric(B)
for(b in 1:B){
muestrab=sample(muestra,replace=TRUE)
estadisticob[b]=estadistico(muestrab)
}
#Calculo z0
z0=qnorm(mean(estadisticob<=estadistico(muestra)))
#Calculo a
theta_i=mJack(muestra,estadistico)
an=sum((mean(theta_i)-theta_i)^3)
ad=6*sum((mean(theta_i)-theta_i)^2)^(3/2)
a=an/ad
alpha1=pnorm(z0+(z0+qnorm(alpha/2))/(1-a*(z0+qnorm(alpha/2))))
alpha2=pnorm(z0+(z0+qnorm(1-alpha/2))/(1-a*(z0+qnorm(1-alpha/2))))
IC=quantile(estadisticob,c(alpha1,alpha2))
return(list(IC=IC,est=estadisticob))
}

ICt=function(muestra,estadistico,B=100,alpha=0.05,BB=50){
    estadisticob=numeric(B)
    se=numeric(B)
    for(b in 1:B){
        muestrab=sample(muestra,replace=TRUE)
        estadisticob[b]=estadistico(muestrab)
        estadisticobb=numeric(BB)
        for(bb in 1:BB){
            muestrabb=sample(muestrab,replace=TRUE)
            estadisticobb[bb]=estadistico(muestrabb)
        }
        se[b]=sd(estadisticobb)
    }
    Z=(estadisticob-estadistico(muestra))/se
    q=quantile(Z,c(alpha/2,1-alpha/2))
    return(c(estadistico(muestra)-q[1]*sd(estadisticob),estadistico(muestra)-q[2]*sd(estadisticob)))
}

#Ejercicio 4
set.seed(1)
MC=100
B=100
theta=1
n=20

IC.t=matrix(NA,nrow=MC,ncol=2)
IC.BP=matrix(NA,nrow=MC,ncol=2)
IC.BCa=matrix(NA,nrow=MC,ncol=2)

for(t in 1:MC){
muestra=rexp(n,1/theta)
IC.t[t,]=ICt(muestra,mean,B,alpha=0.05,BB=50)
IC.BP[t,]=ICBP(muestra,mean,B,alpha=0.05)$IC
IC.BCa[t,]=ICBCa(muestra,mean,B,alpha=0.05)$IC
}
