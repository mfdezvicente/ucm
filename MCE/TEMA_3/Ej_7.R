#Ejercicio 7
alpha=0.5
c1=1/alpha+exp(-1)

#help(rgamma)
#curve(dgamma(x,scale=1,shape=alpha),0,5)
curve(dgamma(x,scale=1,shape=alpha),xlim=c(0,5))

dy=function(x,c1,alpha){
(x^(alpha-1)/c1)*(0<x & x<1)+(exp(-x)/c1)*(x>=1)
}

c=c1/gamma(alpha)

#curve(dy(x,c1,alpha),add=TRUE,col="green")
curve(c*dy(x,c1,alpha),add=TRUE,col="pink")

ry=function(n,c1,alpha){
u=runif(n)
x=ifelse(u<1/(c1*alpha),(u*c1*alpha)^(1/alpha),-log(c1*(1-u)))
}

#BONDAD DE AJUSTE

set.seed(1)
contador=0
n=100
muestrax=c()
while(length(muestrax)<n){
u=runif(1)
y=ry(1,c1,alpha)
if((0<y & y<1 & u<=exp(-y))|(y>=1 & u<=y^(alpha-1))){muestrax=c(muestrax,y)}
contador=contador+1
}
