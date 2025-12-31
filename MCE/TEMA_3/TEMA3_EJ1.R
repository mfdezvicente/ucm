# EJERCICIO 1. TEMA 3
## APARTADO A
set.seed(1)
MC <- 10000
v<- numeric(MC)
v_1 <- runif(MC)
v_2 <- runif(MC)

for(i in 1:MC){
	v[i] = (v_1[i]+v_2[i])/2
}

print (v)
hist(v, col = "skyblue", main = "Función de densidad de la media de la V", freq=FALSE)
curve(dnorm(x, mean(v), sd(v)), add=TRUE, col="red")
ks.test(v,pnorm)
#####################################################
dtriang = function(x,a,b,c){
	2*(x-a)/((b-a)*(c-a))*(a<=x & x<c) + 2*(b-x)/((b-a)*(b-c))*(c<=x & x<=b)
}

hist(v, freq = FALSE)
curve(dtriang(x,min(v), max(v), mean(v)), add =TRUE, col ="red")


ptriang = function(x,a,b,c){
	(x-a)^2/((b-a)*(c-a))*(a<=x& x<=c) + (1-(b-x)^2/((b-a)*(b-c)))*(c<=x & x<b) +1*(x>=b)
}

hist(v, freq = FALSE)
curve(dtriang(x,min(v), max(v), mean(v)), add =TRUE, col ="red")

plot(ecdf(v))
curve(ptriang(x,min(v), max(v), mean(v)), add=TRUE, col="red")
ks.test(v, ptriang, min(v), max(v), mean(v))


## APARTADO B
set.seed(1)
MC <- 10000
V_aux <- matrix(runif(MC*10), ncol = MC, nrow=10)
V = apply(V_aux, 2, mean) #2 es por columnas 
length(V)


hist(V, col = "skyblue", main = "Función de densidad de la media de la V", freq=FALSE)
curve(dnorm(x, mean(V), sd(V)), add=TRUE, col="red")
ks.test(V,pnorm, mean(V), sd(V))


