# DENSIDAD 
kernel.densidad = function(datos, puntos, h, kernel){
    ndatos = length(datos)
    npuntos = length(puntos)
    matk = kernel((puntos%*%t(rep(1,ndatos)) -t(datos%*%t(rep(1,npuntos)))/h))
    as.vector((matk%*%rep(1,ndatos))/(ndatos*h))
}

set.seed(1)
n = 20
muestra = rnorm(n)
kernelg = function(x){
    dnorm(x)
}

h = 0.25
puntos = seq (-3,3,length = 1000)

f_est = kernel.densidad(muestra, puntos, h, kernelg)

#plot(puntos, f_est, type = "l")

hist (muestra, freq = FALSE)
lines(puntos, f_est)

f_teorica = dnorm(puntos)
plot(puntos, f_teorica, type = "l", col = "red", ylim = c(0,0.5))
lines(puntos, f_est)


###########################

#distribución
kernel.distribucion = function(datos, puntos, h){
    ndatos = length(datos)
    npuntos = length(puntos)
    matk = Kerneldf((puntos%*%t(rep(1, ndatos)) -t(datos%*%t(rep(1,npuntos)))/h))
    as.vector((matk%*%rep(1,ndatos))/ndatos)
}

set.seed(1)
n = 20
muestra = rnorm(x)
Kerneldfg = function(x){
    pnorm(x)
}
h = 1.06^sd(muestra)*n^(-1/5)
puntos = seq(-3,3,length = 1000)

F_est = kernel.distribucion(muestra, puntos, h, Kerneldfg)
plot(puntos, F_est, type = "l")

plot(ecdf(muestra))
lines(puntos, F_est)

F_teorica = pnorm(puntos)
plot(puntos, F_teorica, type = "l", col = "red",)
lines(puntos, F_est)


# Error cuadrático medio 
# Quiero calcularlo mediante simulación
kernel.distribucion = function(datos, puntos, h){
    ndatos = length(datos)
    npuntos = length(puntos)
    matk = kernel((puntos%*%t(rep(1, ndatos)) -t(datos%*%t(rep(1,npuntos)))/h))
    as.vector((matk%*%rep(1,ndatos))/ndatos)
}


kenelg = function(x){
    dnorm(x)
}
puntos = seq(-6,6,length = 100)
f_teorica = dnorm(puntos)

set.seed(1)
n = 20
MC =100
error = matrix(NA, ncol=length(puntos), nrow = 100)
for (t in 1:MC){
    muestra = rnorm(x)
    h = 1.06^sd(muestra)*n^(-1/5)
    f_est = kernel.densidad(muestra, puntos, h, kernelg)
    error[t,] = (f_est - f_teorica)^2
}
MSE = apply(error, 2, mean)
plot(puntos, MSE, type = "l", main = "MSE estimador de densidad kernel", ylab = "MSE", xlab = "x")
