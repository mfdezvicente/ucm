#FUNCION DE DENSIDAD DE LA TRIANGULAR DEL EJERCICIO 2.4
dtrianng = function(x) { 
    x *(0<x & x<=1) + (2-x) * (1<x & x<2)

}
p = 3/4

fdensidadt = function(x) { 
    4/3 * dtrianng(x) *(1/2<x & x<3/2)
}

Fdistribuciont = function(x) { 
    (2/3 * x^2-1/6) * (1/2<x & x<=1) + (-2/3 * x^2 + 8/3 * x - 2) * (1<x & x<3/2) + 1 * (x>=3/2)
}
# GENERO 100 VALORES DE UNA TRIANGULAR TRUNCADA
n = 100
muestrat = c()
set.seed(1)
while(length(muestrat) < n) {
    u = runif(1)
    x = ifelse(u < 1/2, sqrt(2*u), 2 - sqrt(2*(1-u)))
    if (1/2 < x & x < 3/2) {
        muestrat = c(muestrat, x)   
    }
}
print(muestrat)

# VEMOS SI ESTA MUESTRA SE AJUSTA A UNA TRIANGULAR TRUNCADA
par(mfrow=c(1,2))
hist(muestrat, freq = FALSE, main = "Histograma de variable triangular truncada")
curve(fdensidadt, add=TRUE, col="red", lwd=2)

plot(ecdf(muestrat), main = "Función de distribución acumulada empírica vs Teórica")
curve(Fdistribuciont, add=TRUE, col="red", lwd=2)

print(ks.test(muestrat, Fdistribuciont)) # ptriang es la función de distribución acumulada de la triangular




