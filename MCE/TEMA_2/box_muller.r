#USAMOS EL MÉTODO DE B-M PARA OBTENER N VALORES DE N(0,1)
n = 1000
set.seed(1)
U1 = runif(n)
U2 = runif(n)
x = sqrt(-2*log(U2))*cos(2*pi*U1)
y = sqrt(-2*log(U2))*sin(2*pi*U1)


hist(x, prob=TRUE, main="Histograma de la muestra")
curve(dnorm(x), col="red", add=TRUE)
ks.test(x, "pnorm")

hist(y, prob=TRUE, main="Histograma de la muestra")
curve(dnorm(x), col="red", add=TRUE)
ks.test(y, "pnorm")

z = c(x[1:500], y[1:500])
curve(dnorm(x), col="red", add=TRUE)
ks.test(z, "pnorm")
