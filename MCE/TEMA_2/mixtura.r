#GENERAMOS N VALORES DE LA MIXTURA EJ 20

n = 100
p = 0.9
set.seed(1)
u1 = runif(n)
u2 = runif(n)

x = ifelse(u2 < 1/2, sqrt(u2/2), 1 - sqrt((1-u2)/2))
muestrax = ifelse(u1 < p, runif(n), x)
print(muestrax)


# COMPROBAR QUE X SIGUE f2
hist(muestrax, prob=TRUE, main="Histograma de la muestra")
curve(0.9*dunif(x) + 0.1* (1/ (2*sqrt(2*x)) + 1/(2*sqrt(2*(1-x)))), col="red", add=TRUE)
ks.test(muestrax, "punif")

# COMPROBAR QUE LA MUESTRAX SIGUE f 