# ESTE TEST ES PARA ALBSOLUTAMENTE CONTINUAS. SI PUEDO USARLO LO USARÉ
# VAMOS A HACER USO DEL CONTRASTE DE KOLMOGOROV-SMIRNOV PARA COMPROBAR SI UNA MUESTRA SIGUE UNA DISTRIBUCIÓN UNIFORME
# PARA ELLO, VAMOS A GENERAR UNA MUESTRA DE NÚMEROS
set.seed(1)
num.al = runif(100)
res = ks.test(num.al, "punif", 0, 1) #Uso punif porque es la función de distribución acumulada de la uniforme

str(res)
print(res$p.value)

# AHORA VAMOS A HACERLO DE MANERA GRÁFICA

## DIBUJAMOS LA FUNCIÓN DE DISTRIBUCIÓN ACUMULADA EMPÍRICA
emp = ecdf(num.al)
plot(emp, main="Función de distribución acumulada empírica vs Teoríca U(0,1)", xlab="x", ylab="F(x)")
points(num.al, emp(num.al), col="green", pch=19)
#abline(0,1, col="red") # Dibuja la función de distribución acumulada teórica
curve(punif(x,0,1), col="red", add=TRUE) # Dibuja la función de distribución acumulada teórica


# PARA LA NORMAL VAMOS A HACERLO CON LA FUNCIÓN rnorm
set.seed(1)
num.al2 = rnorm(10, mean=0, sd=1)
emp = ecdf(num.al2)
plot(emp, main="Función de distribución acumulada empírica vs Teoríca
        N(0,1)", xlab="x", ylab="F(x)")
curve(pnorm(x,0,1), col="red", add=TRUE) # Dibuja la función de distribución acumulada teórica

print(ks.test(num.al2, pnorm)) #Uso pnorm porque es la función de distribución acumulada de la normal
print(ks.test(num.al2, punif)) #Uso punif porque es la función de distribución acumulada de la uniforme

