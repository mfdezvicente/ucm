# EJEMPLO DE TEST BOOTSTRAP PARA LA IGUALDAD DE MEDIAS
# H0; mux = muy

#ELEGIMOS EL ESTADISTICO mean(x) - mean(y)
B=100
set.seed(1234)
m = 15
n = 32

x = rnorm(m, mean=0, sd=1)
y = rnorm(n, mean=-1, sd=1)

# DISEÑAMOS EL TEST BOOTSTRAP
tobs = mean(x) - mean(y)
tobs

# APROXIMO LA DISTRIBUCIÓN NULA
# NECESITO ESTAR BAJO H0
# TRANSFORMO X, Y ORIGINALES PARA QUE TENGAN LA MISMA MEDIA
z = c(x, y)
nuevox = x - mean(x) + mean(z)
nuevoy = y - mean(y) + mean(z)
T = numeric(B)
for (b in 1:B){
  bootx = sample(nuevox, size=m, replace=TRUE)
  booty = sample(nuevoy, size=n, replace=TRUE)
  T[b] = mean(bootx) - mean(booty)
}
hist (T, main="Distribución nula del estadístico", xlab="T")
# CALCULO EL P-VALOR
pvalor = mean(abs(T) >= abs(tobs))
alpha = 0.05
q = quantile (T, probs=c(alpha/2, 1-alpha/2))
abline (v=q, col="blue", lty=2)




hist(T, main="Distribución nula del estadístico", xlab="T")
points (tobs, 0, col="red", pch=19, cex=2)
mean(abs(T) >= abs(tobs))
alpha = 0.05
q = quantile (T, probs=c(alpha/2, 1-alpha/2))
abline (v=q[1], col="blue", lty=2)
abline (v=q[2], col="blue", lty=2)
# FIN DEL EJE