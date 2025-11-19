# EJEMPLO
muestra = c(94, 38, 23, 197, 99, 16, 141)

# CALCULO TOBS
# EL ESTADISTICO ES sqrt(n) * (mean - 129) / sd

n = length(muestra)
tobs = sqrt(n) * (mean(muestra) - 129) / sd(muestra)
tobs

# APROXIMO LA DISTRIBUCIÓN NULA
# LO PRIMERO ES GENERAR MUESTRAS BAJO H0
# LA MEDIA BAJO H0 ES 129
B = 1000
set.seed(1234)
T = numeric(B)
nmuestra = muestra - mean(muestra) + 129
#abs(mean(nmuestra) - 129) < 1e-10

for (b in 1:B){
  bootsample = sample(nmuestra, size=n, replace=TRUE)
  T[b] = sqrt(n) * (mean(bootsample) - 129) / sd(bootsample)
}
hist (T, main="Distribución nula del estadístico", xlab="T")
points( tobs, 0, col="red", pch=19, cex=2)

#H1: la media es distinta de 129
mean(abs(T) >= abs(tobs))
pvalor = mean(abs(T) >= abs(tobs))
pvalor
alpha = 0.05
q = quantile (T, probs=c(alpha/2, 1-alpha/2))
abline (v=q[1], col="blue", lty=2)
abline (v=q[2], col="red", lty=2)

# H1: la media es menor que 129
mean(T <= tobs)

