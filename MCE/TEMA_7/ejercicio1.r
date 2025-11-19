# MUESTRA DE X DE TAMAÑO 7 CON MEDIA 129 Y SD 66.77. Y LLEVA A CABO UN TEST DE HIPÓTESIS PARA LA MEDIA 129 USANDO X_BARRA  
# -129 Y TOMANDO COMO DISTRIBUCIÓN NULA ESTIMADA: 

# A) LA DISTRIBUCIÓN EMPÍRICA DE LA MUESTRA (BOOTSTRAP SIMPLE)
# B) LA DISTRIBUCIÓN EMPÍRICA TRASLADADA PARA QUE TENGA MEDIA 129 (BOOTSTRAP TRASLADADO)

# APARTADO A
x = rnorm(7, mean=129, sd=66.77)
# CALCULO TOBS
# EL ESTADISTICO ES sqrt(n) * (mean - 129) / sd
n = 7
tobs = mean(x) - 129
tobs
B=1000
set.seed(1234)
T = numeric(B)

for (b in 1:B){
  bootsample = sample(x, size=n, replace=TRUE)
  T[b] = mean(bootsample) - 129
}
hist (T, main="Distribución nula del estadístico", xlab="T")
# H1 : MUY DISTINTA DE 129
mean(abs(T) >= abs(tobs))

points( tobs, 0, col="red", pch=19, cex=2)

# b)

x = rnorm(7, mean=129, sd=66.77)
# CALCULO TOBS
# EL ESTADISTICO ES sqrt(n) * (mean - 129) / sd
n = 7
tobs = mean(x) - 129
tobs
B=1000
set.seed(1234)
T = numeric(B)
nuevox = x - mean(x) + 129
for (b in 1:B){
  bootsample = sample(nuevox, size=n, replace=TRUE)
  T[b] = mean(bootsample) - 129
}
hist (T, main="Distribución nula del estadístico", xlab="T")
points( tobs, 0, col="red", pch=19, cex=2)
# H1: MUY DISTINTA DE 129
mean(abs(T) >= abs(tobs))

