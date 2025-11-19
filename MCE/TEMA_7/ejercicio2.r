# EJERCICIO 2
# REPITE EL EJERCICIO PLANTEADO EN EL EJ 1 UN TOTAL DE MC = 100, GUARDANDDO EL P-VALOR OBTENIDO EN CADA SIMULACIÓN.
# AL FINAL, CALCULA LA PROPORCIÓN DE VECES QUE SE RECHAZA LA HIPÓTESIS NULA AL NIVEL DE SIGNIFICACIÓN 5%.
MC = 100
pvalores = numeric(MC)
set.seed(1234)
for (i in 1:MC){
  x = rnorm(7, mean=129, sd=66.77)
  # CALCULO TOBS
  # EL ESTADISTICO ES sqrt(n) * (mean - 129) / sd
  n = 7
  tobs = mean(x) - 129
  B=1000
  T = numeric(B)
  for (b in 1:B){
    bootsample = sample(x, size=n, replace=TRUE)
    T[b] = mean(bootsample) - 129
  }
  pvalores[i] = mean(abs(T) >= abs(tobs))
}
pvalores
mean(pvalores < 0.05)   # PROPORCIÓN DE VECES QUE SE RECHAZA H0 AL NIVEL 5%

# BAJO EL APARTADO B
MC = 100
pvalores = numeric(MC)
set.seed(1234)
for (i in 1:MC){
  x = rnorm(7, mean=129, sd=66.77)
  # CALCULO TOBS
  # EL ESTADISTICO ES sqrt(n) * (mean - 129) / sd
  n = 7
  tobs = mean(x) - 129
  B=1000
  T = numeric(B)
  nuevox = x - mean(x) + 129
  for (b in 1:B){
    bootsample = sample(nuevox, size=n, replace=TRUE)
    T[b] = mean(bootsample) - 129
  }
  pvalores[i] = mean(abs(T) >= abs(tobs))
}
pvalores
mean(pvalores < 0.05)   # PROPORCIÓN DE VECES QUE SE RECHAZA H0 AL NIVEL 5%     

# SI QUIERO ESTIMAR LA POTENCIA
MC = 100
pvalores = numeric(MC)
set.seed(1234)
for (i in 1:MC){
    x = rnorm(7, mean=170, sd=66.77)
    # CALCULO TOBS
    # EL ESTADISTICO ES sqrt(n) * (mean - 129) / sd
    n = 7
    tobs = mean(x) - 129
    B=1000
    T = numeric(B)
    nuevox = x - mean(x) + 129
    for (b in 1:B){
      bootsample = sample(nuevox, size=n, replace=TRUE)
      T[b] = mean(bootsample) - 129
    }
    pvalores[i] = mean(abs(T) >= abs(tobs))
}
pvalores
mean(pvalores < 0.05)   # PROPORCIÓN DE VECES QUE SE RECHAZA H0 AL NIVEL 5%
# ESTA SERÍA LA POTENCIA DEL TEST PARA UNA MEDIA REAL DE 135

