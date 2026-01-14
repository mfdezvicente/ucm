# EJERCIO 5 
m = 8 
n = 11

set.seed(1)
x = rnorm(m, 0, sqrt(0.5))
y = rnorm(n, 2, sqrt(0.5))

# CALULO EL TOBS
tobs = var(x) -var(y)
tobs

# APROXIMO LA DISTRIVUCIÓN DE T (EL ESTADÍSTICO) BAJO H0

z =c(x,y)
nuevox = x/sd(x) * sd(z)
nuevoy = y/sd(y) *sd(z)
abs(var(nuevox) - var(nuevoy)) < 10e-12
B = 100

T = numeric(B)
for (b in 1:B){
    xb = sample(nuevox, replace = TRUE)
    yb = sample(nuevoy, replace=TRUE)
    T[b] = var(xb) - var(yb)
}
hist[T]
#points = (tobs, 0, col = "red", pch = 16)
# CALULAMOS EL P-VALOR SI H1 VARIANZAS DISTINTAS
mean(abs(T) > abs(tobs))

# CALULAMOS EL P VALOR SI H1: SIGMA**2_X > SIGMA**2_Y
mean(T < tobs)
#}
pval [t] = mean(abs(T) >= abs(tobs)
)

T[b] = var(xb) - var(yb)
#}
pval[t] = mean(abs(T) >= abs(tobs))
#}
mean(pval < 0.05)
