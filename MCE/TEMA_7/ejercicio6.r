m = 8
n = 11
set.seed(1)
x = rnorm(m, 15, sqrt(0.5))
y = rnorm (n, 0, sqrt(0.5))

# CALCULO EL TOBS 
tobs = mean(x) - mean(y) -15
z = c(x,y)
nuevox = x - mean(x) + mean (z) + 7.5
nuevoy = y - mean(y) + mean(z) - 7.5

B = 100
T = numeric(B)
for (b in 1:B){
    xb = sample(nuevox, replace = TRUE)
    yb = sample(nuevoy, replace = TRUE)
    T[b] = mean(xb) - mean (yb) -15
}
hist (T)
points  (tobs, 0,  col = "red", pch =16)

mean (abs(T) > abs (tobs))
