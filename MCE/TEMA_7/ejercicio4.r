# APROXIMAMOS LA POTERNCIA DEL TEST

n= 10
set.seed(1)
MC=100 
pval = numeric(MC)
x = rnorm(n, mean= 0, sd =2.2)
tobs = var(x) - 0.5
xneuvo = x*sqrt(0.5)/sd(x)
B=100
T = numeric(B)
for (b in 1:B){
    xb = sample(xnuevo, replace = TRUE)
    T[b] = var (xb) - 0.5
}
pval[b] = mean (abs(T) >= abs(tobs))
# end MC 
mean(pval < 0.05)
# FIN APROXIMACION POTENCIA