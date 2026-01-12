# MONTECARLO PARA LA COMPARACIÓN 
B=10000
v = numeric(B)
x = rnorm(B, mean = 2, sd = 2)
y = runif(B, min = 1, max =5)

for (i in 1:B){
    v[i] = ifelse(x[i]<y[i], 1, 0)
}

prob = mean(v)
prob
