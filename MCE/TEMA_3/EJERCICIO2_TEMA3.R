# TEMA 3. EJERCICIO 2 
set.seed(1)
MC <- 10000
x <- rnorm(MC,2,4)
y <- runif(MC,1,5)
p <- numeric(MC)

for (i in 1:MC){
	p[i] = ifelse((x[i]<y[i]),1,0)
}
TA = table(p)
print(TA)