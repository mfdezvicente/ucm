# EJERCICIO 2.2

set.seed(1)
n = 100
u = runif(n)
x = ifelse(u<0.15, 1, ifelse(u<0.2, 2, ifelse(u<0.55, 3, 4)))
print(x)
x

print(mean(x==1))
print(mean(x==2))
print(mean(x==3))
print(mean(x==4))

TA= table(x)
print(TA)

hist(x, freq = FALSE, breaks = c(0.5:4.5), main = "Histograma de variable discreta")
chisq.test(TA, p=c(0.15, 0.05, 0.35, 0.45))