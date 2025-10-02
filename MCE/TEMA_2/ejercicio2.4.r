#EXPONENCIAL 

#GENERO 100 VALORES DE LA EXP (LMBDA)
lambda = 3
n = 100
set.seed(1)
u = runif(100)
x = -log(1-u)/lambda

print(ks.test(x,pexp,3))


#par(mfrow=c(1,2))
#hist(x,freq = FALSE)    
#curve(dexp(x,3),add=TRUE, col="red")

#plot(ecdf(x))
#curve(pexp(x,3), add=TRUE, col ="red")



# ---------------- WEIBULL ------------------------#

set.seed(1)
n=100
u = runif(n)
x = sqrt((-log(1-u)))
print(x)

set.seed(1)
n = 100
u = runif(n)
x = sqrt(-log(1-u))

# Comparar con rweibull
y = rweibull(n, shape = 2, scale = 1)

# Histogramas comparados
hist(x, freq = FALSE, col = rgb(1,0,0,0.5), main = "Weibull k=2, λ=1")
curve(dweibull(x, shape=2, scale=1), add=TRUE, col="blue", lwd=2)

ver = ks.test(x, pweibull, shape=2, scale = 1)

print(ver)

# ----------------- TRIANGULAR ---------------------#
#FUNCION TRIANGULAR 
dtrianng = function(x) { 
    x *(0<x & x<=1) + (2-x) * (1<x & x<2)

}

# GENERO 100 VALORES DE UNA TRIANGULAR
n = 100
set.seed(1)
u = runif(n)
x = ifelse(u<=0.5, sqrt(2*u), 2 - sqrt(2*(1-u)))
print(x)
hist(x, freq = FALSE)
curve(dtrianng, add=TRUE, col="red")
print(ks.test(x, ptriang, 0, 2, 1)) # ptriang es la función de distribución acumulada de la triangular

# ----------------- CAUCHY ---------------------#

# GENERAMOS 100 VALORES DE UNA CAUCHY (0,1)
set.seed(1)
n= 100 #Fijo el tamaño de la secuancia a obtener

# PASO 1 
u = runif(n)

#PASO 2
x = tan(pi*(u-1/2))
print(x)


#---------------------------------------------------------------------#
#---------COMPROBAMOS QUE VERDADERAMENTE ES UNA CAUCHY----------------#
#---------------------------------------------------------------------#


hist(x, freq = FALSE, ylim= c(0, 0.35))
curve(dcauchy(x), add = TRUE, col = "blue")



# OTRA FORMA

emp = ecdf (x)
plot(emp)
curve(pcauchy(x), add = TRUE, col='red')
print(ks.test(x,pcauchy))


#------------------------------BERNOUILLI---------------------------------------#
#GENERAMOS 100 VALORES DE UNA BERNOUILI DE PARAMETRO 
p = 0.7
n = 10000
set.seed(1)
# PASO 1
u = runif(n)
# PASO 2 
x = ifelse (u <= (1-p), 0,1)
print(x)

hist(x)

#OBTENERMOS LA PROPORCI´NO
print (mean(x==0))
print (mean(x==1))


# FORMA 2
emp = ecdf(x)
plot(emp)
curve(pbinom(x,1,p), add=TRUE, col = "red")

#CONTRASTE DE LA JI-CUADRADOS
TAA = table(x)