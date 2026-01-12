# EJERCICIO 3.3 

##################
## APARTADO A) ### 
##################

# QUEREMOS COMPARAR DOS DIFERENTES DISTRIBUCIONES. PARA ELLO USAMOS MONTECARLO. 

MC <- 10000
x = rgamma(10000, 2, rate = 2)
x_teorica = rgamma(10000, 2, rate = 1)
c = 2
x_def = numeric(MC)
for (i in 1:MC){
    x_def[i] = c*x[i]
}


hist(x_teorica, breaks = 50, freq = FALSE, col = rgb(0,0,1,0.25),
     main = "Histogramas", xlab = "x")
hist(x_def, breaks = 50, freq = FALSE, col = rgb(1,0,0,0.25), add = TRUE)

ks.test(x_teorica, x_def)
## PODEMOS COMPROBAR QUE EFECTIVAMENTE NO SE PUEDE RECHAZAR LA HIPÓTESIS NULA PORQUE P-VALUE ES DE 0.251



##################
## APARTADO B) ### 
##################

MC_1 <- 10000
MC_2 <- 100


x <- matrix(rgamma(MC_1*MC_2, shape = 2, rate = 1), nrow = MC_1, ncol = MC_2)
x_practica = numeric(MC_1)
x_teorica = rgamma(MC_1, 200, 1)

for (i in 1:MC_1){
  x_practica[i] <- sum(x[i, ])
}

hist(x_practica, breaks = 100, col = "red", main ="Histogramas")
hist(x_teorica, breaks = 100, col = "skyblue", add = TRUE)

ks.test(x_teorica, x_practica)
## PODEMOS COMPROBAR QUE EFECTIVAMENTE NO SE PUEDE RECHAZAR LA HIPÓTESIS NULA PORQUE P-VALUE ES DE 0.6756



##################
## APARTADO C) ### 
##################

MC = 10000
x = rbeta(MC, 2,1)
x_teorica = rexp(MC, 2)
x_practica = numeric(MC)

for (i in 1:MC) { 
    x_practica[i] = -log(x[i]) 
}
hist(x_practica, breaks = 100, col = "red", main ="Histogramas")
hist(x_teorica, breaks = 100, col = "skyblue", add = TRUE)

ks.test(x_practica, x_teorica)

## PODEMOS COMPROBAR QUE EFECTIVAMENTE NO SE PUEDE RECHAZAR LA HIPÓTESIS NULA PORQUE P-VALUE ES DE 0.3762