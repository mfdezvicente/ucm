class=c(137,138.5,140,141,142,143.5,145,147,148.5,150,153,154,155,156.5,157,158,158.5,159,160.5,161,162,167.5)
B = 100
set.seed(1)
mediab <- numeric(B)
medianab <- numeric(B)
for (i in 1:B){
    classb <- sample(class, replace = TRUE)
    mediab[i] <- mean(classb)
    medianab[i] <- median(classb)
}

mediab
medianab

## Si quiero calcular el error boostrap
sd(mediab)
sd(medianab)

# Para calcular el valor del sesgo
sesgo_mediab <- mean(mediab) - mean(class)
sesgo_medianab <- mean(medianab) - mean(class)

sesgo_mediab
sesgo_medianab

varianza = function(x){
    var(x) * length(x-1)/length(x)
}


#####################################################


## PRUEBAS EXAMEN

datos <- c(18.1,8.6,4.7,7.3,3.5,12.4,13.8,1.2)

B <- 100
n <- length(datos)
mediab <- numeric(B)

for (i in 1:B){
  muestra <- sample(datos, size = n, replace = TRUE)  # <- tamaño correcto
  mediab[i] <- mean(muestra)
}

sesgo <- mean(mediab) - median(datos)
sesgo
ks <- ks.test(mediab, "pnorm", mean = mean(mediab), sd = sd(mediab))
ks$p.value


