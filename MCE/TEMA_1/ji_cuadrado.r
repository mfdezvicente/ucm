# VAMOS A USAR ESTE SCRIPT PARA REALIZAR EL TEST DE CHI CUADRADO
# ESTE TEST ES PARA VARIABLES DISCRETAS
set.seed(1)
num.al = runif(100)
# CREAMOS UNA FUNCIÓN QUE NOS HAGA EL TEST DE CHI CUADRADO
# PARA ELLO, VAMOS A AGRUPAR LOS NÚMEROS EN 10
chi_cuadrado = function(num.al, k){
  n = length(num.al)
  # CREAMOS LOS INTERVALOS
  intervalos = seq(0, 1, length.out = k+1)
  # CREAMOS UNA TABLA DE FRECUENCIAS OBSERVADAS
  frecuencias_observadas = hist(num.al, breaks = intervalos, plot = FALSE)$counts
  # FRECUENCIA ESPERADA
  frecuencias_esperadas = rep(n/k, k)
  # REALIZAMOS EL TEST DE CHI CUADRADO
  test = chisq.test(frecuencias_observadas, p = rep(1/k, k))
  return(test)
}
res = chi_cuadrado(num.al, 10)
str(res)
print(res$p.value)
# SI EL P VALUE ES MAYOR QUE 0.05, NO RECHAZAMOS
# SI EL P VALUE ES MENOR QUE 0.05, RECHAZAMOS LA HIPÓTESIS NULA
# AHORA VAMOS A HACERLO DE MANERA GRÁFICA
# DIBUJAMOS LAS FRECUENCIAS OBSERVADAS Y ESPERADAS
barplot(table(cut(num.al, breaks = seq(0, 1, length.out = 11))), main = "Frecuencias observadas vs esperadas", xlab = "Intervalos", ylab = "Frecuencias", col = "lightblue")
abline(h = length(num.al)/10, col = "red", lwd = 2) # Dibuja la línea de la frecuencia esperada
legend("topright", legend = c("Frecuencia esperada"), col = c("red"), lwd = 2)  


# --------------------------- MANERA DE LA PROFESORA --------------------------- #
# FORMA 1 
set.seed(1)
num.al = runif(100)
hist(num.al)

cortes = seq(0,1, by=0.1) # Crea los intervalos
tabla = table(cut(num.al, cortes)) # Crea la tabla de frecuencias observ
print(tabla)

contraste = chisq.test(tabla) # Realiza el test de chi cuadrado
print(contraste)
print(contraste$p.value) # Imprime el p-value


# FORMA 2
frec.obs = numeric()
for (i in 1:(length(cortes)-1)){
  frec.obs[i] = sum((cortes[i] <num.al) & (num.al <= cortes[i+1]))
}

print(frec.obs)
V = sum((frec.obs - 0.1*length(num.al))^2/(0.1*length(num.al)))
print(V)

p_valor = 1- pchisq(V, (length(cortes)-1)-1) # Grados de libertad = número de intervalos - 1 - número de parámetros estimados (en este caso 0 
print(p_valor)


# FORMA 3
set.seed(1)
res = chisq.test(frec.obs, p=rep(0.1,times =length(frec.obs))) # p es la probabilidad teórica de cada intervalo
print(res$p.value)