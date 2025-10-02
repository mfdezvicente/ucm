muestra=c(0.563,0.478,0.218,0.396,0.455,0.624,0.527,0.163,0.527,0.692,0.187,0.005,0.0382,0.923,0.147,0.811, 0.531,0.545,0.450,0.839,0.999,0.536,0.926,0.373,.986,.810,.067,.471,.824,.825,.809,.603,.397,.197,.811,.620,.671,.867,.02,.635,.429,.274,.264,.217,.446,.049,.945,.132,.238,.082)
n=length(muestra)
print(n)

signos = numeric(n-1)
for (i in 1:(n-1)){
    signos[i] = sign(muestra[i+1]-muestra[i])
}
print(signos)
npos = sum(signos >= 0) # Número de +
npos = sum(signos <= 0) # Número de -

muestra_1 = muestra[-1] # Quita el primer elemento
muestra_2 = muestra[-n] # Quita el último elemento
S = muestra_1 - muestra_2
nnpos = sum(S > 0) # Número de +
nneg = sum(S < 0) # Número de -
print (nnpos)
print (nneg)

#Recuento de rachas
R= 0
for (i in 1:(n-2)){
    if (signos[i] != signos[i+1]){
        R = R + 1
    }
}
print(R) # Número de rachas

#Calculamos el valor del estadístico
m = n-1
Z = (R+ 0.5-(1+2*npos*nneg/m))/sqrt(2*npos*nneg*(2*npos*nneg - m)/(m^2*(m-1)))
print(Z)
p_valor = 2*(1 - pnorm(abs(Z))) # Prueba bilateral
print(p_valor)




# -----------------------------------------------------------------

muestra=c(0.563,0.478,0.218,0.396,0.455,0.624,0.527,0.163,0.527,0.692,0.187,0.005,0.0382,0.923,0.147,0.811,
          0.531,0.545,0.450,0.839,0.999,0.536,0.926,0.373,.986,.810,.067,.471,.824,.825,.809,.603,.397,.197,
          .811,.620,.671,.867,.02,.635,.429,.274,.264,.217,.446,.049,.945,.132,.238,.082)

n = length(muestra)
print(n)

# Signos de las diferencias consecutivas
signos = sign(diff(muestra))

# Eliminar empates (0)
signos = signos[signos != 0]

# Recontar + y -
npos = sum(signos > 0)
nneg = sum(signos < 0)

# Número de rachas: 1 + cambios de signo
R = 1 + sum(diff(signos) != 0)
print(R)

# Estadísticos bajo H0
m = npos + nneg
ER = 1 + (2*npos*nneg)/m
VR = (2*npos*nneg*(2*npos*nneg - m)) / (m^2 * (m - 1))

# Corrección por continuidad (bilateral)
cc = ifelse(R > ER, -0.5, 0.5)

Z = (R + cc - ER) / sqrt(VR)
p_valor = 2 * (1 - pnorm(abs(Z)))

print(c(npos=npos, nneg=nneg, m=m))
print(Z)
print(p_valor)

