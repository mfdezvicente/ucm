gamma = 1.8 
beta = 0.98
r = 0.03
teta_1 = 0.2
teta_2 = 0
w_1 = 1000
w_2 = 1000



# Modelo de dos periodos con impuestos sobre el trabajo
c_1 = (1/((1+beta)*(1+gamma))) * ((1-teta_1)*w_1 + (1-teta_2)*w_2*(1+r)^(-1))
c_2 = (beta/((1+beta)*(1+gamma))) * ((1-teta_1)*w_1*(1+r) + (1-teta_2)*w_2)
n_1 = 1- gamma*c_1/w_1*(1-teta_1)
n_2 = 1- gamma*c_2/w_2*(1-teta_2)

print(c_1)
print(c_2)
print(n_1)
print(n_2)

# Calculo el valor de la función de utilidad
u = log(c_1) - gamma*log(1-n_1) + beta*(log(c_2) - gamma*log(1-n_2))
print(u)

# Calculo el valor del ahorro
s = w_1*(1-teta_1)*n_1 - c_1
print(s)

# Calculo el nivel de recaudación en valor presente
T = teta_1*w_1*n_1 + teta_2*w_2*n_2/(1+r)
print('El valor de la recaudación es:')
print(T)





