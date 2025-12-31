# EJERCICIO 
# SEA f(,y) UNA FUNCIÓN DEFINIDA SOBRE UNA REGIÓN RECTANGULAR [a,b]x[c,d] Y (x,y) UN VECTOR ALEATORIO CON DISTRIBUCIÓN UNIFORME EN D 
# ENTONCES: f_u (x,y)  = \frac{1}{(b-a)(d-c)} para todo (x,y) pertenenciente a D
# E[h(X,Y)] = \int{\int{h(x,y)f_u(x,y)dydx = \frac{1}{(b-a)(d-c)}\int_a^b{\int_c^d{h(x,y)dydx 


# ALGORITMO MC: 
##### 1.- GENERAMOS X_1, .....X_N QUE SIGAN UNA U(a,b)
##### 2.- GENERAMOS Y_1, .....Y_N QUE SIGAN UNA U(c,d)
##### 3.- [1/n * sum_i=1^nH(x_i, y_i)] (b-a)(d-c) = I
##### I = \int_0^pi/{\int_0^pi/2{sen(x)cos(y)dydx = 2



# APROXIMAMOS LA INTEGRAL DOBLE EN (0,PI)X(0,pi/2) en (sin(x)*(cos(y)))*(b-a)*(d-c)

set.seed(1)
a<-0
b<-pi
c<-0
d<-pi/2
MC <- 10000

x<-runif(MC,a,b)
y<-runif(MC, c,d)
I_est= mean(sin(x)*(cos(y)))*(b-a)*(d-c)
I_est 