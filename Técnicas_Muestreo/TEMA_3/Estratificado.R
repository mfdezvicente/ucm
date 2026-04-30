#-------------------------------------------------------------------------------
# EJERCICIO 8 - TAMAÑO MUESTRAL PARA AFIJACION PROPORCIONAL Y DE MÍNIMA VARIANZA CON Y SIN REPO (ESTIMADOR MEDIA)
#-------------------------------------------------------------------------------

n_h = function(w_h, N) w_h * N

s2 = function(sigma_h, w_h, N){
  sigma_h^2 * (n_h(w_h, N) / (n_h(w_h, N) - 1))
}

s = function(s2) sqrt(s2)

n_afij_prop_sin_rep = function(wh, cuasivarianza, error, N) (sum(wh * cuasivarianza)) / (error + (1 / N * sum(wh * cuasivarianza)))

n_afij_prop_con_rep = function(wh, cuasivarianza, error) sum(wh * cuasivarianza) / error

n_afij_min_var_sin_rep = function(wh, cuasivarianza, cuasidesviacion, error, N) (sum(wh * cuasidesviacion))^2 / (error + (1 / N * sum(wh * cuasivarianza)))

n_afij_min_var_con_rep = function(wh, cuasivarianza, cuasidesviacion, error) (sum(wh * cuasidesviacion))^2 / error

nh_prop = function(wh, n) wh * n

nh_min_var = function(wh, n, cuasidesviacion) n * wh * cuasidesviacion / (sum(wh * cuasidesviacion))

#-------------------------------------------------------------------------------

N = 1000
estrato = c(1, 2, 3)
wh = c(0.6, 0.3, 0.1)
sigma = c(4, 12, 80)

nh = n_h(wh, N)
cuasivarianza = s2(sigma, wh, N)
cuasidesviacion = s(cuasivarianza)

tabla = data.frame(estrato, wh, sigma, nh, cuasivarianza, cuasidesviacion)
tabla


prop_sin_rep = n_afij_prop_sin_rep(wh, cuasivarianza, 5, N)
prop_sin_rep

prop_con_rep = n_afij_prop_con_rep(wh, cuasivarianza, 5)
prop_con_rep

ceiling(n_afij_prop_sin_rep(wh, cuasivarianza, 5, N))
ceiling(n_afij_prop_con_rep(wh, cuasivarianza, 5))

min_var_sin_rep = n_afij_min_var_sin_rep(wh, cuasivarianza, cuasidesviacion, 5, N)
min_var_sin_rep

min_var_con_rep = n_afij_min_var_con_rep(wh, cuasivarianza, cuasidesviacion, 5)
min_var_con_rep

ceiling(n_afij_min_var_sin_rep(wh, cuasivarianza, cuasidesviacion, 5, N))
ceiling(n_afij_min_var_con_rep(wh, cuasivarianza, cuasidesviacion, 5))

#-------------------------------------------------------------------------------

nh_prop_sin_rep = round(nh_prop(wh, prop_sin_rep))
nh_prop_sin_rep
sum(nh_prop_sin_rep)

nh_prop_con_rep = round(nh_prop(wh, prop_con_rep))
nh_prop_con_rep
sum(nh_prop_con_rep)

nh_min_var_sin_rep = round(nh_min_var(wh, min_var_sin_rep, cuasidesviacion))
nh_min_var_sin_rep
sum(nh_min_var_sin_rep)

nh_min_var_con_rep = round(nh_min_var(wh, min_var_con_rep, cuasidesviacion))
nh_min_var_con_rep
sum(nh_min_var_con_rep)

#-------------------------------------------------------------------------------

data.frame(nh_prop_sin_rep, nh_prop_con_rep, nh_min_var_sin_rep, nh_min_var_con_rep)

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# EJERCICIO 9 - ESTIMADOR DE LA MEDIA, ERROR DE MUESTREO (AFIJACIÓN PROPORCIONAL)
#-------------------------------------------------------------------------------
Nh = c(24, 36, 30, 30)
N = sum(Nh)
nh = c(4, 6, 5, 5)
Wh = Nh / N
fh = nh / Nh

data.frame(Estrato = 1:4, Nh = Nh, nh = nh, Wh = Wh, fh = fh)

# El error de muestreo cabe esperar que no se parezca mucho.

estrato1 = c(94, 90, 102, 110) 
estrato2 = c(91, 99, 93, 105, 111, 101)
estrato3 = c(108, 96, 100, 93, 93)
estrato4 = c(92, 110, 94, 91, 113)

muestras = list(estrato1, estrato2, estrato3, estrato4)
muestras

xbar = sapply(muestras, mean)
s2 = sapply(muestras, var)

tabla_estratos = data.frame(
  Estrato = paste0("Estrato", 1:4),
  n_h = sapply(muestras, length),
  Media_muestral = xbar,
  Cuasivarianza = s2
)

tabla_estratos

xbar_st = sum(Wh * xbar)
xbar_st

var_xbar_st = sum(Wh^2 * (1 - fh) * s2 / nh)
var_xbar_st

se_xbar_st = sqrt(var_xbar_st)
se_xbar_st

# Vampos a compararlo con muestreo aleatorio simple sin reposición.

x = c(94, 90, 102, 110, 91, 99, 93, 105, 111, 101, 108, 96, 100, 93, 93, 92, 110, 94, 91, 113)
n = length(x)
N = 120

xbar = mean(x)
xbar

s2 = var(x)
s2

f = n / N

var_xbar = (1 - f) * s2 / n

se_xbar = sqrt(var_xbar)
se_xbar

# El error es menor porque la muestra es muy pequeña. para muestras más grandes gana el muestreo estratificado.

#-------------------------------------------------------------------------------
# EJERCICIO 10 - ESTIMACIÓN DE LA PROPORCION Y DEL ERROR DE MUESTREO 
#-------------------------------------------------------------------------------
N = 10000
nh = c(1050, 900, 1700)
n = sum(nh)
Nh_aprox = nh * N / n

ceiling(Nh_aprox)
sum(ceiling(Nh_aprox)) # Aquí me cato que no suma 10000 (sale 10001), así que redondeo el más pequeño para abajo.

Nh = c(2877, 2466, 4657)
Wh = Nh / n
ph = c(800 / 1050, 600 / 900, 1300 / 1700)
qh = 1 - ph
fh = nh / Nh

p_est = sum(Wh * ph)
var_p_est = sum(Wh^2 * (1 - fh) * ph * qh / (nh - 1))
se_p_est = sqrt(var_p_est)

data.frame(
  Estrato = c("A", "B", "C"),
  Nh = Nh,
  nh = nh,
  Wh = Wh,
  ph = ph,
  qh = qh, 
  fh = fh
)

#-------------------------------------------------------------------------------
# EJERCICIO 11 - TAMAÑO MUESTRAL PARA MEDIA Y PROPORCIÓN CON REMPLAZAMIENTO
#-------------------------------------------------------------------------------
Nh = c(120, 80)
N = sum(Nh)
Wh = Nh / N
Wh
S = c(4, 4)

error_media = 1

# Celing nunca en una funcion, siempre fuera
# Si la suma no cuadra, lo defino a mano, pero hay que poner la suma que sino penalizará.

n_media = sum(Wh * S^2) / error_media^2
n_media

nh_media = Wh * n_media
nh_media

ceiling(nh_media)
sum(ceiling(nh_media))

# Como no sale 16, aproximamos
nh = c(10, 6)
nh

e_prop = 0.1
n_prop = sum(Wh * 0.5 * 0.5) / e_prop^2
n_prop

nh_prop = Wh * n_prop
nh_prop



#####################################################################################
## TEMA 3 - PRÁCTICA EXAMEN 
#####################################################################################

N_h = function(N, wh){
  N*wh
}

n_h_prop = function(n, wh){
  n*wh
}
n_h_min = function(n, w, sh){
  (n*wh*sh)/sum(wh*sh)
}

S_h2 = function(N_h, sigma){
  N_h/(N_h -1)* sigma**2
}

sh = function(S_h2){
  sqrt(S_h2)
}

fh = function(nh, N_h)

n_prop_no_rep = function(wh, S_h2, N, error){
  sum(wh * S_h2)/(error**2  + sum(wh * S_h2)/N)
}
n_prop_rep = function(wh, S_h2, N, error){
  sum(wh * S_h2)/(error**2)
}

n_min_no_rep = function(wh, sh, N, error){
  (sum(wh * sh)**2)/(error**2  + 1/N * sum(wh * (sh**2)))
}
n_min_rep = function(wh, sh, error){
  (sum(wh * sh)**2)/(error**2)
}


## ESTIMADORES 

x_media = function(wh, x_h){
  sum(wh * mean(x_h))
}

var_hat_media_sin_rep = function(wh, fh, S_h2, nh){
  sum(wh**2 * (1-fh) * sh**2/nh)
}

var_hat_media_con_rep = function(wh, S_h2, nh){
  sum(wh**2 * S_h2/nh)
}

prop = function(wh, ph){
  sum(wh*ph)
}






### EJERCICIO 8 

N <- 1000
wh <- c(0.6, 0.3, 0.1)
sigma_h <- c(4, 12, 80)
N_h <- N*wh
S_h2_muestra <- S_h2(N_h, sigma_h)
sh2_muestra <- sh(S_h2_muestra)

# AFIJACIÓN PROPORCIONAL NO REP
n1 <- n_prop_no_rep(wh, S_h2_muestra, N, sqrt(5))
n1
ceiling(n1)

nh_prop_sin_rep = n1*wh
nh_prop_sin_rep_ceil <- ceiling(nh_prop_sin_rep)
nh_prop_sin_rep_ceil
# AFIJACIÓN PROPORCIONAL REP
n2 <- n_prop_rep(wh,S_h2_muestra, N, sqrt(5))
n2
ceiling(n2)
nh_prop_sin_rep_2 = n2*wh
nh_prop_sin_rep_ceil_2 <- ceiling(nh_prop_sin_rep_2)
nh_prop_sin_rep_ceil_2

# AFIJACIÓN MINIMA VARIANZA NO REP
n3 <- n_min_no_rep(wh, sh2_muestra, N, sqrt(5))
n3
ceiling(n3)

nh_min_sin_rep_3 = n3*wh*sh2_muestra/(sum(wh * sh2_muestra))
nh_min_sin_rep_ceil_3 <- ceiling(nh_min_sin_rep_3)
nh_min_sin_rep_ceil_3
# AFIJACIÓN MINIMA VARIANZA REP
n4 <- n_min_rep(wh,sh2_muestra, sqrt(5))
n4
ceiling(n4)
nh_min_sin_rep_4 = n4*wh*sh2_muestra/(sum(wh * sh2_muestra))
nh_min_rep_ceil_4 <- ceiling(nh_min_sin_rep_4)
nh_min_rep_ceil_4


# EJERCICIO 9 
N_h = c(24, 36, 30, 30)
N <- 120 
wh <- N_h/N
n <- 20
nh <- c(4,6,5,5)
fh_muestra <- fh(nh, N_h)
x1 <- c(94, 90, 102, 110)
x2 <- c(91, 99, 93, 105, 111, 101)
x3 <- c(108, 96, 100, 93, 93)
x4 <- c(92,110, 94, 91, 113)

muestras <- list(x1, x2, x3, x4)
x_medias <- sapply(muestras, mean)
ventas_medias <- x_media(wh, x_medias)
sigma <- sapply(muestras, var)
cuasi <- S_h2(N_h, sigma)
varianza <- var_hat_media_con_rep(wh, sigma, nh)

ventas_medias
varianza
error <- sqrt(varianza)
error
