#-------------------------------------------------------------------------------
# EJERCICIO 8
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
# EJERCICIO 9
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
# EJERCICIO 10
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
# EJERCICIO 11
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
