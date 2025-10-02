# Tema 1 - Números aleatorios y pseudoaleatorios
# Autor: Miguel Fernández
# Fecha: 2025 - 09 - 17

# Generar congruencias 
# x_n= (a*x_{n-1}) mod m
#------------------------------------------------------------------------------------------------------#
m = 2^31 - 1
a = 7^5
c = 0

# Fijamos la semilla
x0 = 1

#Fijamos el tamaño de la secuencia 
n = 100
v = numeric(n)
v[1] = (a*x0 + c) %% m # Primer valor depende de la semilla 
for (i in 2:n){
  v[i] = (a*v[i-1] + c) %% m
}
#print(v)

u = v/m # Normalizamos los valores entre 0 y 1
#print(u)

# ----------------------PARTE 2--------------------------------#
#----------------------#---------------------------------------#
# Ahora vamos a hacer lo mismo pero con c distinto de 0
# A partir de ahora vamos a crear una función que haga el proceso anterior. Para ello, debería introducir los valosres de 
# a, c, m, x0 y n . Aunque por defecto vamos a poner algunos anteriores valores 

g = function(x0, n, a=7^5, c=0, m=2^31 - 1){
  v = numeric(n)
  v[1] = (a*x0 + c) %% m # Primer valor depende de la semilla 
  for (i in 2:n){
    v[i] = (a*v[i-1] + c) %% m
  }
  u = v/m # Normalizamos los valores entre 0 y 1
  return(print(u))
}

#g(10,100)



# HAY UN COMANDO DE R QUE SE LLAMA runif QUE HACE LO MISMO QUE LA FUNCIÓN ANTERIOR PERO DE FORMA MÁS SENCILLA
# runif(n, min=0, max=1) genera n números aleatorios entre min y max
#------------------------------------------------------------------------------------------------------#
set.seed(1) # Fijamos la semilla
v= runif(100)
print(v)

#------------------------------------------------------------------------------------------------------#
# TERMINADA ESTA PARTE, VOLVEMOS A LA TEORÍA. 

