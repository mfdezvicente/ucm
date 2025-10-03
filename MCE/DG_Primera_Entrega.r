## -------------------------------------------------##
## TAREA 1 - METODOS COMPUTACIONALES EN ESTADISTICA-##
## NOMBRE: MIGUEL FERNÁNEZ VICENTE -----------------##
## DNI: 03203763R ----------------------------------##
##--------------------------------------------------##

## EJERCICIO 1 ##
# GENERAR 100 VALORES DEL LANZAMIENTO DE UNA MODELA CON PROBABILIDAD DE CARA DE 1/6.

set.seed(1)
n = 100
u = runif(n)
x = ifelse(u<=1/6, 1, 0)
print(x)
print(mean(x))  # Aproximacion de la probabilidad de cara
print(var(x))   # Aproximacion de la varianza

## EJERCICIO 2 ##
# UN SEMAFORO TINE 0.4 VERDE, 0.1 AMARILLO Y 0.5 ROJO. SIMULAR 100 SEMAFOROS.
set.seed(1)
n = 1000
u = runif(n)
x = ifelse(u<=0.4, "VERDE", ifelse(u<=0.5, "AMARILLO", "ROJO"))
print(x)
print(mean(x=="VERDE"))    # Aproximacion de la probabilidad de verde
print(mean(x=="AMARILLO"))  # Aproximacion de la probabilidad de amarillo
print(mean(x=="ROJO"))      # Aproximacion de la probabilidad de rojo   
print(table(x))             # Tabla de frecuencias
barplot(table(x))         # Diagrama de barras

## EJERCICIO 3 ##
# LA PROBABILIDAD DE UN DÍA DESPEJADO PARA MAÑANA SI HOY ES DESPEJADO ES 0.8, MIENTRAS QUE SI HOY ES LLUVIOSO ES 0.4. SIMULAR 100 DÍAS.
# EN EL CASO DE QUE SEAN DEPENDIENTES # 
set.seed(1)
n = 100
u = runif(n)
x = numeric(n)
x[1] = ifelse(u[1]<=0.5, "DESPEJADO", "LLUVIOSO")  # Suponemos que el primer día tiene igual probabilidad de ser despejado o lluvioso
for (i in 2:n) {
    if (x[i-1] == "DESPEJADO") {
        x[i] = ifelse(u[i]<=0.8, "DESPEJADO", "LLUVIOSO")
    } else {
        x[i] = ifelse(u[i]<=0.4, "DESPEJADO", "LLUVIOSO")
    }
}
print(x)
print(mean(x=="DESPEJADO"))  # Aproximacion de la probabilidad de despejado
print(mean(x=="LLUVIOSO"))   # Aproximacion de la probabilidad de lluvioso
print(table(x))              # Tabla de frecuencias
barplot(table(x))          # Diagrama de barras

# EN ESTE CASO NO SON INDEPENDIENTES, YA QUE LA PROBABILIDAD DE UN DÍA DEPENDE DEL DÍA ANTERIOR.

## EN EL CASO DE QUE SON INDEPENDIENTES ##
set.seed(1)
n = 100
u = runif(n)
x = ifelse(u<=0.5, "DESPEJADO", "LLUVIOSO")  # Suponemos que cada día tiene igual probabilidad de ser despejado o lluvioso
print(x)
print(mean(x=="DESPEJADO"))  # Aproximacion de la probabilidad de
print(mean(x=="LLUVIOSO"))   # Aproximacion de la probabilidad de lluvioso
print(table(x))              # Tabla de frecuencias
barplot(table(x))          # Diagrama de barras

