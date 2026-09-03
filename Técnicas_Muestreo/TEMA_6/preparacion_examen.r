
# EXAMEN

# Ejercicicio 1 muestre estratificado (2.5)
# - medio poblacional
# - ⁠total poblacional
# - ⁠varianza y error de muestreo del estimador

# Supongamos que nos piden con afijación proporcional. Supongamos que tenemos unos vectores con los datos

muestras <- list(x1, x2, x3, x4)
medias <- sapply(muestras, function(estrato) mean(estrato))
media_estimada <- sum(wh*medias)
total_estimado <- N * media_estimada
var_estimado <- sum(wh**2 * (1 - fh) * S2 / nh)
error_estimado <- sqrt(var_estimado)

# Supongamos que nos piden con afijación proporcional. Supongamos que tenemos unos vectores con los datos 

muestras <- list(x1, x2, x3, x4)
medias <- sapply(muestras, function(estrato) mean(estrato))
media_estimada <- sum(wh*medias)
total_estimado <- N * media_estimada
var_estimado_min <- sum(wh**2 * S2 / nh)
error_estimado_min <- sqrt(var_estimado_min)



# Ejercicio 2: métodos indirectos de estimación:
# •	⁠Razón
# * ⁠Total y media
# * ⁠Sesgo y error

razon <- mean(y)/mean(x) # Suponiendo que la y es la variable de estudio y que depende del valor de X
media_y_estimada <- mean(X)* razon  # Esta mean es la de la poblacional
total_y_estimado <- N * media_y_estimada


# Si nos preguntan por el sesgo de la razón
f <- n/N
sesgo_razon <- (1-f)*(R*var(y)**2 - cov(x,y))/(n*(mean(x)**2))
varianza_estimada_razon <- (1 - f) / (n * mean(x)**2) * (var(y) + razon**2 * var(x) - 2 * razon * cov(x,y))
error_sesgo <- sqrt(varianza_estimada_razon)



# Ejercicio 3 conglomerados:
# •⁠  ⁠media poblacional
# •⁠  ⁠⁠total poblacional
# •⁠  ⁠⁠varianza y error de muestreo del estimador

# Si nos preguntan por una etapa con mismo tamaño 

N <-      # número total de conglomerados en la población
n <-      # número de conglomerados seleccionados
Mbar <-   # tamaño de cada conglomerado

M <- N * Mbar

Xi <- sapply(datos, sum)     # total de cada conglomerado muestral
xbari <- sapply(datos, mean) # media de cada conglomerado muestral

f <- n / N

media_estimada <- mean(xbari)
total_estimado <- M * media_estimada

s2_medias <- var(xbari)
Vhat_media <- (1 - f) * s2_medias / n
EE_media <- sqrt(Vhat_media)

# Ejercicio 4 sistematico:
# •⁠  ⁠función muestras_sistematicas
# •⁠  ⁠⁠qué es un paso
# •⁠  ⁠⁠varianza exacta de la media
# •⁠  ⁠⁠estimar media y total dado un arranque
# •⁠  ⁠⁠dado el mismo arranque varianza de diferencias sucesivas


muestras_sistematicas_examen <- function(N,n){
    k <- N/n 
    if (k != floor(k)){stop("Para este tema, se estudia únicamente pasos enteros")}
    k <- as.integer(k)
    muestras <- lapply(1:k, function(delta){
        seq(from = delta, by = k, length.out= n)
    })
    names(muestras) = paste0("delta_", 1:k)
    muestras
}

# En muestreo sistemático, el paso es el salto fijo entre un elemento seleccionado y el siguiente dentro de una lista ordenada.

muestras <- muestras_sistematicas_examen(N,n)
medias <- sapply(muestras, function(sistema) mean(X[sistema]))
media_pob <- mean(X)
varianza_exacta <- mean((medias-media_pob)**2)

# Estiamr la media dado el arranque 
delta <- 3
s1 <- seq(from = delta, by = k , length.out= n)
x1 <- X[s1]
media_estimada <- mean(x1)
total_estimado <- N*media_estimada

# Varianza en sucesivas
suma_dif <- sum(diff(x1)**2)
varianza_sucesiva <- (1-f)*suma_dif/(2*N*(n-1))
error_sucesivo <- sqrt(varianza_sucesiva)

varianza_total_sucesiva <- N^2 * varianza_sucesiva
error_total_sucesivo <- sqrt(varianza_total_sucesiva)