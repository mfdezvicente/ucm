# EJERCICIO 1
x <- c(-4,5,-7,10,1)
n = length(x)
jack = matriz(NA, nrow =n, ncol = (n-1))

for (i in 1:n){
    jack[i,] = x[-i]
}
jack


mjack = function(x, estadistico){
    n = length(x)
    jack = matriz(NA, nrow =n, ncol = (n-1))

    for (i in 1:n){
        jack[i,] = x[-i]
    }
    return(apply(jack,1, estadistico))
}

mjack(x, mean)

