# EJERCICIO 1 
# Considera la población Ω = {A, B, C, D, E} y tamaño muestral n = 3
# A) Calcula el número de muestras posibles en los 4 esquemas: ordenado con reposición, ordenado sin reposición, no ordenado sin reposición, no ordenado con reposición.

Omega = c('A','B','C', 'D', 'E')
n = 3
S_osr  <- muestras_ordenado_sin_rep(Omega, n)      # ordenado sin reposición
S_nosr <- muestras_no_ordenado_sin_rep(Omega, n)   # no ordenado sin reposición
S_ocr  <- muestras_ordenado_con_rep(Omega, n)      # ordenado con reposición
S_nocr <- muestras_no_ordenado_con_rep(Omega, n)   # no ordenado con reposición

n_muestras_osr = nrow(S_osr)
n_muestras_nosr = nrow(S_nosr)
n_muestras_ocr = nrow(S_ocr)
n_muestras_nocr = nrow(S_nocr)

cat("El número de muestras ordenadas sin reposición es:", n_muestras_osr, "\n")
cat("El número de muestras no ordenadas sin reposición es:", n_muestras_nosr, "\n")
cat("El número de muestras ordenadas con reposición es:", n_muestras_ocr, "\n")
cat("El número de muestras no ordenadas con reposición es:", n_muestras_nocr, "\n")

# B) Enumera cada espacio muestral con las funciones de muestras y verifica que el tamaño coincide con las fórmulas.



# EJERCICIO 2
