# Parámetros
S_0 <- 100 # Precio inicial
K <- 100   # Strike
T <- 1     # Tiempo hasta vencimiento
r <- 0.05  # Tasa libre de riesgo
sigma <- 0.2 # Volatilidad

N_steps <- 252 
dt <- T / N_steps
M_paths <- 50000

set.seed(1)

# --- CÁLCULO DEL PRECIO  ---

S_actual = rep(S_0, M_paths)  # Vector con el precio actual de las M simulaciones
S_sum    = rep(0,   M_paths)  # Vector para ir sumando los precios

for (i in 1:N_steps) {
  Z = rnorm(M_paths) 
  
  # Fórmula Recursiva
  S_actual = S_actual * exp((r - 0.5 * sigma^2) * dt + sigma * sqrt(dt) * Z)
  
  # Acumulamos para el promedio asiático
  S_sum = S_sum + S_actual
}

# Calculamos el promedio aritmético final
S_media <- S_sum / N_steps

# Calculamos Payoff y Precio
payoffs_asiatica <- pmax(S_media - K, 0)
precio_asiatica  <- mean(payoffs_asiatica) * exp(-r * T)

cat("--- RESULTADOS DE VALORACIÓN ---\n")
cat("Precio Opción Asiática (Montecarlo):", round(precio_asiatica, 4), "\n")

# --- GRÁFICO ---

# Simulamos solo 50 caminos completos para pintarlos (para que el gráfico sea legible)
N_plot <- 50 
S_matrix = matrix(0, nrow = N_plot, ncol = N_steps + 1)
S_matrix[, 1] = S_0

# Simulamos trayectorias completas
for (t in 1:N_steps) {
  Z = rnorm(N_plot)
  S_matrix[, t + 1] = S_matrix[, t] * exp((r - 0.5 * sigma^2) * dt + sigma * sqrt(dt) * Z)
}

# Tiempos para el eje X
tiempos <- seq(0, T, length.out = N_steps + 1)

plot(tiempos, S_matrix[1, ], type = "n", ylim = range(S_matrix),
     main = "Trayectorias Simuladas (Opción Asiática)",
     xlab = "Tiempo (Años)", ylab = "Precio del Activo",
     las = 1)

grid()

# Pintar todas las trayectorias en gris
for (i in 1:N_plot) {
  lines(tiempos, S_matrix[i, ], col = rgb(0.7, 0.7, 0.7, 0.5))
}

# Resaltar 3 trayectorias en colores
lines(tiempos, S_matrix[1, ], col = "blue",  lwd = 1.5)
lines(tiempos, S_matrix[2, ], col = "red",   lwd = 1.5)
lines(tiempos, S_matrix[3, ], col = "green", lwd = 1.5)

# TENDENCIA ESPERADA (nueva)
tendencia <- S_0 * exp(r * tiempos)
lines(tiempos, tendencia, col = "black", lty = 2, lwd = 2)

legend("topleft", legend = c("Trayectorias Aleatorias", "Tendencia Esperada"),
       col = c("gray", "black"), lty = c(1, 2), lwd = c(1, 2), bg = "white")
