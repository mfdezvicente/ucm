# -----------------------------------------------------------------------------
# VALORACIÓN DE OPCIONES EUROPEAS: VALIDACIÓN Y ANÁLISIS DE CONVERGENCIA
# -----------------------------------------------------------------------------
library (ggplot2)
library (scales)

# --- 1. PARÁMETROS DEL MODELO ---
S_0 <- 100 # Precio inicial
K <- 100 # Strike
T <- 1 # Tiempo hasta vencimiento (1 año)
r <- 0.05 # Tasa libre de riesgo (5%)
sigma <- 0.2 # Volatilidad (20%)

set.seed (1)
M_paths <- 1000000

# --- SIMULACIÓN MONTECARLO (UN SOLO PASO) ---
Z <- rnorm (M_paths)

S_T = S_0 * exp((r - 0.5 * sigma^2) * T + sigma * sqrt(T) * Z)

# Calculamos los payoffs de una Call: max(S_T - K, 0)
payoffs = pmax (S_T - K, 0)

# Descontamos a valor presente
precio_mc_call = mean(payoffs) * exp(-r * T)
precio_mc_call

# --- CÁLCULO TEÓRICO (BLACK-SCHOLES) PARA VALIDACIÓN --- 
d1 = (log(S_0 / K) + (r + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
d2 = d1 - sigma * sqrt(T)

precio_bs_call = S_0 * pnorm(d1) - K * exp(-r * T) * pnorm(d2)
precio_bs_call

# --- RESULTADOS ---
cat ("------------------------------------------------\n")
cat ("Parámetros_ S_0=", S_0, " K=", K, " T=", T, " r=", r, " sigma=", sigma, "\n")
cat ("------------------------------------------------\n")
cat ("Precio Teórico (Black-Scholes):", precio_bs_call, "\n")
cat ("Precio Estimado (Montecarlo):", precio_mc_call, "\n")
cat("Error de Estimación:", abs (precio_mc_call - precio_bs_call), "\n")
cat ("------------------------------------------------\n")

# --- GRÁFICO 1: CONVERGENCIA DEL ESTIMADOR ---

# Calculamos el precio promedio evolutivo (simulación 1, promedio 1-2, promedio 1-3, ...)
precios_acumulados = cumsum (payoffs) / (1:M_paths) * exp(-r * T)

# Intervalos de Confianza
sd_total = sd (payoffs * exp(-r * T))
error_estandar_evolutivo = sd_total / sqrt(1:M_paths)
ic_superior = precios_acumulados + 1.96 * error_estandar_evolutivo # 95% Confianza
ic_inferior = precios_acumulados - 1.96 * error_estandar_evolutivo

# Definimos un rango para dibujar la gráfica (no pintamos el millón entero para no saturar el gráfico)
pasos_plot = seq (1000, M_paths, by=1000)

# Generamos el plot
plot (pasos_plot, precios_acumulados[pasos_plot], type = "n",
      main = "Convergencia de Montecarlo vs Black-Scholes", 
      xlab = "Número de Simulaciones (M)", 
      ylab = "Precio de la Opción Call", 
      ylim = c(precio_bs_call - 0.5, precio_bs_call + 0.5), 
      xaxt = "n")

grid (col = "gray90", lty = "dotted")
axis (1, at = seq (0, M_paths, by = 200000), 
      labels = c("0", "200k", "400k", "600k", "800k", "1M"))
axis (2)

polygon (c(pasos_plot, rev (pasos_plot)), 
         c(ic_superior [pasos_plot], rev (ic_inferior [pasos_plot])), 
         col = rgb (0.8, 0.8, 0.8, 0.5), 
         border = NA)

abline (h = precio_bs_call, col = "red", lwd = 1, lty = 2)

lines (pasos_plot, precios_acumulados [pasos_plot], col = "blue", lwd = 1)

legend ("topright", legend = c("Estimación Montecarlo", "Valor Teórico (BS)", "IC 95%"), 
        col = c("blue", "red", "gray80"), lty = c(1, 2, 1), lwd = c(1, 2, 10), 
        bg = "white", box.col = "gray")

