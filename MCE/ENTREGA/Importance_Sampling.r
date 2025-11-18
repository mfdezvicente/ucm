# --- PARÁMETROS INICIALES ---
S0 <- 800
K <- 1200  
T <- 5
r <- 0.02
sigma <- 0.11
M_paths <- 1000000 # Un millón de simulaciones

# --- 1. MONTECARLO ESTÁNDAR (CRUDO) ---
# Simulamos Z ~ N(0,1)
set.seed(1)
Z_crudo <- rnorm(M_paths, mean = 0, sd = 1)

# Fórmula cerrada para ST (solución de la EDO de Black-Scholes)
ST_crudo <- S0 * exp((r - 0.5 * sigma^2) * T + sigma * sqrt(T) * Z_crudo)

# Payoff y Precio
payoff_crudo <- pmax(ST_crudo - K, 0)
precio_mc_crudo <- mean(payoff_crudo) * exp(-r * T)
var_crudo <- var(payoff_crudo * exp(-r * T)) # Varianza del estimador

# --- 2. MUESTREO POR IMPORTANCIA (IMPORTANCE SAMPLING) ---

# Paso clave: Definir 'mu'. ¿Cuánto queremos empujar la simulación?
# Queremos que la media de log(ST) se acerque a log(K).
# Esto fuerza a que muchas simulaciones acaben cerca del Strike.
mu_shift <- (log(K / S0) - (r - 0.5 * sigma^2) * T) / (sigma * sqrt(T))

# Simulamos Z* ~ N(mu, 1)
set.seed(1)
Z_is <- rnorm(M_paths, mean = mu_shift, sd = 1) 

# Calculamos ST con las Z desplazadas
ST_is <- S0 * exp((r - 0.5 * sigma^2) * T + sigma * sqrt(T) * Z_is)

# CÁLCULO DEL PESO (Radon-Nikodym)
# W = f(x) / g(x) -> Corrección por haber cambiado la distribución
weights <- exp(-mu_shift * Z_is + 0.5 * mu_shift^2)

# Payoff ponderado
payoff_is <- pmax(ST_is - K, 0) * weights
precio_mc_is <- mean(payoff_is) * exp(-r * T)
var_is <- var(payoff_is * exp(-r * T))

# --- 3. COMPARACIÓN DE RESULTADOS ---

# Precio Black-Scholes (Benchmark exacto)
d1 <- (log(S0/K) + (r + sigma^2/2)*T) / (sigma*sqrt(T))
d2 <- d1 - sigma*sqrt(T)
precio_bs <- S0*pnorm(d1) - K*exp(-r*T)*pnorm(d2)

cat("Resultados Comparativos:\n")
cat("------------------------------------------------\n")
cat("Precio Black-Scholes (Teórico):", round(precio_bs, 4), "\n")
cat("Precio MC Estándar:            ", round(precio_mc_crudo, 4), "\n")
cat("Precio MC Importance Sampling: ", round(precio_mc_is, 4), "\n\n")

cat("Análisis de Eficiencia (Varianza):\n")
cat("------------------------------------------------\n")
cat("Varianza MC Estándar:          ", var_crudo, "\n")
cat("Varianza MC IS:                ", var_is, "\n")
ratio_reduccion <- var_crudo / var_is
cat("Factor de Reducción de Varianza:", round(ratio_reduccion, 2), "veces mejor\n")

# Intervalos de Confianza (al 95%)
error_std_crudo <- sqrt(var_crudo / M_paths)
error_std_is <- sqrt(var_is / M_paths)

cat("\nPrecisión (Ancho del intervalo de confianza):\n")
cat("Error Estándar (Crudo):", round(error_std_crudo, 5), "\n")
cat("Error Estándar (IS):   ", round(error_std_is, 5), "\n")


# --- 4. VISUALIZACIÓN  ---

library(ggplot2)


df_plot <- data.frame(
  S_T = c(ST_crudo, ST_is),
  Metodo = factor(rep(c("Standard MC", "Importance Sampling"), each = M_paths))
)

# 2. Definición de límites para el Zoom (Focus Area)
# Nos centramos alrededor del Strike para ver el detalle de las colas
x_min <- K * 0.5
x_max <- K * 1.5


g <- ggplot(df_plot, aes(x = S_T, fill = Metodo, color = Metodo)) +
  
  # Geometría de Densidad (con transparencia alpha para ver el solapamiento)
  geom_density(alpha = 0.4, size = 0.8) +
  
  # Línea vertical del Strike
  geom_vline(xintercept = K, linetype = "dashed", color = "red", size = 1) +
  
  # Anotación de texto para el Strike
  annotate("text", x = K, y = 0, label = paste(" Strike K =", K), 
           vjust = -1, hjust = -0.1, color = "red", angle = 90, fontface = "bold") +
  
  scale_fill_manual(values = c("Importance Sampling" = "#004c99", "Standard MC" = "#999999")) +
  scale_color_manual(values = c("Importance Sampling" = "#004c99", "Standard MC" = "#666666")) +
  
  # Control de ejes y zoom (coord_cartesian no elimina datos, solo hace zoom)
  coord_cartesian(xlim = c(x_min, x_max)) +
  
  # Etiquetas y Títulos
  labs(
    title = "Distribución de Precios a Vencimiento S(T)",
    subtitle = "Comparativa: Montecarlo Estándar vs. Muestreo por Importancia",
    x = "Precio del Activo S(T)",
    y = "Densidad de Probabilidad",
    caption = paste("Simulaciones:", format(M_paths, big.mark=","), "| Tarea 4")
  ) +
  
  # Tema limpio 
  theme_minimal() +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40"),
    axis.text = element_text(color = "black")
  )

# 4. Mostrar el gráfico
print(g)


# --- VARIANZA (Visualización de Payoffs) ---

library(gridExtra)

# Vector 1: Contribuciones del MC Estándar
contrib_crudo <- pmax(ST_crudo - K, 0) * exp(-r*T)

# Vector 2: Contribuciones del Importance Sampling
contrib_is <- pmax(ST_is - K, 0) * weights * exp(-r*T)

# B) GRÁFICO 1: "El Código de Barras" (Primeras 500 simulaciones)

N_muestra <- 500
df_muestra <- data.frame(
  Simulacion = rep(1:N_muestra, 2),
  Valor = c(contrib_crudo[1:N_muestra], contrib_is[1:N_muestra]),
  Metodo = rep(c("Standard MC", "Importance Sampling"), each = N_muestra)
)

g1 <- ggplot(df_muestra, aes(x = Simulacion, y = Valor, color = Metodo)) +
  geom_point(alpha = 0.8, size = 1.8) +
  geom_hline(yintercept = precio_bs, linetype = "dashed", color = "black") +
  annotate("text", x = 1, y = precio_bs + 5, label = "Precio Real", 
           hjust = 0, size = 3) +
  facet_wrap(~Metodo, ncol = 1, scales = "free_y") +
  theme_bw() +
  labs(title = "Comparativa de Muestras Individuales (Primeras 500 simulaciones)",
       subtitle = "Standard MC (abajo): ceros y picos descontrolados.  
Importance Sampling (arriba): valores estables cerca del precio real.",
       y = "Contribución al Precio (Payoff Descontado * Peso)") +
  theme(legend.position = "none")


# --- MOSTRAR g1 EN UNA NUEVA VENTANA ---
if (.Platform$OS.type == "windows") windows() else 
  if (Sys.info()["sysname"] == "Darwin") quartz() else x11()

print(g1)
# ---------------------------------------------
