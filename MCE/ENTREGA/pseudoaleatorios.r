############################################################
# 1. Paquetes necesarios
############################################################
if(!require(randtoolbox)) install.packages("randtoolbox")
if(!require(randtests)) install.packages("randtests")
if(!require(ggplot2)) install.packages("ggplot2")

library(randtoolbox)
library(randtests)
library(ggplot2)

############################################################
# 2. Parámetros del modelo de VaR
############################################################

S0    <- 100
mu    <- 0.0005
sigma <- 0.01

N_sim <- 10000   # Número de simulaciones internas (escenarios)
N_rep <- 100     # Número de repeticiones para medir la estabilidad

seed_0 <- 1234   # Semilla base reproducible

############################################################
# 3. Generadores de números pseudoaleatorios
############################################################

# 3.1 LCG (Linear Congruential Generator)
# Nota: Mantenemos la lógica explicita por motivos didácticos.
lcg_unif <- function(n, seed, a = 16807, m = 2^31 - 1) {
  # Evitamos semilla 0
  if (seed %% m == 0) seed <- 12345 
  
  x <- numeric(n)
  x[1] <- seed %% m
  
  # Bucle simple (Nota: Lento en R puro, pero ilustrativo)
  for (k in 2:n) {
    # Usamos as.numeric para evitar overflow de enteros antes del módulo
    val <- (as.numeric(a) * x[k - 1])
    x[k] <- val %% m
  }
  return(x / m)
}

# 3.2 Mersenne Twister (Estándar en R)
mt_unif <- function(n, seed = NULL) {
  RNGkind("Mersenne-Twister")
  if (!is.null(seed)) set.seed(seed)
  return(runif(n))
}

# 3.3 Secuencia Sobol (Randomized QMC)
# CORRECCIÓN CRÍTICA: scrambling = 1 (Owen scrambling)
# Esto permite que 'set.seed' afecte la secuencia, permitiendo medir varianza.
# 3.3 Secuencia Sobol con Random Shift (RQMC Manual)
sobol_unif <- function(n, seed = NULL) {
  # 1. Fijar semilla en R para controlar el "shift" aleatorio
  if (!is.null(seed)) set.seed(seed)
  
  # 2. Generar un salto (shift) aleatorio uniforme
  # Esto asegura que cada repetición tenga un punto de partida distinto
  shift <- runif(1)
  
  # 3. Generar Sobol determinista base (scrambling=0, init=TRUE para reiniciar)
  # Usamos la secuencia pura, siempre idéntica antes del shift
  u_base <- randtoolbox::sobol(n = n, dim = 1, scrambling = 0, init = TRUE)
  
  # Si devuelve matriz, aseguramos vector numérico
  if (is.matrix(u_base)) u_base <- u_base[, 1]
  
  # 4. Aplicar la rotación de Cranley-Patterson (Suma modular)
  # (u + shift) %% 1 mantiene los puntos en [0,1]
  u_final <- (u_base + shift) %% 1
  
  return(as.numeric(u_final))
}

############################################################
# 4. Limpieza de extremos (Safety check)
############################################################

clip_unit_interval <- function(u, eps = 1e-12) {
  # Evita qnorm(0) = -Inf o qnorm(1) = Inf
  u <- pmax(u, eps)
  u <- pmin(u, 1 - eps)
  return(u)
}

############################################################
# 5. Función de simulación de VaR (Optimizada)
############################################################

simulate_var_for_generator <- function(gen_name, gen_fun,
                                       N_sim, N_rep,
                                       S0, mu, sigma,
                                       seed_base = 1234) {
  
  # CORRECCIÓN: Usamos lapply en lugar de bucle for + rbind para eficiencia
  results_list <- lapply(1:N_rep, function(r) {
    
    seed_r <- seed_base + r
    
    # 1) Generar U(0,1)
    U <- gen_fun(N_sim, seed_r)
    U <- clip_unit_interval(U)
    
    # 2) Normal estándar
    Z <- qnorm(U)
    
    # 3) Precio simulado (Geometric Brownian Motion)
    # Corrección financiera: S_t = S0 * exp((mu - 0.5*sigma^2) + sigma*Z)
    drift <- mu - 0.5 * sigma^2
    diffusion <- sigma * Z
    S1 <- S0 * exp(drift + diffusion)
    
    # Pérdidas (L > 0 es pérdida)
    L  <- S0 - S1
    
    # 4) VaR (Percentiles empíricos)
    VaR95 <- quantile(L, 0.95, names = FALSE)
    VaR99 <- quantile(L, 0.99, names = FALSE)
    
    # Estructura de retorno
    data.frame(
      generator = rep(gen_name, 2),
      rep       = rep(r, 2),
      alpha     = c(0.95, 0.99),
      VaR       = c(VaR95, VaR99)
    )
  })
  
  # Consolidar resultados
  do.call(rbind, results_list)
}

############################################################
# 6. Calidad estadística
############################################################

quality_tests_for_generator <- function(gen_name, gen_fun,
                                        N_qual = 10000, seed = 999) {
  
  U <- gen_fun(N_qual, seed)
  U <- clip_unit_interval(U)
  
  ## Test Chi-cuadrado (Uniformidad)
  k <- 10
  breaks <- seq(0, 1, length.out = k + 1)
  obs <- table(cut(U, breaks = breaks, include.lowest = TRUE))
  chi <- suppressWarnings(chisq.test(obs))
  
  ## Test de rachas (Independencia)
  # Nota: Sobol fallará esto o dará resultados mixtos porque NO es i.i.d.
  # Es una característica esperada de QMC.
  runs <- runs.test(U)
  
  data.frame(
    generator   = gen_name,
    chi_sq_pval = chi$p.value,
    runs_pval   = runs$p.value
  )
}

############################################################
# 7. EJECUCIÓN DE TODAS LAS SIMULACIONES
############################################################

generators <- list(
  LCG   = lcg_unif,
  MT    = mt_unif,
  Sobol = sobol_unif
)

cat("Iniciando simulación de VaR... \n")

## 7.1 VaR
# Usamos lapply para iterar sobre los generadores
all_results <- do.call(
  rbind,
  lapply(names(generators), function(gname) {
    cat("  -> Procesando generador:", gname, "\n")
    simulate_var_for_generator(
      gen_name = gname,
      gen_fun  = generators[[gname]],
      N_sim    = N_sim,
      N_rep    = N_rep,
      S0       = S0,
      mu       = mu,
      sigma    = sigma,
      seed_base = seed_0
    )
  })
)

############################################################
# 7.2 Resumen de VaR (Tabla Final)
############################################################

# Media del VaR estimado
summary_mean <- aggregate(VaR ~ generator + alpha, data = all_results, FUN = mean)
# Estabilidad (Desviación Típica del estimador)
summary_sd <- aggregate(VaR ~ generator + alpha, data = all_results, FUN = sd)

summary_var <- merge(
  summary_mean,
  summary_sd,
  by = c("generator", "alpha"),
  suffixes = c("_mean", "_sd")
)

summary_var <- summary_var[order(summary_var$generator, summary_var$alpha), ]
row.names(summary_var) <- NULL

print("--- TABLA RESUMEN VAR ---")
print(summary_var)

############################################################
# 7.3 Calidad estadística
############################################################

cat("\nCalculando tests de calidad... \n")
quality_summary <- do.call(
  rbind,
  lapply(names(generators), function(gname) {
    quality_tests_for_generator(
      gen_name = gname,
      gen_fun  = generators[[gname]],
      N_qual   = 10000,
      seed     = seed_0 + 999
    )
  })
)

print("--- TABLA CALIDAD ESTADÍSTICA ---")
print(quality_summary)

############################################################
# 8. GRÁFICOS
############################################################

## =========================================================
## 8.1 Boxplot del VaR por generador y alpha
## =========================================================

p_box <- ggplot(all_results, aes(x = generator, y = VaR, fill = generator)) +
  geom_boxplot(outlier.alpha = 0.3) +
  facet_wrap(~ alpha, scales = "free_y", labeller = label_bquote(alpha == .(alpha))) +
  labs(
    title = "Estabilidad del Estimador VaR (Monte Carlo vs QMC)",
    subtitle = paste("Basado en", N_rep, "repeticiones de", N_sim, "simulaciones"),
    x     = "Generador",
    y     = "VaR Estimado"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# print(p_box) # Descomentar para ver en RStudio
ggsave("boxplot_VaR.png", plot = p_box, width = 9, height = 6, dpi = 200)

## =========================================================
## 8.2 Histograma U(0,1) por generador
## =========================================================

# Generamos una muestra fresca para visualizar
U_LCG   <- lcg_unif(10000, seed = 123)
U_MT    <- mt_unif(10000, seed = 123)
U_Sobol <- sobol_unif(10000, seed = 123) # Ahora con scrambling=1 se verá uniforme pero "desordenado"

df_U <- data.frame(
  U = c(U_LCG, U_MT, U_Sobol),
  generator = factor(rep(c("LCG", "MT", "Sobol"), each = 10000))
)

p_hist <- ggplot(df_U, aes(x = U)) +
  geom_histogram(bins = 50, aes(y = after_stat(density)),
                 color = "white", fill = "#c7ddf3cd", alpha = 0.8) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  facet_wrap(~ generator, ncol = 1) +
  scale_x_continuous(limits = c(0,1), expand = c(0,0)) +
  labs(y = "Densidad", x = "Valor") +
  theme_minimal()

# print(p_hist) # Descomentar para ver en RStudio
ggsave("hist_U_generators.png", plot = p_hist, width = 9, height = 8, dpi = 200)