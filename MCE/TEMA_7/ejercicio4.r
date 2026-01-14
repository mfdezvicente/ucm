# ===== Ajustes =====
n     = 10
alpha = 0.05
MC    = 10000
B     = 1000     # mejor 1000 que 100

sigma0 = sqrt(0.5)   # H0: var = 0.5

# ========= (1) NIVEL: P(rechazar H0 | H0 verdadera) =========
set.seed(1)
pval_nivel = numeric(MC)
for (i in 1:MC){

  # datos bajo H0
  x = rnorm(n, mean = 0, sd = sigma0)

  # estadístico observado (bilateral)
  tobs = var(x) - 0.5

  # bootstrap bajo H0: recentrar y reescalar para que var sea 0.5
  x0 = (x - mean(x)) * sigma0 / sd(x)

  T = numeric(B)
  for (b in 1:B){
    xb = sample(x0, size = n, replace = TRUE)
    T[b] = var(xb) - 0.5
  }

  # p-valor bootstrap bilateral (con corrección +1 para evitar 0)
  pval_nivel[i] = (sum(abs(T) >= abs(tobs)) + 1) / (B + 1)
}

nivel_est = mean(pval_nivel < alpha)
nivel_est


# ========= (2) POTENCIA: P(rechazar H0 | alternativa) =========
# Elige una alternativa. Ejemplo: sd = 2.2  -> var = 4.84
sigma1 = 2.2

set.seed(2)
pval_pot = numeric(MC)

for (i in 1:MC){

  # datos bajo H1
  x = rnorm(n, mean = 0, sd = sigma1)

  # estadístico observado
  tobs = var(x) - 0.5

  # para calcular p-valor, NECESITAS distribución bajo H0 -> recentrar + reescalar a var 0.5
  x0 = (x - mean(x)) * sigma0 / sd(x)

  T = numeric(B)
  for (b in 1:B){
    xb = sample(x0, size = n, replace = TRUE)
    T[b] = var(xb) - 0.5
  }

  pval_pot[i] = (sum(abs(T) >= abs(tobs)) + 1) / (B + 1)
}

potencia_est = mean(pval_pot < alpha)
potencia_est
