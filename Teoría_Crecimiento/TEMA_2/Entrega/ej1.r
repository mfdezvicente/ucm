# ==========================================================
# Solow con progreso tecnológico (continuo) + shock en g
# Simulación en tiempo discreto usando Euler para ODE:
#   dk/dt = s k^alpha - (n + g + delta) k
# y reconstrucción per cápita: y_pc = A * y_eff
# ==========================================================

rm(list = ls())

# ---- Parámetros ----
alpha <- 0.33
s     <- 0.20
delta <- 0.05
n     <- 0.01

g1 <- 0.02           # crecimiento tecnológico inicial
g2 <- 0.04           # crecimiento tras el shock

t0 <- 50             # momento del shock
T  <- 200            # horizonte total
dt <- 0.1            # paso de tiempo (más pequeño = más preciso)

# ---- Malla temporal ----
time <- seq(0, T, by = dt)
N    <- length(time)

# ---- Función g(t) ----
g_t <- function(t) ifelse(t < t0, g1, g2)

# ---- Estado estacionario (en unidades eficientes) ----
k_star <- function(g) (s / (n + delta + g))^(1/(1 - alpha))

k1_star <- k_star(g1)
k2_star <- k_star(g2)

# ---- Inicialización ----
k <- numeric(N)
A <- numeric(N)

k[1] <- k1_star   # economía inicialmente en estacionario para g1
A[1] <- 1         # normalización

# ---- Simulación (Euler) ----
for (i in 1:(N-1)) {
  t   <- time[i]
  g   <- g_t(t)

  # ODE en unidades eficientes
  dk  <- s * (k[i]^alpha) - (n + g + delta) * k[i]
  k[i+1] <- k[i] + dt * dk

  # Tecnología (continuo): dA/dt = g A
  A[i+1] <- A[i] * exp(g * dt)
}

# ---- Variables en unidades eficientes ----
y_eff <- k^alpha
c_eff <- (1 - s) * y_eff
i_eff <- s * y_eff

# ---- Variables per cápita (por trabajador) ----
# y_pc = Y/N = A * y_eff ; etc.
y_pc <- A * y_eff
c_pc <- A * c_eff
i_pc <- A * i_eff

# ---- Tasas de crecimiento per cápita (aprox continuo) ----
# (log(x_{t+dt})-log(x_t))/dt
gy_pc <- diff(log(y_pc)) / dt
gc_pc <- diff(log(c_pc)) / dt
gi_pc <- diff(log(i_pc)) / dt

# ==========================================================
# Gráficos
# ==========================================================

op <- par(mfrow = c(2,3), mar = c(4,4,3,1))

# (1) k eficiente
plot(time, k, type="l", lwd=2, xlab="t", ylab="k (eficiente)",
     main="Capital por trabajador eficiente k(t)")
abline(v=t0, lty=2)
abline(h=k1_star, lty=3)
abline(h=k2_star, lty=3)

# (2) y eficiente
plot(time, y_eff, type="l", lwd=2, xlab="t", ylab="y (eficiente)",
     main="Output por trabajador eficiente y(t)")
abline(v=t0, lty=2)

# (3) c eficiente
plot(time, c_eff, type="l", lwd=2, xlab="t", ylab="c (eficiente)",
     main="Consumo por trabajador eficiente c(t)")
abline(v=t0, lty=2)

# (4) log y per cápita
plot(time, log(y_pc), type="l", lwd=2, xlab="t", ylab="log(Y/N)",
     main="log(Output per cápita)")
abline(v=t0, lty=2)

# === Zoom en log(C/N) y dos pendientes en rojo ===

yy <- log(c_pc)

# ventana de zoom (ajústala si quieres)
win <- time >= 45 & time <= 70

# punto de anclaje en t0 (el valor de log(C/N) justo en t0)
i0 <- which.min(abs(time - t0))
y0 <- yy[i0]

# rectas de referencia (en rojo), ambas pasan por (t0, y0)
line_g1 <- y0 + g1 * (time - t0)
line_g2 <- y0 + g2 * (time - t0)

# para que no se recorte, usa el rango de yy en la ventana
# y añade un pequeño margen
yr  <- range(yy[win], line_g1[win], line_g2[win])
pad <- 0.05 * diff(yr)
ylim_zoom <- c(yr[1] - pad, yr[2] + pad)

plot(time[win], yy[win],
     type="l", lwd=2, xlab="t", ylab="log(C/N)",
     main="log(Consumo per cápita) (zoom + rectas g1,g2)",
     ylim=ylim_zoom)
abline(v=t0, lty=2)

# rectas en rojo
lines(time[win], line_g1[win], col="red", lwd=2, lty=2)  # pendiente g1
lines(time[win], line_g2[win], col="red", lwd=2, lty=3)  # pendiente g2

legend("topleft",
       legend=c("log(C/N)", paste0("recta pendiente g1=", g1),
                paste0("recta pendiente g2=", g2)),
       lwd=c(2,2,2), lty=c(1,2,3), col=c("black","red","red"),
       bty="n")




gc_pc <- diff(log(c_pc))/dt
tt <- time[-1]

win2 <- tt >= 45 & tt <= 120
plot(tt[win2], gc_pc[win2],
     type="l", lwd=2, xlab="t", ylab="g_{C/N}(t)",
     main="Crecimiento per cápita del consumo")
abline(v=t0, lty=2)
abline(h=g1, lty=3)
abline(h=g2, lty=3)



# (6) tasa de crecimiento per cápita (y)
plot(time[-1], gy_pc, type="l", lwd=2, xlab="t", ylab="g_{Y/N}(t)",
     main="Crecimiento per cápita (aprox)")
abline(v=t0, lty=2)
abline(h=g1, lty=3)
abline(h=g2, lty=3)

par(op)

# ==========================================================
# Imprimir
# ==========================================================
cat("k* (g1) =", k1_star, "\n")
cat("k* (g2) =", k2_star, "\n")
cat("Nuevo crecimiento per cápita en BGP debe ser g2 =", g2, "\n")
