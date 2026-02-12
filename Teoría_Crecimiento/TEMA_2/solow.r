# =========================
# Práctica TCE: Solow
# =========================

# Parámetros
s     <- 0.25
delta <- 0.10
A     <- 1
n     <- 0.02
alpha <- 0.33

# -------------------------
# A) k* estado estacionario
# -------------------------
k_star <- ((s*A)/(delta+n))^(1/(1-alpha))
k_star

# -------------------------
# B) k* regla de oro
# -------------------------
s_oro <- alpha
k_star_oro <- ((s_oro*A)/(delta+n))^(1/(1-alpha))
k_star_oro

# -------------------------
# C) Gráfico: y, (δ+n)k, s y, s_oro y
# -------------------------
kgrid <- seq(0.01, 10, by=0.01)

y1 <- A * kgrid^alpha
y2 <- (delta+n) * kgrid
y3 <- s     * A * kgrid^alpha
y4 <- s_oro * A * kgrid^alpha

plot(kgrid, y1, type="l", lwd=2, col="blue",
     ylim=c(0, 2), xlab="k", ylab="",
     main="Solow: y, (δ+n)k, s·y y s_oro·y")
lines(kgrid, y2, lwd=2, col="red")
lines(kgrid, y3, lwd=2, col="darkgreen")
lines(kgrid, y4, lwd=2, col="orange")
legend("topright",
       legend=c("y = A k^alpha", "(δ+n)k", "s·y", "s_oro·y"),
       col=c("blue","red","darkgreen","orange"),
       lty=1, lwd=2, bty="n")

# Comentario C:
# Al subir s de 0.25 a s_oro=0.33, la curva de ahorro s·y se desplaza hacia arriba (gira hacia arriba).

# -------------------------
# D) Cambio permanente en t=10: s pasa a s_oro
# Trayectorias exactas (por solución cerrada en el continuo):
# Usamos x(t)=k(t)^(1-alpha), que sigue una ecuación lineal.
# -------------------------

Tmax <- 100
t <- 0:Tmax
t_change <- 10

# Economía en t=0 está en el estado estacionario inicial
k0 <- k_star

# Función: trayectoria exacta de k(t) tras el cambio
k_path <- numeric(length(t))

# Antes del cambio: constante en k_star
k_path[t <= t_change] <- k0

# Después del cambio:
# x(t) = k(t)^(1-alpha)
# x(t) = x*_new + (x(t_change) - x*_new) * exp(-(1-alpha)(δ+n)(t-t_change))
x_change <- k0^(1-alpha)
x_star_new <- (s_oro*A)/(delta+n)   # porque x* = k*^(1-alpha) = sA/(δ+n)

idx_after <- which(t > t_change)
tau <- t[idx_after] - t_change

x_after <- x_star_new + (x_change - x_star_new) * exp(-(1-alpha)*(delta+n)*tau)
k_path[idx_after] <- x_after^(1/(1-alpha))

# Trayectorias de y y c
y_path <- A * k_path^alpha
c_path <- (1 - ifelse(t <= t_change, s, s_oro)) * A * k_path^alpha

# Tasa de crecimiento de k (aprox discreta): g_k(t) = (k_t - k_{t-1})/k_{t-1}
gk <- rep(NA, length(t))
gk[-1] <- (k_path[-1] - k_path[-length(t)]) / k_path[-length(t)]

# -------------------------
# Gráficos D
# -------------------------
par(mfrow=c(2,2))

plot(t, k_path, type="l", lwd=2, col="blue",
     xlab="Tiempo", ylab="k(t)", main="Trayectoria de k(t)")
abline(v=t_change, lty=2)

plot(t, y_path, type="l", lwd=2, col="blue",
     xlab="Tiempo", ylab="y(t)", main="Trayectoria de y(t)")
abline(v=t_change, lty=2)

plot(t, c_path, type="l", lwd=2, col="blue",
     xlab="Tiempo", ylab="c(t)", main="Trayectoria de c(t)")
abline(v=t_change, lty=2)

plot(t, gk, type="l", lwd=2, col="blue",
     xlab="Tiempo", ylab="g_k(t)", main="Crecimiento de k en transición")
abline(h=0, lty=3)
abline(v=t_change, lty=2)

par(mfrow=c(1,1))
