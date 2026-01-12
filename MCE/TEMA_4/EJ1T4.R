# TEMA 4. EJERCICIO 1
# Datos (22 alturas ordenadas)
class <- c(137,138.5,140,141,142,143.5,145,147,148.5,150,153,154,
           155,156.5,157,158,158.5,159,160.5,161,162,167.5)

n <- length(class)
B <- 100
set.seed(1)

# =========================
# APARTADO A)
# =========================
mediab   <- numeric(B)
medianab <- numeric(B)

# Estadísticos en la muestra original
media_hat   <- mean(class)
mediana_hat <- median(class)

for (b in 1:B){
  classb <- sample(class, replace = TRUE)
  mediab[b]   <- mean(classb)
  medianab[b] <- median(classb)
}

par(mfrow=c(1,2))
hist(mediab,   main="Bootstrap media",   xlab="media*",  col="gray")
abline(v=media_hat, col="red", lwd=2)
hist(medianab, main="Bootstrap mediana", xlab="mediana*",col="gray")
abline(v=mediana_hat, col="red", lwd=2)

# Error estándar bootstrap (SE_boot)
se_media_boot   <- sd(mediab)
se_mediana_boot <- sd(medianab)

se_media_boot
se_mediana_boot

# Referencia: SE teórico aproximado de la media (si fuera i.i.d. y normal-ish)
sd(class)/sqrt(n)

# =========================
# APARTADO B) Sesgo vía bootstrap
# (media muestral y mediana muestral como estimadores de la MEDIA poblacional)
# =========================
# En bootstrap plug-in, la "media poblacional" se aproxima por mean(class)
mu_hat <- mean(class)

bias_media_boot   <- mean(mediab)   - mu_hat
bias_mediana_boot <- mean(medianab) - mu_hat

bias_media_boot
bias_mediana_boot

# (Opcional) también puedes ver el sesgo relativo
bias_media_boot / mu_hat
bias_mediana_boot / mu_hat


# =========================
# APARTADO C) Comparación gráfica de distribuciones (media vs mediana)
# =========================
par(mfrow=c(1,2))

hist(mediab, prob=TRUE, main="Distribución bootstrap de la media",
     xlab="media*", col="gray", border="white")
lines(density(mediab), lwd=2)
abline(v=mu_hat, col="red", lwd=2)         # referencia: mean(class)
abline(v=mean(mediab), col="blue", lwd=2, lty=2)  # centro bootstrap

hist(medianab, prob=TRUE, main="Distribución bootstrap de la mediana",
     xlab="mediana*", col="gray", border="white")
lines(density(medianab), lwd=2)
abline(v=mu_hat, col="red", lwd=2)              # misma referencia (media poblacional)
abline(v=mean(medianab), col="blue", lwd=2, lty=2)

# (Opcional) Boxplot comparativo rápido
par(mfrow=c(1,1))
boxplot(mediab, medianab, names=c("media*", "mediana*"),
        main="Comparación bootstrap: media vs mediana", col="gray")


# =========================
# APARTADO D) (ya lo tenías) + corrección replace
# Varianza (denominador n) y cuasivarianza (denominador n-1)
# =========================

varianza <- function(x){
  # var(x) en R usa (n-1) abajo; esto la convierte a denominador n
  var(x) * (length(x) - 1) / length(x)
}

set.seed(1)
varianzab <- numeric(B)
cuasib    <- numeric(B)

for (b in 1:B){
  classb <- sample(class, replace = TRUE)
  varianzab[b] <- varianza(classb)  # denom n
  cuasib[b]    <- var(classb)       # denom n-1
}

par(mfrow=c(1,2))
hist(varianzab, main="Bootstrap varianza (denom n)", xlab="var*", col="gray")
hist(cuasib,    main="Bootstrap cuasivarianza (denom n-1)", xlab="s2*", col="gray")

# Error estándar bootstrap
se_var_boot   <- sd(varianzab)
se_cuasi_boot <- sd(cuasib)

se_var_boot
se_cuasi_boot


# =========================
# APARTADO E) Sesgo bootstrap (varianza y cuasivarianza) como estimadores de sigma^2
# =========================
# En bootstrap plug-in, la "varianza poblacional" es la varianza con denom n de la muestra original
sigma2_hat <- varianza(class)

bias_var_boot   <- mean(varianzab) - sigma2_hat
bias_cuasi_boot <- mean(cuasib)    - sigma2_hat

bias_var_boot
bias_cuasi_boot

# =========================
# APARTADO F) Comparación gráfica: distribuciones de varianza vs cuasivarianza
# =========================
par(mfrow=c(1,2))

hist(varianzab, prob=TRUE, main="Distribución bootstrap varianza (n)",
     xlab="var*", col="gray", border="white")
lines(density(varianzab), lwd=2)
abline(v=sigma2_hat, col="red", lwd=2)                 # referencia sigma^2 (plug-in)
abline(v=mean(varianzab), col="blue", lwd=2, lty=2)    # centro bootstrap

hist(cuasib, prob=TRUE, main="Distribución bootstrap cuasivarianza (n-1)",
     xlab="s2*", col="gray", border="white")
lines(density(cuasib), lwd=2)
abline(v=sigma2_hat, col="red", lwd=2)                 # misma referencia sigma^2
abline(v=mean(cuasib), col="blue", lwd=2, lty=2)

# Boxplot comparativo
par(mfrow=c(1,1))
boxplot(varianzab, cuasib, names=c("var*(n)", "s2*(n-1)"),
        main="Comparación bootstrap: varianza vs cuasivarianza", col="gray")
abline(h=sigma2_hat, col="red", lwd=2)
