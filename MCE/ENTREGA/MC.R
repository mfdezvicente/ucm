S0 <- 800
K <- 800
T <- 5
r <- 0.02
sigma <- 0.11

# Parámetros para la simulación de trayectorias

set.seed (1)
M_paths <- 1000000
N_steps <- 15
dt = T / N_steps

S_t = matrix (nrow=M_paths, ncol=N_steps+1)
S_t [,1] = S0

for (j in 2:(N_steps+1)){
  Z=rnorm(M_paths)
  S_t[, j] = S_t[, j-1] * exp((r-0.5*sigma^2)*dt + sigma*sqrt(dt)*Z)
}



time_vec = seq (0, T, dt)
matplot (time_vec, t(S_t),
         type='l', 
         lty=1, 
         col = rainbow(M_paths), 
         main = "Simulación por MonteCarlo de S(t)",
         xlab = "Tiempo (t)",
         ylab = "Precio del Activo S(t)")
grid()


# Precio de la opción por Black-Scholes
d1 = (1/(sigma*sqrt(T)))*log(S0/(K*exp(-r*T)))+sigma*sqrt(T)/2
d2 = d1-sigma*sqrt(T)

precio_bs_call = S0*pnorm(d1) - K*exp(-r*T)*pnorm(d2)
precio_bs_call

# Precio de la opción por MonteCarlo

payoffs = pmax(S_t[,N_steps+1]-K, 0)

precio_mc_call = mean(payoffs)*exp(-r*T)
precio_mc_call