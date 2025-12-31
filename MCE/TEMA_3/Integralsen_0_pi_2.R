# APROXIMACIÓN POR MC 

set.seed(1)
MC <- 10000 
I_est <- numeric(MC)
I_estVA <- numeric (MC)

n<- 100
for (i in 1:MC) {
	u<- runif(n)
	u_pi<- u*pi/2
	I_est[i] <- mean(sin(u_pi))*pi/2  
	uVA = c(u[1:(n/2)], 1-u[1:(n/2)])
	u_piVA = uVA*pi/2
	I_estVA[i] = mean(sin(u_piVA))*pi/2

}

hist(I_est)
sd(I_est)
sd(I_estVA)

#JUSTIFICACIÓN 

set.seed(1)
MC <- 10000
u <- runif(MC)
cor(sin(u*pi/2), sin((1-u)*pi))