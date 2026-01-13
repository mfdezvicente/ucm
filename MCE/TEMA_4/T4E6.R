
datos <- read.table("scoredata.txt")
head(datos)



#a)
nota_est <- datos[,5]
n <- dim(datos)[1] # Números de alumnos 
p <- dim(datos) [2] # Números de asignaturas

names(datos)
nota_est <- datos[,5] 

varianza <- function(x){
var(x)*(length(x) -1)/length(x)
}

mean(nota_est)
median(nota_est)
varianza(nota_est)

#a)

nota_est <- datos[,5]

set.seed(1)
B <- 100
mediab <- numeric(B)
medianab <- numeric(B)
varianzab <- numeric(B)

for (b in 1:B){
nota_estb <- sample(nota_est, replace= TRUE)
mediab[b] <- mean(nota_estb)
medianab[b] <- median(nota_estb)
varianzab[b] <- varianza(nota_estb)
}

sd(mediab)
sd(medianab)
sd(varianzab)


par (mfrow=c(1,3))
hist(mediab)
hist(medianab)
hist(varianzab)


#b)
B <- 50

set.seed(1)

cov(datos)
lambda <- eigen(cov(datos))$values
theta_hat <- lambda[1]/sum(lambda)
theta_hat


theta_hatb <- numeric(B)
for (b in 1:B){
	indb<-(sample(1:n), replace = TRUE)
	datosb <- datos[indb,] 
	theta_hatb <- eigen(cov(datosb))$values[1]/sum(eigen(cov(datosb))$values[1])

}
hist(theta_hatb)
sd(theta_hatb)