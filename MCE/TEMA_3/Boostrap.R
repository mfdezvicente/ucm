# VAMOS A COGER EL EJEMPLO FAITHFUL DE R

head(faithful)
x<- faithful$eruptions
hist(x)
median(x)

B = 10000
set.seed(1)
vmediana <- numeric(B)

for(b in 1:B){
	xn = sample(x,replace=TRUE)
	vmediana[b]= median (xn)

}

hist(vmediana, col = "skyblue", lwd = 2, main= "Distriución de la mediana por Boostrap")
