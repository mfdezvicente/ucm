# VAMOS A HACERLO PARA EL CASO DE LA MEDIA MUESTRAL. AUNQUE COMO SABEMOS NOO HACE FALTA PQUE SABEMS PAA LA MEDIA MUESTAL SUS EES ESTANDRÇ

sigma= 2 
set.seed(1)
n=15 

muestra = rnorm(n,1,sigma)

#SABEMOS QUE EL ERROR ESTANDA DE LA MEDIA MUESTAL ES LA DESV TÍPICA 
sigma/sqrt(n) # ESTO SERÍA EL VALOR EXACTO

#SI NO CONOCEMOS SIGMA. #TRATAMOS DE ESTIMAR SIGMA CON LOS DATOS

sd(muestra)/sqrt(n)

#SI NO CONOCEMOS LA EXPESIÓN DE LA SD(MEDIA MUESTRAL) RECURRIMOS A BOOTSTRAP
B = 100
mediab = numeric(B)

for (b in 1:B){
	muestrab= sample(muestra, replace = TRUE)
	mediab[b] = mean(muestrab)
}

hist(mediab, col= "skyblue",main = "Distribución de la media muestral mediante Boostrap") #ESTA ES LA DISTRIBUCIÓN DE LA MEDIA MUESTRAL MEDIANTE BOOSTRRAP

sd(mediab)