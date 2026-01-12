# EJERCICIO 3 - TEMA 4
set.seed(1)
lsat = c(576, 635, 558, 578, 666, 580, 555, 661, 651, 605, 653, 575, 545, 572, 594) 
gpa = c(3.39, 3.3, 2.81, 3.03, 3.44, 3.07, 3, 3.43, 3.36, 3.13, 3.12, 2.74, 2.76, 2.88, 2.96)

B = 100

mean(lsat)
mean(gpa)
cor(lsat, gpa)

vcor = numeric(B)

for (b in 1:B){
indb = sample(1:15, replace = TRUE) # IDEA ES QUE REMUESTRE PERO QUE LA MUESTRA DE LSAT VAYA JUNTO A LA DE GPA
lsat[indb]
gpa[indb]
vcor[b] = cor(lsat[indb], gpa[indb])  
}

sd(vcor) 

mean(vcor) - cor(lsat, gpa) 
mean(vcor) - cov(lsat, gpa)