n=5
p=0.6
set.seed(1)
E=0
FF=0

while(E<5){
u=runif(1)
if(u<p){
E=E+1
}else{
FF=FF+1
}
}#end de while
FF

#Vamos a generar n=100 valores
n=100
x=numeric()
while(length(x)<n){
E=0
FF=0

while(E<5){
u=runif(1)
if(u<p){
E=E+1
}else{
FF=FF+1
}
}#end de while
x=c(x,FF)
}

table(x)
chisq.test()



