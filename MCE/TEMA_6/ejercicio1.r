# EJERCICIO 1
x <- c(-4,5,-7,10,1)
n = length(x)
mJack=function(x,estadistico){
n=length(x)
Jack=matrix(NA,nrow=n,ncol=(n-1))
for(i in 1:n){
Jack[i,]=x[-i]
}
return(apply(Jack,1,estadistico))
}
# TERMINAMOS EL EJERCICIO