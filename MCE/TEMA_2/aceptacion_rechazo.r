# MÉTODO DE ACEPTACIÓN Y RECHAZO
curve(dbeta(x,3,1), xlim= c(-0.5,1.5))
#curve(dunif(x,0,1), add=TRUE, col="red")
c=3
curve(c* dunif(x,0,1), add=TRUE, col="red")