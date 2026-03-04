#PRÁCTICA 2.2
rm(list = ls())
library(readxl)
datos = read_excel("ucm/Econometría_Aplicada/ENTREGA_2/PrÃ¡ctica 2_2 Datos fertil2.xls")
#1) Objetivo: Estudiar el efecto de la educación sobre la fertilidad
datos
#Para ver si metemos la edad al cuadrado (los efectos cuadráticos) representamos el gráfico de dispersión children y educ

plot(datos$age, datos$children, pch=19)
#son variables de conteo, no continuas

modelo1=lm(children~age,data=datos) # Creamos una regresión para ponerlo en el gráfico
abline(modelo1,col="red",lwd=4)
#se ve que al hacer la regresión entre el número de hijos y la edad, 
#por encima de la línea regresora hay una especie de curva

#2) Variables
   #Explicativas: 
    #1) Educación
    #2) Edad
    #3) Edad al cuadrado

  #A explicar:
    #Fertilidad (número de hijos)

#3) Tamaño muestral
n=length(datos$children)
n #4361

#4) Modelo

  #A)
    #1) Especificación: children=b0 + b1educ+ b2age+ b3age^2 + u
    #2) Estimación de los coeficientes:
      modelo2=lm(children~educ+age+I(age^2),data=datos)
      summary(modelo2)
      #Son todos significativos por lo que no habría problemas de multicolinealidad
      #Interpretación: a más años de educación menos hijos, cada año, el número de hijos disminuye -0.09
      #Ceteris paribus
      
      #ENDOGENEIDAD POR VARIABLE OMITIDA
      
      #La cov(educ,u) no es 0, entonces añadimos una variable instrumental
      #Buscamos los instrumentos:
      #grado de urbanización tiene cov distinta de 0 con educ pero no en los niños, posible instrumento
      #URBAN
      #otro que puede serlo es la variable si nace en la primera mitad del año o la segunda
      #FRSTHALF

      #B) Mínimos cuadrados bietápicos (MC2E)

      #el hecho de nacer en el segundo trimestre significa que vas a tener menos educación pero no significa q vaya a haber más hijos
      
      #Regresión de la primera etapa(RI) 
      #Especificación: children=b0+b1*educ+b2*age+b3*(age^2)+u
      #educ=p0+p1*frsthalf+p2*age+p3*(age^2)+v
      #Estimación:
      modelo_1e=lm(educ~frsthalf+age+I(age^2),data=datos)
      summary(modelo_1e)
      # el coeficiente de first half es significativo por lo q se cumple q la
      #cov(frsthalf,educ) es distinto de cero
      #F=t^2 es aprox 49 que es mayor de 10, instrumento fuerte y relevante
      
      #Mínimos Cuadrados Bietápicos
      library("AER")
      modelo_2e=ivreg(children~educ+age+I(age^2)|frsthalf+age+I(age^2),data=datos)
      summary(modelo_2e,diagnostics='T')
      #Validación:
      #No se rechaza la hipótesis nula, q era exogeneidad
      #Wu-Hausman, 0.118>0.05, educ es exógena
      
      #hemos sospechado q cov(educ,u) distinto de 0
      #pero hemos llegado a q la variable es exógena,no es buen modelo


      #modelo bueno el de MCO, modelo2 sobre el modelo_2e
      
      #C)
      #Probamos doble instrumento
      #Especificación: children=b0+b1*educ+b2*age+b3*(age^2)+u
      #Primera etapa: educ=pi0+pi1*frsthalf+pi2*urban+pi3*age+pi4*age^2+v
      # CON DOS INSTRUMENTOS
      #Regresión de la primera etapa (RI)
      #Estimación:
      modelo_1e2=lm(educ~frsthalf+urban+age+I(age^2),data=datos)
      summary(modelo_1e2) #salen significativos los coeficientes instrumentales

      #Regla de oro
      library(car)
      linearHypothesis(modelo_1e2,c("frsthalf=0","urban=0"))
      #El F conjunto de los instrumentos es mayor que 10, por lo que
      #los instrumentos son fuertes y relevantes
      
      modelo_2e2=ivreg(children~educ+age+I(age^2)|frsthalf+urban+age+I(age^2), data=datos)
      summary(modelo_2e2,diagnostics='T')
      #ahora educ endógena así que bien, nos quedamos con este modelo
      #“No se rechaza la hipótesis nula del test de sobreidentificación (Sargan/Hansen), por lo que no encontramos evidencia de que los instrumentos sean inválidos. Aun así, el test no prueba validez; solo indica compatibilidad con las restricciones de exclusión.”




