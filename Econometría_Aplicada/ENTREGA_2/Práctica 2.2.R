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





############################################################
# PLANTILLA IV / 2SLS (MC2E) EN R — CHECKLIST COMENTADA
# Qué pensar al proponer un instrumento z, y qué mirar en R
############################################################

# 0) Limpieza y paquetes
rm(list = ls())

# install.packages(c("AER","car","sandwich","lmtest"))  # si te faltan
library(AER)       # ivreg()
library(car)       # linearHypothesis() para F conjunto
library(sandwich)  # vcovHC(), vcovCL()
library(lmtest)    # coeftest()

############################################################
# 1) Define el problema (cambia nombres)
#
# Ecuación estructural (la "causal"):
#   y = beta0 + beta1 * x + gamma' W + u
#   - x: ENDÓGENA (sospechas cov(x,u) != 0)
#   - W: controles EXÓGENOS
#
# Instrumentos:
#   - Zx: instrumentos EXCLUIDOS (NO entran en la ecuación estructural)
#         pero deben explicar x y ser exógenos respecto a u.
#
# Regla mental:
#   (A) Relevancia (inclusión): Zx explica x dado W
#   (B) Validez (exclusión): Zx NO afecta y salvo vía x  (cov(Zx,u)=0)
############################################################

# Ejemplo de nombres (ajusta a tu dataset):
# datos <- read.csv("...")

# y  <- "children"
# x  <- "educ"
# W  <- c("age", "I(age^2)")   # controles exógenos
# Zx <- c("frsthalf", "urban") # instrumentos excluidos


############################################################
# 2) Paso 0: OLS (referencia)
#   Esto NO arregla endogeneidad, pero te da baseline.
############################################################

# modelo_ols <- lm(children ~ educ + age + I(age^2), data = datos)
# summary(modelo_ols)

# Nota: No uses Jarque-Bera como prueba de endogeneidad.
# La endogeneidad es cov(x,u) != 0, no "residuos no normales".


############################################################
# 3) CONDICIÓN 1 — RELEVANCIA (restricción de inclusión)
#   "¿El instrumento mueve x, controlando por W?"
#
# Primera etapa:
#   x = pi0 + pi1*Zx + delta'W + v
#
# Lo que miras:
#   - Significatividad de los instrumentos en 1ª etapa
#   - F de instrumentos excluidos (regla práctica: F > 10)
############################################################

# 3.1) Primera etapa (ejemplo con 2 instrumentos excluidos)
# primera_etapa <- lm(educ ~ frsthalf + urban + age + I(age^2), data = datos)
# summary(primera_etapa)

# 3.2) F conjunto de instrumentos excluidos: H0: frsthalf=0 y urban=0
# linearHypothesis(primera_etapa, c("frsthalf = 0", "urban = 0"))

# Si tienes SOLO 1 instrumento excluido (ej. frsthalf),
# el F de instrumentos excluidos coincide con t^2 aproximadamente,
# pero mejor decirlo como "F de primera etapa" igualmente.


############################################################
# 4) CONDICIÓN 2 — VALIDEZ (restricción de exclusión / exogeneidad)
#   "¿Zx está incorrelado con u? ¿Afecta a y solo a través de x?"
#
# Esto NO se observa en datos: se justifica con argumento económico.
#
# Estadísticamente:
#   - Si el modelo está SOBREIDENTIFICADO (más instrumentos que endógenas),
#     puedes hacer un test de sobreidentificación (Sargan/Hansen).
#   - Si está EXACTAMENTE identificado, NO puedes testear validez con Sargan/Hansen.
############################################################


############################################################
# 5) Estimación IV / 2SLS (MC2E) con ivreg()
#
# Sintaxis clave:
#   ivreg( y ~ x + W | Zx + W )
#
# A la izquierda del |: ecuación estructural
# A la derecha del |: TODOS los instrumentos (excluidos Zx + exógenos W)
############################################################

# modelo_iv <- ivreg(children ~ educ + age + I(age^2) |
#                    frsthalf + urban + age + I(age^2),
#                    data = datos)

# Diagnósticos útiles (pon TRUE, no 'T'):
# summary(modelo_iv, diagnostics = TRUE)

# Qué significa lo que suele salir:
# - Weak instruments: fuerza de la 1ª etapa (si es bajo => instrumento débil)
# - Wu-Hausman: H0: x es exógena (si NO rechazas, OLS podría bastar)
# - Sargan (si sobreidentificado): H0: instrumentos válidos (no prueba validez;
#   si rechazas, al menos uno es inválido)


############################################################
# 6) Errores estándar robustos (recomendado)
############################################################

# Robustos a heterocedasticidad:
# coeftest(modelo_iv, vcov. = vcovHC(modelo_iv, type = "HC1"))

# Si hay clusters (ej. provincia):
# cl <- datos$provincia
# coeftest(modelo_iv, vcov. = vcovCL(modelo_iv, cluster = cl, type = "HC1"))


############################################################
# 7) Decisión final: ¿me quedo con OLS o con IV?
#
# (i) Si 1ª etapa es débil (F bajo): IV es impreciso y poco fiable.
# (ii) Si Wu-Hausman rechaza exogeneidad de x: IV tiene más sentido.
# (iii) Si Sargan/Hansen rechaza: hay sospecha de instrumentos inválidos.
#
# Frase para informe:
# - "Instrumentos fuertes" -> F de excluidos alto.
# - "Endogeneidad" -> Wu-Hausman.
# - "Validez conjunta (si sobreidentificado)" -> Sargan/Hansen.
############################################################


############################################################
# 8) (Extra) Caso con DOS endógenas: y2 y y3
#
# Estructural: y1 = ... + b1*y2 + b2*y3 + W + u
# Necesitas al menos tantos instrumentos excluidos como endógenas (>=2).
#
# En 2SLS, las primeras etapas conceptuales son:
#   y2 ~ Z + W
#   y3 ~ Z + W
# donde Z incluye instrumentos excluidos y W los exógenos del modelo.
############################################################