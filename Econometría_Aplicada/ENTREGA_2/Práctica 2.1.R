##############
# PRÁCTICA 1 #
##############

# El objetivo de este análisis es estudiar la relación entre el nivel educativo 
# y los ingresos mensuales de los individuos, basándonos en la teoría del 
# Capital Humano.

# Para ello, vamos a estimar primero un modelo de regresión lineal simple
# tomando la variable wage como variable dependiente y la variable educ como 
# variable explicativa. Empezaremos estimando el modelo por MCO, suponiendo
# exogeneidad. 

library (readxl)
datos = read_excel("C:/Users/Usuario/GitHub/ucm/Econometría_Aplicada/ENTREGA_2/PrÃ¡ctica 2_1 Datos wage2.xls", na=".")

modelo_mco = lm (wage~educ, data=datos) 
summary (modelo_mco)

# Supongamos que cov(educ, u) ≠ 0. Supongamos que es endógena. 

# Entonces hay que incluir una variable instrumento que tiene que incluir dos
# restricciones: una de inclusión y otra de exclusión. 
# Necesitamos una variable que esté relacionada con educ pero que no esté
# relacionado con wage. 

# Restricción de inclusión: El instrumento z debe estar relacionado con x: cov(z,x)≠0
# Restricción de exclusión: El instrumento z no debe estar relacionado con u:cov(z,u)=0

# Usaremos como variable instrumental 'sibs' (nº de hermanos) puesto que, en
# teoría, tener más hermanos suele reducir la educación pero no afecta al
# salario directamente. 

# Verifiquemos la restricción de inclusión: 
modelo_1e = lm(educ~sibs, data=datos)
summary (modelo_1e)

# Como pi_1 es significativo se verifica la restricción de inclusión. 

# Sin embargo, la restricción de exclusión no la podemos comprobar porque el
# modelo está exactamente identificado (mismo número de variables endógenas que 
# de instrumentos). 

# install.packages('AER')
library(AER)

modelo_biet = ivreg(wage ~ educ | sibs, data=datos)
summary (modelo_biet)

# Hemos supuesto que cov(educ,u) ≠ 0, pero no sabemos si es cierto. Para ello, 
# vamos a realizar el test de Hausman. 

summary (modelo_biet, diagnostics='T')

# En la parte de los diagnósticos, podemos apreciar 3 tests diferentes: 
# 1. Weak Instruments: Si F > 10, se verifica la regla de oro y podemos 
# confirmar que el instrumento es fuerte. 
# 2. Wu-Hausman: en este caso, como el p-valor < 0.05, educ es endógena y por
# tanto, la estimación realizada mediante MCO es inconsistente. Se usa MC2E. 
# 3. Sargan: No disponible porque este depende del grado de sobreidentificación 
# y, en este caso, el modelo está perfectamente identificado. 

# A continuación, vamos a proceder a utilizar dos intrumentos (sibs y meduc). 
# En este caso, sí podremos utilizar el test de Sargan. 

modelo_1eb = lm (educ ~ sibs + meduc, data=datos)
summary (modelo_1eb)

modelo_bietb = ivreg(wage ~ educ | sibs + meduc, data=datos)
summary (modelo_bietb, diagnostics='T')

# En este caso, como el p-valor del test de Sargan es mayor que 0.05, los
# instrumentos son válidos (no están correlados con el error). 

# Ahora, vamos a proceder a incluir exper como variable exógena de control. 
# Es importante señalar que las variables exógenas se instrumentan a sí mismas. 
modelo_mcoc = lm(wage ~ educ + exper, data=datos)
summary (modelo_mcoc)

modelo_1ec = lm (educ ~ sibs + meduc + exper, data=datos)
summary (modelo_1ec)

modelo_bietc = ivreg (wage~educ + exper | sibs + meduc + exper, data=datos)
summary (modelo_bietc, diagnostics = 'T')

# Podemos apreciar a través de los test, como los instrumentos sibs y meduc son
# fuertes y explican muy bien la educación (Restricción de Inclusión). Además, 
# se rechaza la hipótesis nula de exogeneidad y por último, por el test de
# Sargan no rechazamos la validez de los instrumentos y ambos cumplen la
# Restricción de Exclusión. 

