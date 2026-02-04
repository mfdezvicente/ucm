##############
# PRÁCTICA 5 #
##############

#Analizar cómo influye el nivel de estudios en el salario mensual en una región, donde
#se ha seleccionado una muestra de trabajadores mayores de 22 años para los que se
#ha recopilado la siguiente información:
#EDAD: en años.
#SALARIO: salario mensual en euros.
#ESTUDIOS: 1=Básicos, 2=Medios, 3=Superiores


# FASES
# 1) Especificación del modelo.
# En este caso, es una regresión simple.
# 2) Estimación de los coeficientes del modelo.
# 3) Validación del modelo.
# 4) Predicción.

library (readxl)

datos = read_excel("ucm/Econometría_Aplicada/Datos.xlsx")
datos

datos$edad_cuadrado = datos$EDAD**2
datos$medios <- ifelse(datos$ESTUDIOS == 2, 1, 0)
datos$superiores <- ifelse(datos$ESTUDIOS == 3, 1, 0)

modelo = lm(SALARIO~EDAD + edad_cuadrado + medios + superiores, data = datos)
summary(modelo)
# Observamos que tras estimar el modelo por mínimo cuadrados ordinarios, todas los coeficientes de las variables son significativas. 


#3) Validación 
# Procedemos a validar el modelo. Para ello, realizaremos un contraste de heterocedasticidad de BP y un análisis de la normalidad de los residuos por JB. 

# Para el contraste de heterocedasticidad de BP donde H0: Modelo no tiene heterocedasticidad. H1: El modelo presenta heterocedasticidad
library(lmtest)
bptest(modelo)
# Observamos que el p-valor es 2.2e-16. Por tanto hay heterocedasticidad en los residuos. 

datos$ln_salario <- log(datos$SALARIO)

modelo_log <- lm(ln_salario ~ EDAD + edad_cuadrado + medios + superiores, data = datos)
summary(modelo_log)

bptest(modelo_log)

# Sigue habiendo heterocedasticidad con el modelo en log. 

# Vamos a tratar entonces de hacer MCP 
# 1) Modelo base
u2 <- resid(modelo)^2

# 2) Modelar la varianza (una forma simple)
var_mod <- lm(u2 ~ EDAD + edad_cuadrado + medios + superiores, data = datos)

# 3) Pesos = 1 / varianza estimada (evita ceros o negativos)
w <- 1 / pmax(fitted(var_mod), 1e-8)

modelo_wls <- lm(SALARIO ~ EDAD + edad_cuadrado + medios + superiores, data = datos, weights = w)
summary(modelo_wls)
bptest(modelo_wls)
# Sigue habiendo heterocedasticidad con los MCP porque la varianza no está bien especificada. 


# Realizamos un diagnóstico visual 
plot(modelo, which = 1)
plot(modelo_log, which = 1)

# Como no somos capces de corregir esta heterocedasticidad, realizaremos una estimación del modelo mediante:
library(sandwich)
library(lmtest)
coeftest(modelo_log, vcov = vcovHC(modelo_log, type = "HC1"))

# Para comprobar la normalidad de los residuos

library(car)
vif(modelo)

modelo_2 = lm(SALARIO~ EDAD + medios + superiores, data = datos)
summary(modelo)
vif(modelo_2)
bptest(modelo_2)
# Aunque se corrige la multicolinealidad aproximada sigue exisitiendo el problema de la heterocedasticidad. 
# Voy a probar el mismo cambio de quitar la edad_cuadrado para el caso del log_salario

modelo_log_2 <- lm(ln_salario ~ EDAD + medios + superiores, data = datos)
summary(modelo_log_2)

bptest(modelo_log_2)
# Sigue exisitiendo el problema de la heterocedasticidad.
residuos = residuals(modelo_log_2)
jarque.bera.test(residuos)
qqnorm(residuals(modelo_log_2))
qqline(residuals(modelo_log_2, col ="red"))


res_ordenados <- sort(abs(residuos), decreasing = TRUE)
head(res_ordenados)

datos$o217 <- 0
datos$o217[217] <- 1


datos$o253 <- 0
datos$o253[253] <- 1

datos$o490 <- 0
datos$o490[490] <- 1
modelo_log_2_r <- lm(ln_salario ~ EDAD + medios + superiores + o217 + o253 + o490, data = datos)
summary(modelo_log_2_r)

bptest(modelo_log_2_r)
residuos = residuals(modelo_log_2_r)
jarque.bera.test(residuos)
qqnorm(residuals(modelo_log_2_r))
qqline(residuals(modelo_log_2_r, col ="red"))
