##############################################################
######## PLANTILLA DE INTERPRETACIÓN DE COEFICIENTES #########
######## EXAMEN PRÁCTICO ECONOMETRÍA - TEMAS 2,3,4,5,6 #######
##############################################################

# ============================================================
# OBJETIVO DE ESTA PLANTILLA
# ============================================================
# Esta plantilla complementa la plantilla principal de código.
# Su objetivo es que, cuando tengas un output en el examen, sepas:
#   1) Qué SÍ puedes decir de cada coeficiente.
#   2) Qué NO puedes decir.
#   3) Cómo adaptar la frase al tipo de modelo.
#   4) Qué matices añadir según el test, la significatividad y la estructura.
#
# REGLA DE ORO:
#   La interpretación de un coeficiente depende de DOS cosas:
#   A) Qué tipo de modelo he estimado.
#   B) Qué tipo de variable estoy interpretando.
#
# Ejemplos:
#   - MCO lineal: beta suele ser efecto marginal directo.
#   - IV: beta es efecto usando la parte "limpia" de la variable endógena.
#   - MPL: beta es cambio en probabilidad en puntos porcentuales.
#   - Logit/Probit: beta NO es cambio directo en probabilidad.
#   - Multinomial: beta compara cada categoría con una categoría base.
#   - Tobit: beta afecta a la variable latente, no directamente a y observada.
#   - Heckman: distinguir ecuación de selección y ecuación de resultado.
#   - Efectos fijos: beta es efecto intraindividuo.
#   - Efectos aleatorios: beta se interpreta bajo no correlación entre efectos individuales y regresores.
#   - Panel dinámico: el retardo mide persistencia, instrumentado si procede.
##############################################################


##############################################################
################### 0) REGLAS TRANSVERSALES #################
##############################################################

# ============================================================
# 0.1) SIGNIFICATIVIDAD
# ============================================================
# Si p-valor < 0.05:
#   "El coeficiente es estadísticamente significativo al 5%."
#
# Si p-valor < 0.01:
#   "El coeficiente es estadísticamente significativo al 1%."
#
# Si p-valor < 0.10 pero >= 0.05:
#   "El coeficiente es marginalmente significativo al 10%, pero no al 5%."
#
# Si p-valor >= 0.05:
#   "No hay evidencia estadística suficiente para afirmar que la variable tenga
#    un efecto distinto de cero, manteniendo constantes las demás variables."
#
# OJO:
#   No significativo NO significa que el efecto sea cero con seguridad.
#   Significa que con esta muestra y este modelo no hay evidencia suficiente.


# ============================================================
# 0.2) SIGNO DEL COEFICIENTE
# ============================================================
# Coeficiente positivo:
#   "Un aumento de X se asocia con un aumento de Y", según el modelo.
#
# Coeficiente negativo:
#   "Un aumento de X se asocia con una disminución de Y", según el modelo.
#
# Pero el signo se interpreta de forma distinta según el modelo:
#   - MCO: efecto directo sobre Y.
#   - Logit/Probit: signo sobre la probabilidad de Y=1, magnitud con efectos marginales.
#   - Multinomial: efecto sobre probabilidad relativa frente a la base.
#   - Tobit: efecto sobre variable latente.
#   - Heckman selección: efecto sobre probabilidad latente de selección.
#   - Heckman outcome: efecto sobre cuantía corregida por selección.
#   - Panel FE: efecto dentro del mismo individuo.


# ============================================================
# 0.3) "MANTENIENDO CONSTANTE EL RESTO"
# ============================================================
# En casi todos los modelos debes añadir:
#   "manteniendo constantes el resto de variables"
#   "ceteris paribus"
#
# Porque cada coeficiente es parcial: mide el efecto de X_j dado el resto.


# ============================================================
# 0.4) EFECTO CAUSAL VS ASOCIACIÓN
# ============================================================
# Puedes decir "efecto causal" SOLO si:
#   - el modelo está bien identificado,
#   - los supuestos son defendibles,
#   - en IV los instrumentos son válidos,
#   - en FE se corrige la heterogeneidad constante relevante,
#   - en Heckman se corrige selección bajo los supuestos del modelo.
#
# Si no está claro:
#   Mejor decir "se asocia con", "está relacionado con", "aumenta/disminuye según el modelo".
#
# Frase prudente:
#   "Bajo los supuestos del modelo, el coeficiente puede interpretarse como..."


# ============================================================
# 0.5) VARIABLES EN LOGARITMOS
# ============================================================
# Caso 1: y en niveles, x en niveles:
#   beta = cambio en unidades de y ante aumento de una unidad en x.
#
# Caso 2: y en log, x en niveles:
#   beta ≈ 100*beta % de cambio en y ante aumento de una unidad en x.
#
# Caso 3: y en niveles, x en log:
#   beta/100 = cambio en unidades de y ante aumento del 1% en x.
#
# Caso 4: y en log, x en log:
#   beta = elasticidad.
#   Un 1% más de x se asocia con beta% más de y.
#
# Corrección exacta para binarias en modelos log-lineales:
#   100*(exp(beta)-1)
#
# Ejemplo:
#   Si log(wage) ~ union y beta_union = 0.08:
#      aproximado: estar sindicado aumenta el salario un 8%.
#      exacto: 100*(exp(0.08)-1) = 8.33%.


##############################################################
################### 1) MCO LINEAL BÁSICO ####################
##############################################################

# Modelo:
#   y = beta0 + beta1*x1 + beta2*x2 + u
# Código:
#   modelo_mco <- lm(y ~ x1 + x2, data = datos)
#   summary(modelo_mco)

# ============================================================
# 1.1) VARIABLE CONTINUA
# ============================================================
# Si beta_x1 = 5:
#   "Un aumento de una unidad en x1 se asocia con un aumento de 5 unidades
#    en y, manteniendo constante x2."
#
# SÍ puedes decir:
#   - "cambio esperado en y"
#   - "manteniendo constantes las demás variables"
#   - "si es significativo, hay evidencia estadística de relación"
#
# NO debes decir automáticamente:
#   - "causa" si no has justificado exogeneidad.
#   - "probabilidad" si y no es binaria.
#   - "porcentaje" si y no está en logaritmos.


# ============================================================
# 1.2) VARIABLE BINARIA EN MCO
# ============================================================
# Si D = 1 hombre, 0 mujer:
#   pension = beta0 + beta1*male + controles + u
#
# Si beta_male = 383:
#   "Los hombres tienen, en promedio, una pensión 383 unidades superior
#    a las mujeres, manteniendo constantes los controles."
#
# Si D = 1 tratamiento, 0 no tratamiento:
#   "El grupo tratado tiene una y media beta unidades superior al grupo
#    no tratado, ceteris paribus."
#
# Si no hay identificación causal, decir "diferencia media condicionada",
# no "efecto causal del tratamiento".


# ============================================================
# 1.3) CATEGÓRICAS CON FACTORES
# ============================================================
# Código:
#   datos$prog <- as.factor(datos$prog)
#   datos$prog <- relevel(datos$prog, ref = "academic")
#   modelo <- lm(apt ~ read + math + prog, data = datos)
#
# Si R muestra:
#   proggeneral = -20
#   progvocational = -35
#
# Interpretación:
#   proggeneral:
#      "Los alumnos del programa general tienen, en promedio, 20 puntos menos
#       de aptitud que los del programa academic, manteniendo constantes read y math."
#
#   progvocational:
#      "Los alumnos del programa vocational tienen, en promedio, 35 puntos menos
#       que los del programa academic, manteniendo constantes read y math."
#
# NO debes decir:
#   "general tiene 15 puntos más que vocational" mirando directamente -20 y -35
#   sin hacer contraste.
#
# Para comparar dos categorías que no son la base:
#   linearHypothesis(modelo, "proggeneral = progvocational")
#
# Si p < 0.05:
#   "Hay diferencia significativa entre general y vocational."


# ============================================================
# 1.4) INTERCEPTO
# ============================================================
# Intercepto:
#   Valor esperado de y cuando todas las X valen cero.
#
# Muchas veces NO tiene interés económico.
#
# Frase:
#   "El intercepto representa la predicción para un individuo con todas las
#    variables explicativas iguales a cero. Si ese perfil no es realista,
#    no tiene una interpretación económica relevante."


##############################################################
################ 2) ERRORES ROBUSTOS HC1 ####################
##############################################################

# Código:
#   coeftest(modelo, vcov = vcovHC(modelo, type = "HC1"))

# Interpretación:
#   Los coeficientes NO cambian.
#   Cambian los errores estándar, estadísticos t/z y p-valores.
#
# ¿Por qué?
#   Porque sospechamos heterocedasticidad.
#
# SÍ puedes decir:
#   "La inferencia se realiza con errores robustos a heterocedasticidad."
#
# NO debes decir:
#   "El modelo robusto corrige el sesgo de los coeficientes."
#
# HC1 corrige la INFERENCIA, no transforma una variable endógena en exógena.


##############################################################
################### 3) VARIABLES INSTRUMENTALES ##############
##############################################################

# Modelo:
#   y = beta0 + beta1*x_endog + beta2*x_exog + u
# IV:
#   modelo_iv <- ivreg(y ~ x_endog + x_exog | z + x_exog, data = datos)

# ============================================================
# 3.1) COEFICIENTE ENDÓGENO INSTRUMENTADO
# ============================================================
# Ejemplo:
#   wage ~ educ + exper | sibs + meduc + exper
#
# Si beta_educ = 141.64:
#   "Manteniendo constante la experiencia, un año adicional de educación
#    aumenta el salario esperado en 141.64 unidades, usando la variación de
#    la educación explicada por los instrumentos sibs y meduc."
#
# Versión más fina:
#   "Bajo la validez de los instrumentos, el coeficiente de educ mide el efecto
#    causal local de la educación sobre el salario para la variación inducida
#    por los instrumentos."
#
# SÍ puedes decir:
#   - "usando la parte limpia/exógena de educ"
#   - "si los instrumentos son válidos, puede interpretarse causalmente"
#
# NO debes decir:
#   - "es simplemente el efecto MCO de educ"
#   - "los instrumentos afectan directamente a wage"
#   - "Sargan demuestra completamente que los instrumentos son válidos"
#
# Sargan no demuestra validez absoluta. Solo no rechaza la validez conjunta
# bajo ciertas condiciones y si el modelo está sobreidentificado.


# ============================================================
# 3.2) VARIABLE EXÓGENA EN IV
# ============================================================
# En:
#   wage ~ educ + exper | sibs + meduc + exper
#
# exper aparece a ambos lados porque es exógena y se instrumenta a sí misma.
#
# Si beta_exper = 34.20:
#   "Manteniendo constante la educación instrumentada, un año adicional de
#    experiencia se asocia con un aumento de 34.20 unidades en el salario."
#
# La interpretación de exper es lineal normal, pero dentro de un modelo IV.


# ============================================================
# 3.3) DIAGNÓSTICOS IV
# ============================================================
# Weak instruments:
#   H0: instrumentos débiles.
#   Si F alto y p pequeño:
#      "Los instrumentos explican suficientemente la variable endógena."
#
# Wu-Hausman:
#   H0: la variable sospechosa es exógena.
#   Si p < 0.05:
#      "Rechazamos exogeneidad; MCO sería inconsistente; preferimos IV."
#   Si p >= 0.05:
#      "No hay evidencia de endogeneidad; MCO puede ser preferible por eficiencia."
#
# Sargan:
#   H0: instrumentos válidos.
#   Solo si sobreidentificado.
#   Si p >= 0.05:
#      "No rechazamos la validez conjunta de los instrumentos."
#   Si p < 0.05:
#      "Hay sospecha de que al menos un instrumento no sea válido."
#
# R2 en IV:
#   Puede ser bajo o incluso negativo. No se elige IV por R2.


##############################################################
#################### 4) MODELO DE PROBABILIDAD LINEAL ########
##############################################################

# Modelo:
#   modelo_mpl <- lm(y_binaria ~ x1 + x2, data = datos)
# Donde y_binaria = 1 si ocurre el evento, 0 si no.

# Si beta_x1 = 0.04:
#   "Un aumento de una unidad en x1 aumenta la probabilidad de que y=1
#    en 0.04, es decir, en 4 puntos porcentuales, manteniendo constantes
#    las demás variables."
#
# Si beta_x1 = -0.25:
#   "Un aumento de una unidad en x1 reduce la probabilidad de que y=1
#    en 25 puntos porcentuales."
#
# Si x1 es binaria:
#   "Pasar de x1=0 a x1=1 cambia la probabilidad de y=1 en beta puntos,
#    manteniendo constantes las demás variables."
#
# SÍ puedes decir:
#   - "puntos porcentuales"
#   - "probabilidad"
#   - "efecto marginal constante"
#
# NO debes decir:
#   - "porcentaje" si te refieres a puntos porcentuales.
#     0.04 = 4 puntos porcentuales, no necesariamente "4% más" relativo.
#
# Limitaciones:
#   - puede predecir probabilidades menores que 0 o mayores que 1.
#   - heterocedasticidad.
#   - linealidad de efectos marginales.


##############################################################
#################### 5) LOGIT Y PROBIT ######################
##############################################################

# Modelo:
#   modelo_logit <- glm(y ~ x1 + x2, family=binomial(link="logit"), data=datos)
#   modelo_probit <- glm(y ~ x1 + x2, family=binomial(link="probit"), data=datos)

# ============================================================
# 5.1) COEFICIENTES
# ============================================================
# En Logit/Probit, beta NO es cambio directo en probabilidad.
#
# Si beta_x1 > 0:
#   "Un aumento de x1 aumenta la probabilidad de que y=1, manteniendo
#    constantes las demás variables."
#
# Si beta_x1 < 0:
#   "Un aumento de x1 reduce la probabilidad de que y=1."
#
# NO decir:
#   "beta_x1 = 0.12 implica que la probabilidad aumenta 12 puntos porcentuales."
# Eso es incorrecto.


# ============================================================
# 5.2) EFECTOS MARGINALES
# ============================================================
# Código:
#   slopes(modelo_logit)
#   slopes(modelo_probit)
#
# Si efecto marginal de educ = 0.035:
#   "Un año adicional de educación aumenta la probabilidad estimada de y=1
#    en 3.5 puntos porcentuales, en promedio / para el perfil evaluado."
#
# En Logit/Probit, el efecto marginal depende del individuo.
#
# Puedes decir:
#   - "efecto marginal medio"
#   - "efecto marginal evaluado en las medias"
#
# NO mezclar:
#   coeficiente logit/probit != efecto marginal.


# ============================================================
# 5.3) ODDS RATIO EN LOGIT
# ============================================================
# En Logit, exp(beta) es odds ratio.
#
# Si beta = 0.40:
#   exp(0.40)=1.49
#   "Un aumento de una unidad en x multiplica las odds de y=1 por 1.49,
#    manteniendo constantes las demás variables."
#
# O:
#   "Aumenta las odds en un 49%."
#
# OJO:
#   Odds no es lo mismo que probabilidad.
#   Mejor usar efectos marginales si te preguntan probabilidad.


##############################################################
#################### 6) LOGIT MULTINOMIAL ###################
##############################################################

# Modelo:
#   modelo_mult <- multinom(FUMA ~ EDAD + SUENO + ALCOHOL + EJERTL, data=datos)
#
# Supongamos:
#   FUMA = 0 fumador      <- categoría base
#          1 exfumador
#          2 nunca fumador

# Cada fila del output compara una categoría con la categoría base.
#
# Si fila 1, EDAD = 0.064:
#   "A mayor edad, aumenta la probabilidad relativa de estar en la categoría 1
#    frente a la categoría base 0, manteniendo constantes las demás variables."
#
# Si fila 2, ALCOHOL = -0.644:
#   "A mayor consumo de alcohol, disminuye la probabilidad relativa de estar en
#    la categoría 2 frente a la categoría base 0."
#
# NO decir:
#   "La probabilidad de estar en la categoría 2 baja 0.644."
#
# Eso es falso. El coeficiente afecta al logaritmo de la razón de probabilidades.
#
# exp(beta) = factor multiplicativo de las odds relativas.
#
# Si beta_EDAD en fila 1 = 0.064:
#   exp(0.064) = 1.066
#   "Un año adicional de edad multiplica por 1.066 las odds relativas de
#    estar en categoría 1 frente a categoría 0."
#
# Para hablar de probabilidades absolutas:
#   predict(modelo_mult, type="probs")


##############################################################
######################### 7) TOBIT ###########################
##############################################################

# Modelo:
#   modelo_tobit <- tobit(y ~ x1 + x2, left=0, data=datos)
#
# Idea:
#   y* = X beta + u
#   y = 0 si y* <= 0
#   y = y* si y* > 0

# ============================================================
# 7.1) OBSERVACIONES CENSURADAS
# ============================================================
# Si output:
#   Total = 645
#   Left-censored = 248
#   Uncensored = 397
#
# Interpretación:
#   "De 645 observaciones, 248 están censuradas en cero y 397 presentan
#    valores positivos. Esto justifica considerar un modelo Tobit si los ceros
#    se interpretan como censura o valores latentes no observados."


# ============================================================
# 7.2) COEFICIENTES TOBIT
# ============================================================
# Si ecoprc = -3.75:
#   "Un aumento del precio de las manzanas ecológicas reduce la variable
#    latente de consumo de manzanas ecológicas."
#
# Si regprc = 3.05:
#   "Un aumento del precio de las manzanas convencionales aumenta la variable
#    latente de consumo de manzanas ecológicas, lo que sugiere sustitución."
#
# Si faminc = 0.004:
#   "Una mayor renta familiar aumenta la propensión latente a consumir manzanas
#    ecológicas."
#
# Si hhsize = 0.124:
#   "Los hogares más grandes tienen mayor propensión latente a consumir
#    manzanas ecológicas."
#
# NO decir:
#   "Si ecoprc sube una unidad, ecolbs observado baja exactamente 3.75."
#
# Correcto:
#   "El coeficiente afecta directamente a la variable latente y*. Para saber
#    el efecto sobre la variable observada, hay que calcular efectos marginales."


# ============================================================
# 7.3) TRES EFECTOS EN TOBIT
# ============================================================
# 1) Efecto sobre y*:
#      dE(y*|x)/dx = beta
#      Fácil, pero normalmente menos interesante.
#
# 2) Efecto sobre E(y | y>0, x):
#      efecto entre quienes tienen valor positivo.
#
# 3) Efecto sobre E(y | x):
#      efecto total incluyendo ceros.
#      Suele ser el más interpretable si preguntan consumo esperado total.
#
# Scale / Log(scale): desviación típica estimada del error.
# No es una variable explicativa.


##############################################################
################### 8) TWO-PART / MODELO DE BARRERA #########
##############################################################

# Primera parte:
#   Probit/logit para s = 1[y > 0]
# Segunda parte:
#   MCO para y, usando solo y > 0.

# Primera parte, si coeficiente positivo:
#   "Aumenta la probabilidad de participar / tener y positivo."
# Pero si es probit/logit, beta no es efecto marginal directo.
#
# Segunda parte, si beta_x1 = -2:
#   "Entre quienes consumen, un aumento de una unidad en x1 reduce la cantidad
#    consumida en 2 unidades, manteniendo constantes las demás variables."
#
# NO decir:
#   "Este es el efecto total sobre toda la población."
#
# Para efecto total combinar probabilidad de participar + cantidad condicionada.
#
# Diferencia con Tobit:
#   Tobit: un solo mecanismo para participación y cantidad.
#   Two-part: dos mecanismos separados.
#
# Si errores de ambas ecuaciones están correlacionados:
#   MCO de segunda parte puede estar sesgado -> pensar Heckman.


##############################################################
######################### 9) HECKMAN #########################
##############################################################

# Modelo Heckman:
#   Selección: s* = z gamma + v ; s=1 si observamos y / si y>0
#   Resultado: y = x beta + u
#
# Si corr(u,v) != 0, la muestra observada no es aleatoria.

# ============================================================
# 9.1) OLS NAIVE SOBRE MUESTRA OBSERVADA
# ============================================================
# Código:
#   datos_pos <- subset(datos, pension > 0)
#   modelo_ols <- lm(pension ~ male + edad + tenure + educ, data=datos_pos)
#
# Interpretación de male:
#   "Entre quienes tienen pensión positiva, los hombres tienen una pensión
#    media beta unidades superior/inferior a las mujeres, manteniendo constantes
#    edad, antigüedad y educación."
#
# Lo que NO dice:
#   - No dice la brecha total de género en toda la población.
#   - No corrige que tener pensión positiva puede no ser aleatorio.
#   - No separa adecuadamente selección y cuantía si hay selección endógena.


# ============================================================
# 9.2) ECUACIÓN DE SELECCIÓN
# ============================================================
# selection = pension_pos ~ male + edad + tenure + educ + union
# Es un probit de selección.
#
# Si tenure > 0 significativo:
#   "Mayor antigüedad aumenta la probabilidad latente de tener pensión positiva."
#
# Si educ > 0 significativo:
#   "Mayor educación aumenta la probabilidad latente de tener pensión positiva."
#
# Si union > 0 significativo:
#   "La afiliación sindical aumenta la probabilidad latente de tener pensión positiva."
#
# NO decir:
#   "union aumenta la probabilidad en 1.16."
# Correcto:
#   "union aumenta la probabilidad latente / el índice de selección."


# ============================================================
# 9.3) ECUACIÓN DE RESULTADO
# ============================================================
# outcome = pension ~ male + edad + tenure + educ
#
# Si male = 383:
#   "Corrigiendo por selección muestral, los hombres tienen una pensión esperada
#    383 unidades superior a las mujeres, manteniendo constantes edad,
#    antigüedad y educación."
#
# Si tenure = 13:
#   "Corrigiendo por selección, un año adicional de antigüedad aumenta la pensión
#    esperada en 13 unidades."
#
# Si educ = 46:
#   "Corrigiendo por selección, un año adicional de educación aumenta la pensión
#    esperada en 46 unidades."


# ============================================================
# 9.4) LAMBDA / IMR Y RHO
# ============================================================
# Heckman dos etapas:
#   Si lambda significativo:
#      "Hay evidencia de selección muestral endógena. OLS sobre la muestra
#       observada puede estar sesgado y Heckman es preferible."
#
# Heckman ML:
#   rho = corr(u, v)
#   Si rho significativo:
#      "Los errores de selección y resultado están correlacionados. Hay selección endógena."
#
# Si rho < 0 significativo:
#   "La correlación entre los inobservables que aumentan la selección y los
#    inobservables que aumentan el resultado es negativa."
#
# Para examen basta:
#   "rho significativo justifica Heckman frente al OLS naive."


# ============================================================
# 9.5) VARIABLE DE EXCLUSIÓN
# ============================================================
# union entra en selección y no en resultado.
#
# Debe cumplir:
#   1) Relevancia: explicar pension_pos.
#   2) Exclusión: no afectar directamente a pension una vez controlado por x.
#
# Frase:
#   "union actúa como variable de exclusión porque se supone que afecta a la
#    probabilidad de tener pensión positiva, pero no directamente a la cuantía
#    de la pensión condicionada a tenerla."


##############################################################
#################### 10) DATOS DE PANEL: POOLING ############
##############################################################

# Pooling:
#   y_it = alpha + beta*x_it + u_it
# Ignora heterogeneidad individual.

# Si beta_ROA = -0.4:
#   "Un aumento de una unidad en ROA se asocia con una disminución de 0.4
#    unidades en la deuda, manteniendo constantes las demás variables."
#
# Pero:
#   Esta interpretación ignora diferencias fijas entre individuos.
#
# NO decir:
#   "Controla por características propias de cada empresa."
# Eso lo hace FE/RE, no pooling.


##############################################################
#################### 11) EFECTOS FIJOS #######################
##############################################################

# Modelo:
#   y_it = alpha_i + beta*x_it + u_it

# Si beta_ROA = -0.4:
#   "Cuando una misma empresa aumenta su ROA en una unidad respecto a su
#    propio nivel habitual, su deuda disminuye en 0.4 unidades, manteniendo
#    constantes las demás variables."
#
# Si beta_Produccion = 2:
#   "Cuando una misma empresa aumenta su producción en una unidad, su empleo
#    aumenta en 2 unidades, controlando por las características fijas de la empresa."
#
# Palabras clave:
#   - "dentro del mismo individuo"
#   - "intraempresa"
#   - "respecto a su propia media"
#   - "controlando por heterogeneidad individual constante en el tiempo"
#
# SÍ puede decir FE:
#   "Controla por factores no observados constantes en el tiempo."
#   "Permite que alpha_i esté correlacionado con X."
#   "Reduce sesgo por variables omitidas constantes en el tiempo."
#
# NO puede decir FE:
#   "Corrige cualquier endogeneidad."
#   "Corrige simultaneidad."
#   "Corrige errores de medida."
#   "Corrige variables omitidas que cambian en el tiempo."
#   "Permite estimar variables constantes en el tiempo."
#
# FE NO puede estimar variables que no cambian dentro del individuo:
#   sexo, raza, sector fijo, región fija, educación si no cambia, etc.


##############################################################
#################### 12) EFECTOS ALEATORIOS #################
##############################################################

# Modelo:
#   y_it = alpha + beta*x_it + c_i + u_it

# Si beta_ROA = -0.4:
#   "Un aumento de una unidad en ROA se asocia con una disminución de 0.4
#    unidades en la deuda, manteniendo constantes las demás variables,
#    bajo el supuesto de que las diferencias individuales no observadas
#    no están correlacionadas con ROA ni con el resto de regresores."
#
# RE usa variación:
#   - dentro de individuos,
#   - entre individuos.
#
# Por eso no es una interpretación puramente intraindividuo como FE.
#
# SÍ puede decir RE:
#   "Es más eficiente que FE si cov(alpha_i, X)=0."
#   "Permite estimar variables constantes en el tiempo."
#   "Trata la heterogeneidad individual como aleatoria."
#
# NO puede decir RE:
#   "Es válido aunque alpha_i esté correlacionado con X."
#   "Siempre es mejor que FE porque es más eficiente."
#
# Hausman:
#   H0: cov(alpha_i, X)=0 -> RE válido.
#   H1: cov(alpha_i, X)!=0 -> RE inconsistente, FE preferible.
#
# Si p < 0.05:
#   "Rechazamos H0. Hay evidencia de correlación entre efectos individuales
#    y regresores. Se prefieren efectos fijos."
#
# Si p >= 0.05:
#   "No rechazamos H0. No hay evidencia de correlación. Efectos aleatorios
#    puede usarse y es más eficiente."


##############################################################
#################### 13) PANEL DINÁMICO ######################
##############################################################

# Modelo:
#   y_it = gamma*y_i,t-1 + beta*X_it + alpha_i + u_it
#
# gamma mide persistencia.

# Si gamma = 0.6:
#   "Existe persistencia dinámica: un aumento de una unidad en y del periodo
#    anterior se asocia con un aumento de 0.6 unidades en y actual, manteniendo
#    constantes las demás variables y controlando por efectos individuales."
#
# Si gamma > 0 significativo:
#   "Hay inercia/persistencia."
#
# Si gamma no significativo:
#   "No hay evidencia clara de persistencia dinámica una vez controladas las demás variables."
#
# Si gamma cercano a 1:
#   "Alta persistencia."
#
# Si gamma negativo:
#   "Hay reversión: valores altos en el periodo anterior se asocian con valores
#    más bajos en el periodo actual." Revisar especificación.

# ¿Por qué se instrumenta el retardo?
#   y_i,t-1 es endógeno porque:
#     - está correlacionado con alpha_i,
#     - tras transformar el modelo, queda correlacionado con el error transformado.
#
# Instrumentos internos:
#   y_i,t-2
#   y_i,t-2 - y_i,t-3

# Ejemplo:
#   Empleo ~ Empleo_1 + Salarios + Capital + Produccion |
#            Empleo_2 + Salarios + Capital + Produccion
#
# Empleo_1:
#   "El coeficiente de Empleo_1 mide la persistencia del empleo. Al estar
#    instrumentado con Empleo_2, se interpreta usando la variación del empleo
#    retardado explicada por retardos más antiguos."
#
# Salarios:
#   "Manteniendo constantes el empleo retardado instrumentado, capital y
#    producción, un aumento de salarios se asocia con..."
#
# SÍ en panel dinámico:
#   "Se instrumenta el retardo por endogeneidad dinámica."
#   "Los retardos más antiguos se usan como instrumentos internos."
#   "Si el retardo es significativo, hay persistencia."
#
# NO:
#   "Efectos fijos por sí solo corrige completamente el problema dinámico."
#   "No hace falta pensar en validez del instrumento."
#
# Matiz:
#   En la práctica se justifican los instrumentos por la estructura del modelo.
#   Pero si se puede, conviene mirar relevancia del instrumento.
#   Sargan solo si hay más instrumentos que variables endógenas.


##############################################################
#################### 14) FRASES FINALES DE EXAMEN ###########
##############################################################

# ============================================================
# 14.1) SI TE PIDEN INTERPRETAR CUALQUIER COEFICIENTE
# ============================================================
# Plantilla:
#   "El coeficiente de [X] es [beta]. Esto indica que, manteniendo constantes
#    las demás variables, [interpretación adaptada al modelo]. El efecto es/no es
#    estadísticamente significativo al [nivel] porque el p-valor es [p]."
#
# Adaptar:
#   - MCO: cambio en unidades de y.
#   - MPL: cambio en puntos porcentuales de probabilidad.
#   - Logit/Probit: signo sobre probabilidad, magnitud con efectos marginales.
#   - Multinomial: probabilidad relativa frente a la categoría base.
#   - Tobit: variable latente; magnitud observada con efectos marginales.
#   - Heckman selección: probabilidad latente de selección.
#   - Heckman outcome: cuantía corregida por selección.
#   - FE: dentro del mismo individuo.
#   - RE: bajo supuesto de no correlación entre efectos individuales y X.
#   - Panel dinámico: persistencia si es el retardo; instrumentado si procede.

# ============================================================
# 14.2) SI TE PIDEN QUÉ NO PODEMOS CONCLUIR
# ============================================================
# "No podemos interpretar el coeficiente como causal sin que se cumplan los
#  supuestos de identificación del modelo. En particular, en MCO necesitamos
#  exogeneidad; en IV necesitamos instrumentos válidos; en Logit/Probit los
#  coeficientes no son efectos marginales directos; en Tobit los coeficientes
#  afectan a una variable latente; en Heckman la corrección depende del supuesto
#  de selección y de la variable de exclusión; y en efectos aleatorios necesitamos
#  ausencia de correlación entre efectos individuales y regresores."

# ============================================================
# 14.3) SI TE PIDEN QUÉ MODELO ELEGIR
# ============================================================
# "La elección del modelo depende del problema econométrico. Si hay endogeneidad,
#  uso IV; si la dependiente es binaria, Logit/Probit; si hay muchos ceros por
#  censura, Tobit; si hay dos decisiones, two-part; si hay selección endógena,
#  Heckman; si hay panel, decido entre pooling, efectos fijos y aleatorios con
#  los contrastes; y si hay panel dinámico, instrumento la variable dependiente
#  retardada con retardos más antiguos."

##############################################################
######################## FIN DE LA PLANTILLA #################
##############################################################
