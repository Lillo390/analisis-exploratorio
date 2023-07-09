#################### TEMA 3: Diágnostico y validación ###############

load("C:/Users/1/Master_CD/Analisis_exploratorio_de_datos/Examen_modelos_lineales/Tema 3 Diagnóstico y validación en RLM-20221115/Solución Ejercicios Tema 3/datosTema3.RData")
ls()



#### DIAGNÓSTICO ####


ml1 <- lm(sales ~ TV + radio, data= advertising)

# Grafico generalista de diagnóstico
par(mfrow=c(2,2))
plot(ml1)


# Obtención de residuos:

res_sta<-rstandard(ml1)  #residuos estandarizados

#podriamos calcularlos manualmente:
sigma_hat <- summary(ml1)$sigma
h_i <- influence(ml1)$hat #La función influence proporciona cantidades básicas
# para el diagnóstico del modelo
res_estandar <- residuals(ml1)/(sqrt(1-h_i)*sigma_hat)
sum(res_estandar-res_sta)

res_stu<-rstudent(ml1)  #residuos estudentizados
# no fácilmente calculables a mano

######Gráfico de residuos frente a valores predichos: útil para detectar no linealidad y heterocedasticidad####

par(mfrow=c(1,1))
plot(fitted(ml1),res_stu)

#  Residuos parciales (de cada uno de los predictores)
parciales <- residuals(ml1,type="partial")
plot(advertising$TV,parciales[,1]) 
plot(advertising$radio,parciales[,2]) 
  
###algunos recursos adicionales respecto a no linealidad


    # pintar línea horizonal
    plot(fitted(ml1),res_stu)
    abline(h=0, col="gray")   
    #identify(fitted(ml1),res_stu)
    
    # Añadir un loess (con ggplot se puede añadir IC fácilmente)

  library(ggplot2)
  library(gridExtra)
  ggplot(data = advertising, aes(x =fitted(ml1), y = res_stu)) + 
  geom_point() + geom_smooth(color = "coral",span=0.4) + geom_hline(yintercept = 0) +
  labs(y = "residuos studentizados",
       x = "valores ajustados") +
  theme_bw()
  
  # Vamos a ver marginalmente cual de la variables es la "culpable" (o ambas).
  # Gráficos marginales:

  plot1 <- ggplot(data = advertising, aes(TV, res_stu)) +
  geom_point() + geom_smooth(color = "coral",span=0.4) + geom_hline(yintercept = 0) +
  theme_bw()
  plot2 <- ggplot(data = advertising, aes(radio, res_stu)) +
  geom_point() + geom_smooth(color = "coral",span=0.4) + geom_hline(yintercept = 0) +
  theme_bw()
  grid.arrange(plot1, plot2)
  
  # Posibles soluciones:  regresión polinómica, tranformación de las explicativas.
  
  ml2 <- lm(sales ~ TV + I(TV^2)+radio, data= advertising)
  library(ggplot2)
  ggplot(data = advertising, aes(x =fitted(ml2), y = rstudent(ml2))) + 
    geom_point() + geom_smooth(color = "coral",span=0.4) + geom_hline(yintercept = 0) +
    labs(y = "residuos studentizados",
         x = "valores ajustados") +
    theme_bw()
  
  ml3 <- lm(sales ~ I(sqrt(TV))+radio, data= advertising)
  library(ggplot2)
  ggplot(data = advertising, aes(x =fitted(ml3), y = rstudent(ml3))) + 
    geom_point() + geom_smooth(color = "coral",span=0.4) + geom_hline(yintercept = 0) +
    labs(y = "residuos studentizados",
         x = "valores ajustados") +
    theme_bw()
  
  
  advertising$rstan<-rstandard(lm(sales~radio,data=advertising))
  loessTV <- loess(rstan ~ TV, span = 0.7, data = advertising)
  advertising$TVnl<-predict(loessTV)
  
  mnl1 <- lm(sales~TVnl+radio, data= advertising)
  library(ggplot2)
  ggplot(data = advertising, aes(x =fitted(mnl1), y = rstudent(mnl1))) + 
    geom_point() + geom_smooth(color = "coral",span=0.4) + geom_hline(yintercept = 0) +
    labs(y = "residuos studentizados",
         x = "valores ajustados") +
    theme_bw()
  
  # a veces nada funciona! :(

####algunos recursos adicionales respecto a no homocedasticidad####
  
    # Test de Breusche-Pagan. La hipótesis nula es la homocedasticidad
    library(lmtest)
    bptest(ml1) 
  
  # Posibles soluciones: Transformación de la respuesta

    ajuste_vol <- lm(Volume ~ Height, data = trees)
    bptest(ajuste_vol)  
    
    
    #transformaciones de box-cox
    
    library(MASS)
    b<-boxcox(ajuste_vol, lambda = seq(-2,2, length = 10))
    lambda <- b$x[which.max(b$y)]
    ajuste_vol2 <- lm(I((Volume^lambda-1)/lambda) ~ Height, data = trees)
    plot(fitted(ajuste_vol2), rstudent(ajuste_vol2))
    bptest(ajuste_vol2) 
    
    
    #transformaciones convexas como exp o ^2
    
    ajuste_vol3a <- lm(I(exp(Volume)) ~ Height, data = trees)
    plot(fitted(ajuste_vol3a), rstudent(ajuste_vol3a))
    bptest(ajuste_vol3a) 
    
    ajuste_vol3b <- lm(I(Volume^2) ~ Height, data = trees)
    plot(fitted(ajuste_vol3b), rstudent(ajuste_vol3b))
    bptest(ajuste_vol3b) 
    
    
    #transformaciones cóncavas como log o sqrt
    
    ajuste_vol4a <- lm(I(sqrt(Volume)) ~ Height, data = trees)
    plot(fitted(ajuste_vol4a), rstudent(ajuste_vol4a))
    bptest(ajuste_vol4a) 
    
    ajuste_vol4b <- lm(I(log(Volume)) ~ Height, data = trees)
    plot(fitted(ajuste_vol4b), rstudent(ajuste_vol4b))
    bptest(ajuste_vol4b) 
    
  
##### Normalidad y outliers: qqplot ####

qqnorm(res_stu)
qqline(res_stu,col="red")
#identify(qqnorm(res_stu)$x,qqnorm(res_stu)$y)
#falla normalidad

 ### algunos recursos adicionales

  #test de normalidad
  shapiro.test(res_stu) # La hipótesis nula se corresponde a la normalidad de los residuos
  
  #resumen numérico del vector de residuos
  summary(res_stu)
  advertising[abs(res_stu) > 3,]
  
  #resumen gráfico del vector de residuos
  plot(fitted(ml1),abs(res_stu), type="p", main="residuos vs fitted")
  abline(h=3,lwd=1, col="red")     
  abline(h=4,lwd=2, col="red")
  abline(h=5,lwd=4, col="red")
  # identify(fitted(ml1),abs(res_stu))
  
  #### solución: ninguna

####Medidas de influencia: Leverages  y distancia de Cook  ####  

  #leverages
  n<-nrow(advertising)
  plot(fitted(ml1),hatvalues(ml1),main="leverages vs fitted")

  abline(h=2*3/n,col="red",lwd=1);  
  abline(h=3*3/n,col="red",lwd=3);
  #identify(fitted(ml1),hatvalues(ml1))

  boxplot(hatvalues(ml1))   
  summary(hatvalues(ml1))
  
  
  #distancia de Cooks
 
  plot(fitted(ml1),cooks.distance(ml1),main="Distancia de Cook vs fitted")
  abline(h=1,col="red",lwd=1);  
  #identify(fitted(ml1),cooks.distance(ml1))

  boxplot(cooks.distance(ml1))
  summary(cooks.distance(ml1)) 

  #### Recursos adicionales para infuencia

  # Explorar distancia de cook y leverage simultáneamente
  library(car)
  influencePlot(ml1)
  summary(influence.measures(ml1))

  # La filas 131 y 6 son influyentes. 

  #### solución: eliminar datos influyentes
  
  lm1_sinInfl<-update(ml1, data=advertising[setdiff(rownames(advertising),c("6","131")),])
  par(mfrow=c(2,2))
  plot(lm1_sinInfl)
  
#####Colinealidad  ####  


library(ISLR)
vif(ml1)  #valores menores que 10 no hay problema


pairs(advertising[,c('TV','radio')])
#nada de correlación entre las dos variables

#solo porque es más bonito
library(GGally)
ggpairs(advertising[,c('sales', 'TV','radio')], lower = list(continuous = "smooth"),
        diag = list(continuous = "barDiag"), axisLabels = "none")

#sólo para continuas
cor(advertising[c('TV','radio')])

library(corrplot)
corrplot(cor(advertising), method = "number", tl.col = "black")

# El resumen de nuestro diagnóstico: no normalidad, no linealidad, homogeneidad aceptable, outliers: 131; influyentes: 6 y 131
# ningún problema con la colinealidad. 

### Soluciones

#1º eliminar influyentes y re-evaluar
#2º ocuparse de la multicolinelidad si existiera: eliminar variables con mayores vif. Creación de índices. 
#3º probar resolver heterocedasticidad con transformaciones de la respuesta (cóncavas o de box-cox). Si hay un predictor responsable 
#ponderar
#4º ocuparse de la linealidad: poco que probar, si hay un responsable ajustarlo de manera polinómica o con loess. 
#5º revisar normalidad de los residuos y posibles outliers (En este caso solo revisar)


#####Validación ####  

library(ISLR)
datos <- subset(Credit,select = Income:Balance)
set.seed(12345)
seleccion <- sample(nrow(datos),round(nrow(datos)*3/4))
entrenamiento <- datos[seleccion,]
prueba <- datos[-seleccion,]

ajuste <- lm(Balance ~ .,data=entrenamiento)
summary(ajuste)
prediccion <- predict(ajuste,prueba)
(ecm <- mean((prueba$Balance-prediccion)^2))
## la estimación dada por el modelo: mean(residuals(ajuste)^2)

# Pero el error cuadrático medio de la predicción tiene mucha variabilidad
resultados <- ecm
for (i in 1:20) {
  seleccion <- sample(nrow(datos),round(nrow(datos)*3/4))
  entrenamiento <- datos[seleccion,]
  prueba <- datos[-seleccion,]
  ajuste <- lm(Balance ~ . , data=entrenamiento)
  prediccion <- predict(ajuste,prueba)
  resultados <- c(resultados,mean((prueba$Balance-prediccion)^2))
}
summary(resultados)

## validación cruzada

#  Leaving-one-out              
library(ISLR)
datos <- subset(Credit,select = Income:Balance)
ajuste <- glm(Balance ~ ., data=datos)
library(boot)
ecm <- cv.glm(datos,ajuste)
ecm$delta # La primera componente es la estimación bruta de validación cruzada 
          # del error de predicción. La segunda es la estimación de validación cruzada ajustada
sqrt(ecm$delta)


#  En los modelos lineales, se podría calcular: 
ajuste <- lm(Balance ~ ., data=datos)
residuos <- ajuste$residuals
leverages <- hatvalues(ajuste)
ecm_l_one_out <- mean((residuos/(1-leverages))^2)
ecm_l_one_out


#  k-Fold Cross-Validation       

library(boot)
datos <- subset(Credit,select = Income:Balance)
ajuste <- glm(Balance ~ ., data=datos)
set.seed(12345)
ecm <- cv.glm(datos,ajuste,K=10)
ecm$delta


# Podemos ver que incluso haciendo K grupos la variabilidad del ecm se reduce.
resultadosK <- NULL
for (i in 1:20) {
   resultadosK <- c(resultadosK,cv.glm(datos,ajuste,K=10)$delta[1])
}
summary(resultadosK)


####Validacion en selección de modelos####

#  Selección de modelos por Conjunto de validación

    library(ISLR)
    datos <- subset(Credit,select = Income:Balance)

  #  1. Construimos los conjuntos de entrenamiento y validación
    set.seed(12345)
    seleccion <- sample(nrow(datos),round(nrow(datos)/2))
    entrenamiento <- datos[seleccion,]
    prueba <- datos[-seleccion,]

  #  2.Con datos entrenamiento, obtenemos el mejor modelo de cada tamaño
    library(leaps)
    ajustes.entren <- regsubsets(Balance ~ ., data=entrenamiento, nvmax=11)
    summary(ajustes.entren)

  #  3. Con datos validación, comparamos esos modelos y elegimos el de menor ecm
    matriz.diseño <- model.matrix(Balance ~ ., data=prueba) # Matriz de diseño (X)
    errores.pred <- NULL
    for (i in 1:11) {
      coefi <- coef(ajustes.entren, id=i)
      pred <- matriz.diseño[,names(coefi)] %*% coefi
      errores.pred <- c(errores.pred,mean((pred-prueba$Balance)^2))
    }
    errores.pred   #  Puede variar si cambiamos la semilla de aleatorización
    (qq <- which.min(errores.pred))     # Número de covariables óptimo: 6

    # 4. Ajustamos todos los datos con ese número de predictores.
    ajustes.todo <- regsubsets(Balance ~ ., data=datos, nvmax=11) 
    coef(ajustes.todo, id=which.min(errores.pred))


    
#  Selección de modelos por Conjunto de validación (programación alternativa)

    # 1. construimos una Función de predicción 
    errores.pred.todos <- function(ajustes.entren,matriz.diseño,respuesta) {
    qq <- length(summary(ajustes.entren)$rss) #suma residual de cuadrados
    errores.pred <- NULL
    for (i in 1:qq) {
    coefi <- coef(ajustes.entren, id=i)
    pred <- matriz.diseño[,names(coefi)] %*% coefi
    errores.pred <- c(errores.pred,mean((pred-respuesta)^2))
    }
   return(errores.pred)
   }

    # 2. Construimos los conjuntos de entrenamiento y validación
    set.seed(12345)
    seleccion <- sample(nrow(datos),round(nrow(datos)/2))
    entrenamiento <- datos[seleccion,]
    prueba <- datos[-seleccion,]

    #  Obtención de resultados
    ajustes.entren <- regsubsets(Balance ~ ., data=entrenamiento, nvmax=11)
    matriz.diseño <- model.matrix(Balance ~ ., data=prueba) # Matriz de diseño (X)

    errores.pred <- errores.pred.todos(ajustes.entren,matriz.diseño,prueba$Balance)


    ajustes.todo <- regsubsets(Balance ~ ., data=datos, nvmax=11) 
    coef(ajustes.todo, id=which.min(errores.pred))

#  Selección de modelos utilizando Validación Cruzada  (K-fold cross validation)

    # 1. Construimos la partición del banco de datos en K subgrupos 
      # Varias formas: la rápida (grupos no balanceados en tamaño)
    k <- 10
    n <- nrow(datos)
    set.seed(12345)
    grupos <- sample(1:k,nrow(datos),replace = TRUE)
    # Pero así, los grupos pueden tener tamaños bastante distintos
    table(grupos)

      # Forma alternativa, grupos balanceados en tamaño
    k <- 12
    n <- nrow(datos)
    nn <- floor(nrow(datos)/k)
    kk <- c(rep(1:k,each=nn),1:k)[1:n]
    set.seed(12345)
    grupos <- kk[sample(1:n,n,replace = FALSE)]
    table(grupos)

    #2.  Ahora ya analizamos los datos, usamos la función errores.pred.todos definida antes,
    # en cada uno de los k grupos de validación
    errores.pred <- NULL
    for (j in 1:k) {
     ajustes.entren <- regsubsets(Balance ~ ., data=datos[grupos != j,], nvmax=11)
     matriz.diseño <- model.matrix(Balance ~ ., data=datos[grupos == j,])
     errores.pred.0 <- errores.pred.todos(ajustes.entren,matriz.diseño,datos$Balance[grupos == j])
     errores.pred <- rbind(errores.pred,errores.pred.0)
    }
    colnames(errores.pred) <- paste('num var',paste(1:ncol(errores.pred)))
    errores.pred
    
    # Podemos calcular medias y errores estandar 
    errores.mean <- apply(errores.pred,2,mean)
    errores.se <- apply(errores.pred,2,sd)/sqrt(nrow(errores.pred))
    errores.mean
    errores.se

    plot(errores.mean,type='b',ylim=c(0,60000))

    #  Regla de un-error-estándar: Principio de parsimonia o Navaja de Ockham
    #     Elegimos el tamaño menor para el que su error de predicción medio
    #        no esté a más de un error estandar del error más pequeño
   qq <- which(errores.mean-errores.se < min(errores.mean))[1]
   qq         # Este es el número de covariables obtenido

   #  Obtención del modelo: 
    ajustes.todo <- regsubsets(Balance ~ ., data=datos, nvmax=11) 
    coef(ajustes.todo, id=qq)



####Bootstrap####

## Bootstrap, un ejemplo: Error estándar de la mediana    
library(ISLR)
head(Credit)
summary(Credit)


# Programándolo nosotros      

set.seed(12345)
var<-Credit$Balance
mediana.datos <- median(var)
funMe<-function(var){
  indice<-sample(0:length(var),length(var),rep=TRUE)
  return(median(var[indice]))
}
B<-10^3
medianas.boot<-replicate(B,funMe(Credit$Balance))
medianas.sesgo <- mediana.datos - mean(medianas.boot)
medianas.dt <- sqrt(sum((mediana.datos-medianas.boot)^2)/(B-1))

# Otra forma 

set.seed(12345)
mediana.datos <- median(Credit$Balance)
medianas.boot<-NULL
B<-10^3
for (i in 1:B){
  indice<-sample(9:nrow(Credit),nrow(Credit),rep=TRUE)
  medianas.boot<-c(medianas.boot,median(Credit$Balance[indice]))
}
sqrt(sum((mediana.datos-medianas.boot)^2)/(B-1))

# Utilizando el paquete boot      
library(boot)
set.seed(12345)
boot.fun <- function(variable,indice) median(variable[indice])
boot(Credit$Balance,boot.fun,B)



## Bootstrap, otro ejemplo: Error estándar de la recta de regresión

library(boot)
attach(salarios)
summary(salarios); boxplot(experiencia, salario)
plot(experiencia, salario)
ajuste <- lm(salario ~ experiencia,data=salarios)
abline(coef(ajuste),col="red")
summary(ajuste)

# Error estándar en un punto dado
x <- 5
predict(ajuste,newdata = data.frame(experiencia=x),
        se.fit=TRUE,interval = 'confidence')



B <- 1000
boot.fun <- function(datos,indice,x=x) {
  coeficientes <- coef(lm(salario ~ experiencia,data=datos,subset=indice))
  return(coeficientes[1]+coeficientes[2]*x)
}
set.seed(12345)
boot(salarios,boot.fun,B,x=x)



# Ahora ajustamos una parábola
ajuste <- lm(salario ~ experiencia + I(experiencia^2),data=salarios)
summary(ajuste)

x <- 5
predict(ajuste,newdata = data.frame(experiencia=x),
        se.fit=TRUE,interval = 'confidence')

B <- 1000
boot.fun <- function(datos,indice,x=x) {
  coeficientes <- coef(lm(salario ~ experiencia + I(experiencia^2),
                          data=datos,subset=indice))
  return(coeficientes[1]+coeficientes[2]*x+coeficientes[3]*x^2)
}
set.seed(12345)
boot(salarios,boot.fun,B,x=x)  # Bastante parecido al resultado paramétrico 







