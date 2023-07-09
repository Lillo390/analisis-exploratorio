#### Tema 4. Métodos de regularización, Regresión Logística y clasificación####

load("C:/Users/1/Master_CD/Analisis_exploratorio_de_datos/Examen_modelos_lineales/Tema 4. Métodos de regularización, Regresión Logística y clasificación-20221115/teoría/datosTema4.RData")
ls()

#### Regresión Ridge y Lasso ####

library(ISLR)
summary(Hitters)
x <- model.matrix( Salary~., Hitters)[, -1] # Es muy útil, crea la matriz X
# correspondiente a los predictores y, además, transforma automáticamente 
# las variables cualitativas en variables dummy. Hemos puesto el -1 para no tener en cuenta el 
# intercepto.
y <- Hitters[!is.na(Hitters$Salary),]$Salary # Para quedarnos con los valores que no son NA.

library(glmnet)
grid<-10^seq(10,-2,length=100)
ridge.mod <-glmnet(x, y,alpha=0, lambda =grid) #alpha=0 para Ridge, alpha=1 para Lasso

coef(ridge.mod) # Matriz con 20 filas (una por cada beta) y 100 columnas (una por cada lambda)

# Para lambda en la posición 50
ridge.mod$lambda[50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2)) # Suma de los betas al cuadrado (sin el intercepto)

# Para lambda en la posición 60
ridge.mod$lambda[60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1,60]^2)) # Suma de los betas al cuadrado (sin el intercepto) Vemos que 
# es mucho mayor.

# Para encontrar el mejor lambda podemos utilizar la validación cruzada (cv.glmnet)

# La función ejecuta glmnet nfolds+1 veces (si no se pone nada nfolds=10);
# la primera para obtener la secuencia lambda, y luego el resto para calcular 
# el ajuste con cada uno de los grupos omitidos.
# El error se acumula, y se calcula el error medio y la desviación estándar sobre los grupos.
# Hay que tener en cuenta también que los resultados de cv.glmnet son aleatorios, ya que los grupos 
# se seleccionan al azar. Los usuarios pueden reducir esta aleatoriedad ejecutando
# cv.glmnet muchas veces, y promediando las curvas de error.


set.seed(12345)
cv.out<-cv.glmnet(x,y,alpha=0)
plot(cv.out)
bestlam<-cv.out$lambda.min #lambda.min -> da el lambda que minimiza el error en media cuadrática
bestlam

plot(cv.out$lambda,cv.out$cvm) # cvm -> da el error en media cuadrática

plot(cv.out$lambda[0:10],cv.out$cvm[0:10])

# También podriamos calcular errores de predicción:

set.seed(12345)
seleccion <- sample(nrow(x),round(nrow(x)*3/4))
prueba<-(-seleccion)

cv.out<-cv.glmnet(x[seleccion,],y[seleccion],alpha=0)
bestlam<-cv.out$lambda.min
bestlam

# Podemos ver el error de predicción con los valores prueba:

ridge.pred<-predict(ridge.mod, s=bestlam,newx=x[prueba,])
mean((ridge.pred - y[prueba])^2)

# Si probamos con un lambda mucho más grande el error también aumenta

ridge.pred<-predict(ridge.mod, s=10^10,newx=x[prueba,])
mean((ridge.pred - y[prueba])^2)


###  Ejemplo Plasma en Regresión logística

library("HSAUR3")
lineal <- lm(as.numeric(ESR) ~ fibrinogen, data = plasma)
plot(plasma$fibrinogen,plasma$ESR, xlim=c(1,6), ylim=c(0,3), col='RED')
abline(coef=coef(lineal))


## Ajuste del modelo


plasma_glm_1 <- glm(ESR ~ fibrinogen, data = plasma,family = binomial())
summary(plasma_glm_1)

coef(plasma_glm_1)  # coeficientes
confint(plasma_glm_1)  # intervalos de confianza

# Podemos ver el efecto con el OR
exp(coef(plasma_glm_1)["fibrinogen"])  # ORs
exp(confint(plasma_glm_1))  # intervalos de confianza para OR.

# Predicción y residuos

predict(plasma_glm_1) #valores predichos de la combinación lineal de predictores: logs-odds. 
#coef(plasma_glm_1)[1]+coef(plasma_glm_1)[2]*plasma$fibrinogen
# idéntico a plasma_glm_1$linear

predict(plasma_glm_1, type="response")  #valores predichos en la escala de la respuesta:
# probabilidades de éxito. (en este caso el éxito es ESR>20)
# idéntico a fitted(plasma_glm_1)  o plasma_glm_1$fitted.values

resid(plasma_glm_1, type="response")  # residuos: hay varios tipos: "response", "pearson",..
# response  (comparan con la respuesta)

nuevos <-  data.frame(fibrinogen=c(2.2,3.0,4.5)) 
predict(plasma_glm_1, nuevos, se.fit=TRUE) 
predict(plasma_glm_1, nuevos, type="response") 

# Clasificación en la predicción de todos los valores:
prob<-predict(plasma_glm_1, type="response")
pred <- factor(prob>0.5,labels= levels(plasma$ESR))

table(plasma$ESR,pred) # Tabla de clasificación. Vemos que hemos acertado 28 datos de los 32.



####Notas####

#1 NO necesito que la respuesta esté declarada como factor, pero si es numérica sólo 0's y 1's
as.numeric(plasma$ESR)
plasma_glm_1 <- glm(I(as.numeric(ESR)-1) ~ fibrinogen, data = plasma,family = binomial())
summary(plasma_glm_1)  

#2 Cuidado! para una interpretación correcta debo saber que considera el R 
# como un éxito (segundo nivel del factor)

library(car)
plasma$res1<-recode(plasma$ESR,"'ESR < 20'='si';'ESR > 20'='no'")
plasma_glm_alreves <- glm(res1 ~ fibrinogen, data = plasma,family = binomial())
summary(plasma_glm_alreves)
table(as.numeric(plasma$ESR)-1) # Esto es lo que hemos usado en el anterior.
                                # Consideramos el éxito el 1
table(plasma$ESR)   #para reconocer al éxito (categoría con mayor valor numérico)



######Bondad de ajuste: Contrastar si la deviance es nula (no hay alejamiento del modelo perfecto)####


rm(list=ls())
library("HSAUR3")
plasma_glm_1 <- glm(I(as.numeric(ESR)-1) ~ fibrinogen, data = plasma,family = binomial())
summary(plasma_glm_1) 

#ajuste aceptable? 


pchisq(plasma_glm_1$deviance,plasma_glm_1$df.residual,lower=FALSE)
# Busco aceptar deviance=0. En este caso, p-valor
# mayor que 0.05 no rechazo que la deviance es 0-> el modelo ajusta :D


# Otra forma: ¿ajuste nulo? comparación del modelo actual con el nulo
# (hipótesis nula: ambos modelos son iguales, el nulo y el nuestro)

anova(plasma_glm_1, test ='Chisq')  
# rechazo que mi modelo sea nulo, por lo que es significativamente diferente. 



######Gráficos en regresión logística simple####

# a priori - descriptiva

library(ggplot2)
ggplot(data = plasma, mapping = aes(x = fibrinogen, y = ESR)) +
  geom_boxplot(aes(color = fibrinogen)) +
  geom_point(aes(color = fibrinogen)) +
  theme_bw() +
  theme(legend.position = "null")

# a posteriori - Intervalos de confianza

nuevosf <- seq(from = min(plasma$fibrinogen), to = max(plasma$fibrinogen), length=100)
predicciones <- predict(plasma_glm_1, newdata = data.frame(fibrinogen = nuevosf), 
                        se.fit = TRUE, type = "response")
CI_inferior <- predicciones$fit - 1.96 * predicciones$se.fit
CI_superior <- predicciones$fit + 1.96 * predicciones$se.fit
datos_curva <- data.frame(fibrinogen = nuevosf, probabilidad = predicciones$fit, 
                          CI.inferior = CI_inferior, CI.superior = CI_superior)

plasma$ESR<- ifelse(plasma$ESR == "ESR < 20", yes = 0, no = 1)

ggplot(plasma, aes(x = fibrinogen, y = ESR)) +
  geom_point(aes(color = as.factor(ESR)), shape = "I", size = 3) +
  geom_line(data = datos_curva, aes(y = probabilidad), color = "firebrick") +
  geom_line(data = datos_curva, aes(y = CI.superior), linetype = "dashed") +
  geom_line(data = datos_curva, aes(y = CI.inferior), linetype = "dashed") +
  labs(title = "Modelo logístico ESR ~ fiibrinogeno", 
       y = "P(ESR = >20 |fibrinogeno)", 
       x = "fibrinogeno") +
  scale_color_manual(labels = c("<20", ">20"), values = c("blue", "red")) +
  guides(color=guide_legend("ESR")) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme_bw()


######Diagnóstico: Igual que en regresión lineal ####


rm(list=ls())
library("HSAUR3")
plasma_glm_1 <- glm(ESR ~ fibrinogen, data = plasma,family = binomial())
summary(plasma_glm_1) 

## diagnóstico visual

par(mfrow=c(2,2))
plot(plasma_glm_1)
plot(fitted(plasma_glm_1),residuals(plasma_glm_1))

## numéricos

summary(influence.measures(plasma_glm_1)) #influencia, 23, 15 y 29 (el 13 también podriamos considerarlo)

p<-3
n<-dim(plasma)[1]
par(mfrow=c(1,2))
boxplot(hatvalues(plasma_glm_1))
abline(h=2*p/n,col="red",lwd=1);  
abline(h=3*p/n,col="red",lwd=3);

boxplot(cooks.distance(plasma_glm_1))
abline(h=1,col="red",lwd=1);  


#######Comparación de modelos####

# anidados: comparación de deviances

plasma_glm_2<-update(plasma_glm_1,~.+globulin)
summary(plasma_glm_2)

anova(plasma_glm_1,plasma_glm_2, test ='Chisq')  #globulin no añade nada. 

#no anidados: AIC y/o BIC

AIC(plasma_glm_1,plasma_glm_2)
BIC(plasma_glm_1,plasma_glm_2)

##############Regresión múltiple####

# Ejemplo 1: Interacciones

library("HSAUR3")
ajuste <- glm(ESR ~ fibrinogen*globulin, data = plasma, family=binomial())
summary(ajuste) 


# Ejemplo 2

library(ISLR)
data(Default)
head(Default)

aj1<-glm(default~student,data=Default,family=binomial())
summary(aj1)
pred1<-predict(aj1,data.frame(student=c('No','Yes')), type = 'response') 
pred1#lo hacemos para predecir para el No y para el Sí en el modelo logístico


aj2<-glm(default~.,data=Default,family=binomial())
summary(aj2)
exp(coef(aj2)['studentYes']) # La probabilidad para estudiante es casi la mitad que para no estudiante.

predict(aj2,data.frame(student=c('No','Yes'),balance=1500,income=40000), type = 'response') 


################ANALISIS DISCRIMINANTE########


library(datasetsICR)
data(wine)
str(wine)
datw1<-wine[,1:5]

table(datw1$Class )  #clases de vino. 

names(datw1)<-c("Clase", "Alcohol","AMalic","Ash", "Alca")
datw1$Clase<-factor(datw1$Clase) #mejor si factor. 
n<-nrow(datw1)

# Veamos a priori que sucede gráficamente

library(ggplot2)
library(ggpubr)

plot1 <- ggplot(data = datw1, aes(x = Alcohol)) +
  geom_density(aes(colour = Clase)) + theme_bw()
plot2 <- ggplot(data = datw1, aes(x = AMalic)) +
  geom_density(aes(colour = Clase)) + theme_bw()
plot3 <- ggplot(data = datw1, aes(x = Ash)) +
  geom_density(aes(colour = Clase)) + theme_bw()
plot4 <- ggplot(data = datw1, aes(x = Alca)) +
  geom_density(aes(colour = Clase)) + theme_bw()
# la función grid.arrange del paquete grid.extra permite ordenar
# graficos de ggplot2
library(gridExtra)
ggarrange(plot1, plot2, plot3, plot4, common.legend = TRUE)

#Parece que la que más diferencias tiene entre las tres es Alcohol.
# sugiere eliminar Ash


pairs(x = datw1[, c("Alcohol","AMalic","Ash","Alca")],
      col = c("1", "2","3")[datw1$Clase], pch = 19)

# El par Alcohol, Alca parece que separa bien las 3 clases
# Ash está correlada con Alca, eliminar Ash


#### Estimación de los parámetros de la función de densidad
# y cálculo de la función discriminante.

library(MASS)
m_lda <- lda(Clase ~Alcohol+AMalic+Ash+Alca, data=datw1)
predLDA<-predict(m_lda, newdata=datw1[,2:5])
t<-table(datw1$Clase, predLDA$class, dnn = c("Clase real", "Clase predicha"))
t
100*sum(diag(t))/n # Aquí simplemente vemos cuantos hemos "acertado". Es decir,
# que precisión tenemos

# Una vez obtenidas las funciones discriminantes, se puede clasificar un 
# nuevo vino en función del resto de variables. Por ejemplo:
predict(m_lda, newdata=data.frame(Alcohol=13,AMalic=1.8,Ash=2.5,Alca=16))

# El resultado muestra que, según la función discriminante, la probabilidad 
# posterior de que el el vino pertenezca a la clase 1 es de 0.9644283
# frente al 0.01213084 y el 0.02344081 de las clases 2 y 3.

#########Comprobación de hipótesis:####

## normalidad multivariante

library(MVN)

royston_test <- mvn(data = datw1[,2:5], mvnTest = "royston", multivariatePlot = "qq")
royston_test$multivariateNormality
royston_test$univariateNormality


## Igualdad de matrices de covarianza

library(biotools)
boxM(data =datw1[, 2:5], grouping = datw1$Clase) #se rechaza


### posibilidad: Eliminamos la variable Ash


m_lda2 <- lda(Clase ~Alcohol+AMalic+Alca, data=datw1)
predLDA<-predict(m_lda2, newdata=datw1[,c("Alcohol","AMalic","Alca")])
t2<-table(datw1$Clase, predLDA$class, dnn = c("Clase real", "Clase predicha"))
t2
100*sum(diag(t2))/n # Hemos empeorado un poco en nuestra predicción


royston_test2 <- mvn(data = datw1[,c("Alcohol","AMalic","Alca")], mvnTest = "royston", multivariatePlot = "qq")
royston_test2$multivariateNormality
royston_test2$univariateNormality 


boxM(data =datw1[, c("Alcohol","AMalic","Alca")], grouping = datw1$Clase)  #Ha mejorado 
# un poco pero no significativamente.


# Quitamos AMalic también, para ver que pasa
m_lda3 <- lda(Clase ~Alcohol+Alca, data=datw1)
predLDA<-predict(m_lda2, newdata=datw1[,c("Alcohol","Alca")])
t3<-table(datw1$Clase, predLDA$class, dnn = c("Clase real", "Clase predicha"))
t3
100*sum(diag(t3))/n #No hemos empeorado

royston_test3 <- mvn(data = datw1[,c("Alcohol","Alca")], mvnTest = "royston", multivariatePlot = "qq")
royston_test3$multivariateNormality
royston_test3$univariateNormality  #Hemos mejorado! Aunque sigue sin ser normal.

boxM(data =datw1[, c("Alcohol","Alca")], grouping = datw1$Clase)  #mejor, aunque sea un poco.

##########Análisis cuadrático: Como no se cumplen las hipótesis, quizá sea mejor un análisis cuadrático####

m_qda <- qda(Clase ~Alcohol+Alca+Ash+AMalic, data=datw1)
predQDA<-predict(m_qda, newdata=datw1[,c("Alcohol","Alca","Ash","AMalic")])
t4<-table(datw1$Clase, predQDA$class, dnn = c("Clase real", "Clase predicha"))
t4
100*sum(diag(t4))/n

predict(m_qda, newdata=data.frame(Alcohol=13,AMalic=1.8,Ash=2.5,Alca=16))
