#################### TEMA 2: Regresión Lineal Múltiple ###############

load("C:/Users/1/Master_CD/Analisis_exploratorio_de_datos/Examen_modelos_lineales/Tema 2 Regresión lineal múltiple-20221115/Solución Ejercicios Tema 2/datosTema2.RData")
ls()

#### Ejemplo Confusores: datos deportistas####

hombres <- subset(deportistas,Genero=='male')
mujeres <- subset(deportistas,Genero=='female')

lm_todos <- lm(PrctGrasa ~ MCMagra, data=deportistas)
coef(lm_todos)
lm_hombres <- lm(PrctGrasa ~ MCMagra, data=hombres)
coef(lm_hombres)
lm_mujeres <- lm(PrctGrasa ~ MCMagra, data=mujeres)
coef(lm_mujeres)

coef(lm(PrctGrasa ~ MCMagra+Genero, data=deportistas))

# Dibujamos todo en una misma gráfica

plot(deportistas$MCMagra,deportistas$PrctGrasa,col='BLUE')
points(mujeres$MCMagra,mujeres$PrctGrasa,col='RED')
abline(coef=coef(lm_todos))
abline(coef=coef(lm_hombres),col='BLUE')
abline(coef=coef(lm_mujeres),col='RED')



#### Ejemplo de mejora en la predicción: datos deportistas ####


lm1 <- lm(PrctGrasa ~ MCMagra, data=deportistas)
summary(lm1)$r.squared
summary(lm1)$sigma


lm2 <- lm(PrctGrasa ~ MCMagra+Genero, data=deportistas)
summary(lm2)$r.squared
summary(lm2)$sigma


## Ejemplo: datos Pesos.csv             

lm_pesos <- lm(Peso ~ Edad + Altura + Verduras, data=Pesos)
coef(lm_pesos)
n<-nrow(Pesos) #nº de datos
p<-3 #nº de variables

# utilidad del modelo: test F global


SS_ajuste<-sum((fitted(lm_pesos)-mean(Pesos$Peso))^2)
SS_error<-sum(residuals(lm_pesos)^2) 
SS_y<-sum((Pesos$Peso-mean(Pesos$Peso))^2)

# se puede comprobar que SS_y=SS_error+SS_ajuste: SS_y-SS_error-SS_ajuste

MS_ajuste<-SS_ajuste/3
MS_error<-SS_error/(100-3-1)  
# se puede comprobar que MS_error es sigma^2: summary(lm_pesos)$sigma^2-MS_error
F<-MS_ajuste/MS_error           #F-statistic
pf(F,3,96,lower.tail=FALSE)     #p-value



#Relevancia de cada predictor:

sigma<-summary(lm_pesos)$sigma
v<-diag(summary(lm_pesos)$cov.unscaled)
t<-coef(lm_pesos)/(sigma*sqrt(v))
pt(q=t[1], df=96,lower.tail=FALSE)*2  # a modo de ejemplo


#Bondad del modelo: R2 ajustado:

R2<-SS_ajuste/(SS_ajuste+SS_error)
  #notar que coincide con: 1-SS_error/SS_y

R2adj<-1-(99/96)*(1-R2)  
  #notar que coincide con 1-(sigma^2)/var(Pesos$Peso) 


#predicción:

x0 <- data.frame(Edad=10, Altura=150, Verduras = c(0.1,0.3,0.7))
predict(lm_pesos,newdata=x0,interval='confidence')
predict(lm_pesos,newdata=x0,interval='prediction')

#### Interacción  ####

## Ejemplo: datos deportistas       

hombres <- subset(deportistas,Genero=='male')
mujeres <- subset(deportistas,Genero=='female')
lm_genero <- lm(PrctGrasa ~ MCMagra + Genero, data=deportistas)
summary(lm_genero)
plot(deportistas$MCMagra,deportistas$PrctGrasa,col='BLUE')
points(mujeres$MCMagra,mujeres$PrctGrasa,col='RED')
abline(coef=coef(lm_genero)[1:2],col='RED')
coef_male <- c(coef(lm_genero)[1]+coef(lm_genero)[3],coef(lm_genero)[2])
abline(coef=coef_male,col='BLUE')

## Continuación. Introducimos el producto para evitar rectas paralelas           
lm_inter <- lm(PrctGrasa ~ MCMagra * Genero, data=deportistas)
summary(lm_inter)
plot(deportistas$MCMagra,deportistas$PrctGrasa,col='BLUE')
points(mujeres$MCMagra,mujeres$PrctGrasa,col='RED')
abline(coef=coef(lm_inter)[1:2],col='RED')
coef_male <- c(coef(lm_inter)[1]+coef(lm_inter)[3],coef(lm_inter)[2]+coef(lm_inter)[4])
abline(coef=coef_male,col='BLUE')

 ## Ejemplo: datos advertising  Efecto interacción   (sinergias)   

head(advertising)
ml1 <- lm(sales ~ TV + radio, data= advertising)
summary(ml1)
ml2 <- lm(sales ~ TV * radio, data= advertising)
summary(ml2)


# equivalencia con el test t y anova
  
lm_CHD <- lm(EDATDIAG ~ CHD, data=diabetes)
summary(lm_CHD)
t.test(EDATDIAG ~ CHD, alternative='two.sided', conf.level=.95, var.equal=T, data=diabetes)


lm_ECG <- lm(EDATDIAG ~ ECG, data=diabetes)
summary(lm_ECG)
mod.anova<-aov(EDATDIAG ~ ECG, data=diabetes)
summary(mod.anova)


#### Modelos Regresión polinómica ####           

library(ISLR)
summary(Auto)
plot(Auto$horsepower, Auto$mpg)
reg1 <- lm(mpg ~horsepower, data=Auto)
summary(reg1)
abline(coef=coef(reg1), col="red")

reg2 <- lm(mpg ~ horsepower + I(horsepower^2), data=Auto)
summary(reg2)


reg5 <- lm(mpg ~ horsepower + I(horsepower^2)+I(horsepower^3)+I(horsepower^4)+I(horsepower^5), data=Auto)
summary(reg5)


plot(Auto$horsepower, Auto$mpg, xlab='Potencia del motor', ylab='Millas por galón', main='Banco de datos Auto')
abline(coef = coef(reg1),col='RED',lwd=2)
xx <- seq(50,250,1)
yy2 <- predict(reg2,newdata=data.frame(horsepower=xx))
lines(xx,yy2,col='BLUE',lwd=2)
yy5 <- predict(reg5,newdata=data.frame(horsepower=xx))
lines(xx,yy5,col='GREEN',lwd=2)
legend("topright", c("Recta", "Parabola", "Grado 5"), lwd=2, col = c('RED','BLUE','GREEN'))


reg8 <- lm(mpg ~ horsepower + I(horsepower^2)+I(horsepower^3)+I(horsepower^4)+I(horsepower^5)+
             I(horsepower^6) + I(horsepower^7)+I(horsepower^8), data=Auto)
summary(reg8)  #ups

reg.poly <- lm(mpg ~ poly(horsepower,8), data=Auto)
summary(reg.poly)


yy8 <- predict(reg.poly,newdata=data.frame(horsepower=xx))
lines(xx,yy8,col='orange',lwd=2)
legend("topright", c("Recta", "Parabola", "Grado 5","poly(8)"), lwd=2, col = c('RED','BLUE','GREEN',"orange"))

#### Selección de modelos ####

## Comparación de modelos

library(ISLR)
head(Credit)
summary(Credit)
ajuste1 <- lm(Balance ~ Age + Rating , data=Credit)
ajuste2 <- lm(Balance ~ Age + Rating + Limit , data=Credit)
summary(ajuste1);summary(ajuste2)

x <- rnorm(nrow(Credit))
ajuste3 <- lm(Credit$Balance ~ Credit$Age + Credit$Rating + x)
summary(ajuste3)
anova(ajuste1,ajuste2,test="F")

#Comparación de modelos: AIC

lm_pesos2 <- lm(Peso ~ Edad + Sexo+ Verduras, data=Pesos)
summary(lm_pesos2)
AIC(lm_pesos, lm_pesos2)


lm_pesos3 <- lm(Peso ~ Edad , data=Pesos)
anova(lm_pesos3,lm_pesos,test="F") # Como el p-valor es menor que 0.05, existe
# algún elemento de los eliminados (Altura o Verduras) que es significativo.

#  Para ver como se calcula el AIC

res <- summary(ajuste1)
n <- nrow(Credit)
d <- length(coef(ajuste1))
var_mle <- res$sigma^2*(n-d)/n
2*(d+1) + n*log(2*pi)+n*log(var_mle)+n  #  AIC = 2*num.par-2*log.lik 
AIC(ajuste1)
AIC(ajuste1, ajuste2)    #  Objetivo: minimizar AIC



#  Para ver como se calcula el BIC
res <- summary(ajuste1)
n <- nrow(Credit)
d <- length(coef(ajuste1))
var_mle <- res$sigma^2*(n-d)/n
log(n)*(d+1) + n*log(2*pi)+n*log(var_mle)+n  #  BIC = log(n)*num.par-2*log.lik 
BIC(ajuste1)
BIC(ajuste1, ajuste2)    #  Objetivo: minimizar BIC



####  Selección del mejor subconjunto ####
library(leaps)
ajuste.todo <- regsubsets(Balance ~ . -ID , data=Credit)
summary(ajuste.todo)

#Incluimos el nvmax=11 por que son 11 variables.
ajuste.todo <- regsubsets(Balance ~ . -ID , data=Credit, nvmax=11) 
(resumen <- summary(ajuste.todo))

resultado <- cbind(resumen$rsq,resumen$adjr2,resumen$cp,resumen$bic)
colnames(resultado) <- c('Rsq','RsqAdj','Cp','BIC')
resultado

par(mfrow = c(1,3))
plot(1:11, resumen$adjr2, xlab = "# Variables", main = "Coef. Det. Ajustado",
     type="b")
abline(v = which.max(resumen$adjr2), col = 2)
plot(1:11, resumen$cp, xlab = "# Variables", main = "Cp de Mallows",
     type='b')
abline(v = which.min(resumen$cp), col = 2)
plot(1:11, resumen$bic, xlab = "# Variables", main = "BIC",
     type = "b")
abline(v = which.min(resumen$bic), col = 2)
par(mfrow=c(1,1))


plot(ajuste.todo,scale='r2')
plot(ajuste.todo,scale='adjr2')
plot(ajuste.todo,scale='bic')
plot(ajuste.todo,scale='Cp')

#coeficientes del mejor modelo
coef(ajuste.todo, id=6)
   
####  Selección stepwise ####
library(ISLR)
Credit_sin_ID <- subset(Credit,select = Income:Balance)
ajuste_0 <- lm(Balance ~ 1 , data=Credit_sin_ID)
ajuste_todo <- lm(Balance ~ . , data=Credit_sin_ID)
traza <- step(ajuste_todo, direction = 'both')

traza <- step(ajuste_todo, direction = 'backward')

traza <- step(ajuste_0, Balance ~ Income + Limit + Rating + 
                Cards + Age + Education + Gender + Student +
                Married + Ethnicity, direction = 'forward')

#  Para ver como se calcula el AIC en step()
n <- nrow(Credit_sin_ID)
d <- length(coef(ajuste_todo))
var_mle <- summary(ajuste_todo)$sigma^2*(n-d)/n
2*(d+1) + n*log(2*pi) + n*log(var_mle)+n  #  AIC = 2*num.par-2*log.lik
AIC(ajuste_todo)      #  AIC completo
2*d + n*log(var_mle)  #  AIC sin constante (el utilizado en step())







