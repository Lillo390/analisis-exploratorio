#################### TEMA 1: Regresión Lineal Simple ###############


## Leemos los datos
load("C:/Users/1/Master_CD/Analisis_exploratorio_de_datos/Examen_modelos_lineales/Tema 1 Regresión Lineal Simple-20221115/Solución Ejercicios Tema 1/datosTema1.RData")

## El siguiente comando nos permite ver los objetos de la sesión actual.
ls()

str(anfetas) # Muestra la estructura interna del objeto anfetas
with(anfetas,plot(dosis,consumo))

str(Pesos)
with(Pesos,plot(Edad,Peso))

## Estimación coeficientes manual: Peso ~ Edad

x<-Pesos$Edad; y<-Pesos$Peso
Ey<-mean(y,na.rm=T)
Ex<-mean(x,na.rm=T)
SSx<-sum((x-Ex)^2,na.rm=T)
SSxy<-sum((x-Ex)*(y-Ey),na.rm=T)
(b1<-SSxy/SSx); (b0<-Ey-b1*Ex)

## Regresión lineal Peso ~ Edad

regre <- lm(Peso ~ Edad, data=Pesos)    
#regre <- lm(Peso ~ Edad - 1, data=Pesos)  # Sin intercepto  
coefficients(regre)
y_hat <- predict(regre)
residuos <- residuals(regre)
summary(residuos)

# Dibujamos los datos (azul) con la recta de regresión (rojo)
with(Pesos,plot(Edad,Peso,col='BLUE'))
abline(coef=coefficients(regre),col='RED')

## Regresión lineal consumo ~ dosis

# anfetas <- read.csv2('anfetas.csv') # Para cargar los datos desde un csv
reg_1 <- lm(consumo ~ dosis - 1, data=anfetas)    # Sin intercepto
reg <- lm(consumo ~ dosis, data=anfetas)
summary(reg)
confint(reg)

# Dibujamos los datos (azul) con la recta de regresión (rojo)
with(anfetas,plot(dosis,consumo,col='BLUE'))
abline(coef=coefficients(reg),col='RED')


#### Propiedades ####

# La recta pasa por las medias.
sum(coefficients(regre)*c(1,Ex))  
Ey

# Los residuos tienen media 0
sum(residuos)                   


## Cálculo de la varianza residual
n<-dim(na.omit(cbind(Pesos$Peso, Pesos$Edad)))[1] #Cantidad de datos.
                                                  # Igual que n<-100
Se2<-sum(residuos^2)/(n-2)             

## Cálculo del coeficiente de determinación

SSydadox<-sum(residuos^2)
SSy<-sum((y-Ey)^2,na.rm=T)
R2<-1-SSydadox/SSy                # bondad del ajuste



## Correlacion
with(Pesos,cor(Edad,Peso))
with(Pesos,cor.test(Edad,Peso))
cor(subset(Pesos, select = -Sexo)) 
# Si intentamos hacer la correlación directamente cor(Pesos), sale un error
# ya que la variable Sexo no es numérica

# Se pueden quitar varias variables de la siguiente forma: 
cor(subset(Pesos, select = c(-Sexo,-Edad))) 


cor(anfetas)
with(anfetas,cor.test(dosis,consumo))
with(anfetas,cor(dosis,consumo))

# Lo hacemos con anfetas2  (anfetas pero cambiando la dosis)
with(anfetas2,cor(dosis,consumo))

par.tipicos <- par()
par(mfcol=c(1,2))

reg <- lm(consumo ~ dosis, data=anfetas)
with(anfetas2,plot(dosis,consumo,xlim=c(0,5),ylim=c(30,120)))
abline(coef=coefficients(reg),col='RED')
text(0.5,40,'r= -0.74')
with(anfetas,plot(dosis,consumo,xlim=c(0,5),ylim=c(30,120)))
abline(coef=coefficients(reg),col='RED')
text(0.5,40,'r= -0.90')



#### Distribuciones en el muestreo ####
b0 <- 100
b1 <- 8
dt <- 5
n <- 20
x <- rnorm(n,10,2) #(20 valores de una distribución normal de parámetros
                   # media = 10 y desviación típica = 2)

N <- 100
betas <- NULL
var_resid <- NULL
for (i in 1: N) {
  y <- rnorm(n,b0+b1*x,dt) # generamos 20 valores de una distribución normal
  # de parámetros media = b0+b1x y desviación típica dt = 5
  reg_lin <- lm(y ~ x)
  betas <- rbind(betas,coefficients(reg_lin)) # guardamos los coeficientes de la
  # regresión en cada muestra generada.
  var_resid <- c(var_resid,sum(resid(reg_lin)^2)/(n-2)) # guardamos la varianza
  # residual en cada muestra generada.
}

mean(betas[,1]); mean(betas[,2]); mean(var_resid) # Estimadores insesgados



par.tipicos <- par()
par(mfcol=c(1,2))

plot(betas,xlab='Intercepto',ylab='pendiente')
points(b0,b1,col='RED',pch=19)

plot(var_resid,betas[,2],xlab='Varianza',ylab='pendiente')
points(dt^2,b1,col='RED',pch=19)


#### Intervalos de confianza para los coeficientes ####

regre <- lm(Peso ~ Edad, data=Pesos) 
confint(regre,level=0.95)

#### Contrastes: contraste de Linealidad ####
summary(regre) 

x<-Pesos$Edad; n<-length(Pesos$Edad); 
Sx2<-var(x)
residuos <- residuals(regre)
Se2<-sum(residuos^2)/(n-2)  
SEb1<-sqrt(Se2/(n*Sx2))
t<-b1/SEb1
pt(t,n-2, lower.tail = F)


## Regresion lineal en R:

regreA <- lm(Peso ~ Altura, data=Pesos)   
summary(regreA)
confint(regreA)

regreA2 <- lm(Peso ~ Altura-1, data=Pesos)   
summary(regreA2)
confint(regreA2)


reg <- lm(consumo ~ dosis, data=anfetas)   
summary(reg)
confint(reg)

reg.2 <- lm(consumo ~ dosis - 1, data=anfetas)   
summary(reg.2)
confint(reg.2)



#### Bandas de confianza y de predicción ####

                     
regre <- lm(Peso ~ Edad, data=Pesos)
edades <- data.frame(list(Edad = c(5,8,10)))
predict(regre, newdata = edades, interval = "confidence")
predict(regre, newdata = edades, interval = "prediction")


plot(0,0,xlim=c(5,11),ylim=c(10,25),type='n',xlab='Edad',ylab='Peso')
abline(coef = coefficients(regre), lwd=2)
edades <- data.frame(list(Edad = seq(4,15,length.out = 200)))
bandas <- predict(regre, newdata = edades, interval = "confidence")
lines(edades$Edad,bandas[,2],col='BLUE')
lines(edades$Edad,bandas[,3],col='BLUE')
bandas <- predict(regre, newdata = edades, interval = "prediction")
lines(edades$Edad,bandas[,2],col='RED')
lines(edades$Edad,bandas[,3],col='RED')




#### Algunas notas ####

# variables explicativas dicotómicas


table(Pesos$Sexo)
regre2<-lm(Peso~Sexo,data=Pesos) #categoría de referencia: niña
summary(regre2)

Pesos$niño<-as.numeric(Pesos$Sexo)-1
table(Pesos$niño)
regre2.niño<-lm(Peso~niño,data=Pesos)
summary(regre2.niño)

Pesos$niña<-ifelse(Pesos$niño, 0,1)  #si deseamos cambiar la categoría de referencia
table(Pesos$niña)
regre2.niña<-lm(Peso~niña,data=Pesos)
summary(regre2.niña)


# Variabilidad en la exposición. Cuanto más dispersa es la x, más precisa es 
# la estimación de la pendiente.

a <- 10
b <- 8
dt <- 10
n <- 50
x1 <- rnorm(n,10,1)
x2<-rnorm(n,10,10)
y1 <- rnorm(n,a+b*x1,dt)
y2 <- rnorm(n,a+b*x2,dt)


summary(lm(y1~x1))
summary(lm(y2~x2))  #mucho mejor 2, ya que la x tiene más varianza.


## Diagnóstico del modelo




#### Linealidad ####

set.seed(12345) # para poder reproducir resultados
x <- rnorm(100,20,5)
y1 <- 5+0.5*x + rnorm(100,0,1)
y2 <- 5+0.5*x + 0.5*x^2 + rnorm(100,0,10)
plot(x,y1)
plot(x,y2)

par.tipicos <- par()
par(mfcol=c(1,2))
reg1 <- lm(y1 ~ x)
plot(reg1$fitted.values,reg1$residuals, col='BLUE',
     xlab = 'Predichos',ylab = 'Residuos', main = 'Lineal')
abline(h=0,lty=2)
reg2 <- lm(y2 ~ x)
plot(reg2$fitted.values,reg2$residuals, col='BLUE',
     xlab = 'Predichos',ylab = 'Residuos', main = 'Cuadrático')
abline(h=0,lty=2)
par(par.tipicos)


#### Homocedasticidad ####

set.seed(12345) 
x <- rnorm(100,20,5)
y1 <- 5+0.5*x + rnorm(100,0,1)
y2 <- 5+0.5*x + rnorm(100,0,x^3/10)
plot(x,y1)
plot(x,y2)

par.tipicos <- par()
par(mfcol=c(1,2))
reg1 <- lm(y1 ~ x)
plot(reg1$fitted.values,reg1$residuals, col='BLUE',
     xlab = 'Predichos',ylab = 'Residuos', main = 'Homocedástico')
abline(h=0,lty=2)
reg2 <- lm(y2 ~ x)
plot(reg2$fitted.values,reg2$residuals, col='BLUE',
     xlab = 'Predichos',ylab = 'Residuos', main = 'Heterocedástico')
abline(h=0,lty=2)



#### Normalidad ####

set.seed(12345)
x <- rnorm(100,20,5)
y <- rgamma(100,1,0.5)

par.tipicos <- par()
par(mfcol=c(1,2))

qqnorm(x, col='BLUE', main = 'Datos normales')
qqline(x)
qqnorm(y, col='BLUE', main = 'Datos Gamma(1,0.5)')
qqline(y)


## Ejemplo: con datos anfetaminas

reg <- lm(consumo ~ dosis, data=anfetas)

par.tipicos <- par()
par(mfcol=c(1,2),mar=c(4,4,1,1))

plot(reg$fitted.values,reg$residuals, col='BLUE',
     xlab = 'Predichos',ylab = 'Residuos')
abline(h=0,lty=2)

qqnorm(reg$residuals, col='BLUE', main='')
qqline(reg$residuals)



 ## Ejemplo: Uso de transformaciones

reg <- lm(GFR ~ Creatinina, data=GFR)
plot(GFR$Creatinina,GFR$GFR)
abline(coef=coefficients(reg), col='BLUE')
reg1 <- lm(GFR ~ I(1/Creatinina), data=GFR)

par.tipicos <- par()
par(mfcol=c(1,2),mar=c(4,4,1,1))
plot(GFR$Creatinina,GFR$GFR, xlab = 'Creatinina', ylab='GFR')
abline(coef=coefficients(reg), col='BLUE')
xx <- seq(1,25,0.1)
nuevos <- data.frame(list(Creatinina=xx))
yy <- predict(reg1,newdata = nuevos)
lines(xx,yy,col='RED')
plot(1/GFR$Creatinina,GFR$GFR, xlab = '1 / Creatinina', ylab='GFR')
abline(coef=coefficients(reg1), col='RED')


####  Alternativas más flexibles: KNN regression ####

plot(GFR$Creatinina,GFR$GFR, xlab='Creatinina', ylab='GFR')
xx <- seq(1,25,0.1)
creat <- data.frame(list(Creatinina = xx))
reg <- FNN::knn.reg(GFR$Creatinina, creat, y=GFR$GFR, k=5)
lines(xx,reg$pred,col='RED')



####  Alternativas más flexibles: loess ####

reg <- loess(GFR ~ Creatinina, data=GFR, span = 0.75) # Es el span por defecto 75% de los datos
xx <- seq(1,25,0.1)
creat <- data.frame(list(Creatinina = xx))
plot(GFR$Creatinina,GFR$GFR, xlab='Creatinina', ylab='GFR')
prediccion <- predict(reg, newdata = creat, se = T)
lines(xx[!is.na(prediccion$fit)],
      prediccion$fit[!is.na(prediccion$fit)],col='RED')

  


