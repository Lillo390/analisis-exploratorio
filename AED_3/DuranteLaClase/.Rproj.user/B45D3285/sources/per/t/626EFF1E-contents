datos<-data(package="MASS")

a<-datos$results
conjuntosDatos<-a[,"Item"]
conjuntosDatos<-matrix(conjuntosDatos, ncol=1)

TodosNumericos<-function(x){
  data<-get(x) #Carga el conjunto de datos
  I<-all(sapply(data, is.numeric)==TRUE)
}

R<-apply(conjuntosDatos, 1, TodosNumericos)
ConjuntosNumericos<-conjuntosDatos[R,]

############# TIDY R#############
library(tidyr)
data(package="tidyr")
View(table1)
