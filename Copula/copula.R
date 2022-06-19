library(copula)
library(subcopem2D)
library(ADGofTest)
library(fitdistrplus)
library(VineCopula)


datos<-read.csv("datos.csv")



op<-par(mfrow=c(1,2))
hist(datos$X,prob=TRUE,col="green",main="Histograma de X")
hist(datos$Y,prob=TRUE,col="green",main="Histograma de Y")
summary(datos$X)
summary(datos$Y)


op<-par(mfrow=c(1,2))
matUV<-apply(datos,2,rank)/nrow(datos)
plot(datos,main="datos observados")
plot(matUV,main="pseudo-observaciones")



cor(datos, method = "pearson")[1, 2]    # Pearson
cor(datos, method = "spearman")[1, 2]   # Spearman
cor(datos, method = "kendall")[1, 2]    # Kendall




SC <- subcopemc(datos,50,display = FALSE)     
SC$depMon                               # Medida de dependencia monotona 
SC$depSup                               # Medida Supremo




fw<-fitdist(datos$X, "beta")
summary(fw)$estimate    # Parametros estimados
plot(fw)                # Graficamos el histograma y le ecimamos la desidad beta con los parametros




para1<-as.numeric(summary(fw)$estimate[1])
para2<-as.numeric(summary(fw)$estimate[2])
ad.test(datos$X, pbeta, para1, para2)$p.value



y.transfor<-(datos$Y+24)/24
fw2<-fitdist(y.transfor, "beta")
summary(fw2)$estimate    # Parametros estimados
plot(fw2)                # Graficamos el histograma y le ecimamos la desidad beta con los parametros


para11<-as.numeric(summary(fw2)$estimate[1])
para22<-as.numeric(summary(fw2)$estimate[2])
ad.test(y.transfor, pbeta, para11, para22)$p.value



matXY<-cbind(datos$X,y.transfor) # matriz de las obs de X y las obs de Y con la transformacion




## Ajuste parametrico de Copula bivariada-------------------------------------------------------
Prueba.Frank<-gofCopula(frankCopula(),matUV,N=50) # Prueba de Bondad de Ajuste para una Frank
Prueba.Frank                                      # valor del parametro  y el p-value
Frank<-frankCopula(Prueba.Frank$parameter)        # Creamos una copula con el parametro obtenido
matUV.Frank<-rCopula(nrow(matUV),Frank)           # simulamos pseudo observaciones a partir de la copula
op<-par(mfrow=c(1,2))                             
plot(matUV,main="Pseudo-obs")                     # comparamos las pseudo observaciones
plot(matUV.Frank,main="Pseudo-obs Frank")         # visulamente no se ve una "gran diferencia"



distXY<-mvdc(Frank, c("beta","beta"),list( list(shape1=para1,shape2=para2), 
                                           list(shape1=para11,shape2=para22)))
matXY.Frank<-rMvdc(nrow(datos),distXY)




op<-par(mfrow=c(2,3))
plot(matXY,main="muestra obs",xlim=c(0,1),ylim=c(0,1))
hist(datos$X,main="obs X",prob=TRUE,xlim=c(0,1))
hist(y.transfor,prob=TRUE,main="Y obs",xlim=c(0,1))
plot(matXY.Frank,main="muestra sim",xlim=c(0,1),ylim=c(0,1))
hist(matXY.Frank[,1],prob=TRUE,main="X sim",xlim=c(0,1))
hist(matXY.Frank[,2],prob=TRUE,main="Y sim",xlim=c(0,1))





summary(datos$X)                   # x obs
summary(matXY.Frank[,1])           # X sim
summary(y.transfor)                # Y obs 
summary(matXY.Frank[,2])           # Y sim



cor(matXY, method = "pearson")[1, 2]          # Pearson obs
cor(matXY.Frank, method = "pearson")[1, 2]    # Pearson sim

cor(matXY, method = "spearman")[1, 2]         # Spearman obs
cor(matXY.Frank, method = "spearman")[1, 2]   # Spearman sim

cor(matXY, method = "kendall")[1, 2]          # Kendall obs
cor(matXY.Frank, method = "kendall")[1, 2]    # Kendall sim



XY<- subcopemc(matXY,50,display = FALSE)    
XY.Frank <- subcopemc(matXY.Frank,50,display = FALSE)




XY$depMon                               # Medida de dependencia monotona obs
XY.Frank$depMon                         # Medida de dependencia monotona sim



XY$depSup                               # Medida Supremo obs
XY.Frank$depSup                         # Medida Supremo sim




a<-as.numeric(Prueba.Frank$parameter)
cop <- BiCop(family = 5, par = a)         ## con el numero 5 es como reconoce una copula Frank




raiz <- function(u) {     # Esta funcion devuelve el v tal que [dC(u,v)/du|Fx(x)=u,Fy(y)=v] = 1/2   
  uniroot(
    function(v) BiCopHfunc2(v, u, cop) - 1/2, interval = c(0,1),lower=0,upper=1)$root
}



u<-sort(matUV.Frank[,1])
v<-sapply(u,raiz)
X<-qbeta(u,para1,para2)        # obtenemos x = Fx^(-1)(u)
Y<-qbeta(v,para11,para22)      # obtenemos y = Fy^(-1)(v)
Y.ori<-24*Y -24                ## le aplico la inversa de la transformacion 
op<-par(mfrow=c(1,1))          ## Vamos a Graficar los datos obs y la curva de regresion
plot(datos,xlab="X obs",ylab="Y obs",main="Datos y la Curva de Regresion")                           
lines(X,Y.ori,type="l",col="red",lwd=2)


