###  Modelo Binomial #####

## Uso de libreria derivmkts 
## quantmod
## Instalar PerformanceAnlatytics
## Instalar quadprog
## Uso de la libreria install.packages("IntroCompFinR", repos="http://R-Forge.R-project.org")

library(quantmod)
library(derivmkts)
library(PerformanceAnalytics)
library(quadprog)
library(IntroCompFinR)


### primer usamos quantmod
simbolos=c("TSLA","XOM","JPM")
getSymbols(simbolos,src="yahoo", from="2019-09-30",to="2020-10-06")
#### Estrategia 
#### Vamos a usar los pesos de un portafolio para decidir 
#### Si compramos opciones call o put

tsla=Delt(TSLA$TSLA.Close)[-1] ### calculamos los rendimientos
xom=Delt(XOM$XOM.Close)[-1]
jpm=Delt(JPM$JPM.Close)[-1]

rend=cbind(tsla,xom,jpm) ## Los juntamos en un dataframe
colnames(rend)=simbolos

mu=colMeans(rend)*20## Rendimientos del activo
varcova=var(rend)*20## Matriz Var covar

### Calcualr una forntera eficiente## Prefunta inicial Compramos call o put
## Vamos a usar IntroCompFinR
portafolio=efficient.frontier(mu, varcova,nport = 20,alpha.min = -0.1,alpha.max = 1.1) 

plot(portafolio$sd,portafolio$er, pch=16, col="blue", cex=2, main="Frontera Eficiente",xlab = "Risk",ylab = "Return")

vol=portafolio$sd[8]
re=portafolio$er[8]
weights=as.data.frame(portafolio$weights[8,])## Positivo Call Negativo Put
rownames(weights)=simbolos
colnames(weights)="weights"


### Valoración opciones

r=0.14813/100 ## Porcentaje anual
rs=(1+r)^(1/52)-1## Tasa semanal

### TSLA ## Prima de mercado 36.35

ST=429
KT=450

volT=sd(tsla)*sqrt(5)## Proyecciones semanales
tt=4
nstep=4
d=0
## Proyección del precio de TSLA
## Usar la libreria  derivmkts 
## crr Cox Ross Rubinstein

binomplot(ST, 0, volT, rs, tt, d, nstep,pointsize =0, crr = TRUE, plotvalues = TRUE,plotarrows = TRUE,titles = FALSE)

### Graficar la opción Call
binomplot(ST, KT, volT, rs, tt, d,nstep,american = TRUE, crr = TRUE, plotvalues = TRUE, plotarrows = TRUE, titles=TRUE)


## CalculR
a=binomopt(ST, KT, volT, rs, tt, d, nstep, american=TRUE, putopt=FALSE, crr = TRUE,returntrees=TRUE, returnparams=TRUE)
a$price



### Proyección Precios XOM
### Calcular el precio de una opción

STXOM=34.10
KXOM=32.00
KXOM1=30.00


volTXOM=sd(xom)*sqrt(5)## Proyecciones semanales
tt=4
nstep=4
d=0

binomplot(STXOM, 0, volTXOM, rs, tt, d, nstep,pointsize =0, crr = TRUE, plotvalues = TRUE,plotarrows = TRUE,titles = FALSE)

### Graficar la opción Put
binomplot(STXOM, KXOM, volTXOM, rs, tt, d,nstep,american = TRUE, putopt=TRUE,crr = TRUE, plotvalues = TRUE, plotarrows = TRUE, titles=TRUE)


## CalculR
b=binomopt(STXOM, KXOM, volTXOM, rs, tt, d, nstep, american=TRUE, putopt=TRUE, crr = TRUE,returntrees=TRUE, returnparams=TRUE)
b$price
b$params
b1=binomopt(STXOM, KXOM1, volTXOM, rs, tt, d, nstep, american=TRUE, putopt=TRUE, crr = TRUE,returntrees=TRUE, returnparams=TRUE)
b1$price


