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
getSymbols(simbolos,src="yahoo", from="2019-09-30",to="2020-09-30")
#### Estrategia 
#### Vamos a usar los pesos de un portafolio para decidir 
#### Si compramos opciones call o put

tsla=Delt(TSLA$TSLA.Close)[-1] ### calculamos los rendimientos
xom=Delt(XOM$XOM.Close)[-1]
jpm=Delt(JPM$JPM.Close)[-1]

rend=cbind(tsla,xom,jpm) ## Los juntamos en un dataframe
colnames(rend)=simbolos



