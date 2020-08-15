library(Quandl)
# Para obtener la api_key debe tener un usuario en https://www.quandl.com

# Petroleo

WTI=Quandl("EIA/PET_RWTC_D", api_key="zxdSEzha_e_UwhD8Pgdw", type = "xts")

CL1=Quandl("CHRIS/CME_CL1", api_key="zxdSEzha_e_UwhD8Pgdw",type="xts") 
CL7=Quandl("CHRIS/CME_CL7", api_key="zxdSEzha_e_UwhD8Pgdw",type="xts") 
CL3=Quandl("CHRIS/CME_CL3", api_key="zxdSEzha_e_UwhD8Pgdw",type="xts") 

Petroleo=cbind(WTI, CL1$Last, CL3$Last, CL7$Last)
colnames(Petroleo)=c("WTI","CL1","CL3","CL7")

Petroleo=tail(Petroleo,550)

library(xlsx)
write.xlsx(Petroleo, "Petroleo.xlsx") 



Basis1=WTI-CL1$Last
Basis3=WTI-CL3$Last
Basis7=WTI-CL7$Last

plot(tail(WTI,250), col="red", main= "Spot vs Futures")
lines(tail(CL1$Last,250), col="blue")
lines(tail(CL7$Last,250), col ="green")
lines(tail(CL3$Last,250), col ="orange")
legend("bottom",legend=c("WTI","CL1","CL3","CL7"), col=c("red","blue","orange","green")) 


plot(tail(Basis1,250),col="red", main= "Basis CL")
lines(tail(Basis3,250), col="blue")
lines(tail(Basis7,250), col ="green")


# Oro 


GOLD=Quandl("LBMA/GOLD", api_key="zxdSEzha_e_UwhD8Pgdw", type="xts")
GC1=Quandl("CHRIS/CME_GC1", api_key="zxdSEzha_e_UwhD8Pgdw", type="xts")
GC5=Quandl("CHRIS/CME_GC5", api_key="zxdSEzha_e_UwhD8Pgdw", type="xts")
GC10=Quandl("CHRIS/CME_GC10", api_key="zxdSEzha_e_UwhD8Pgdw", type="xts")
colnames(GOLD)=c("USD1","USD2","GBP1","GBP2","EUR1","EUR2") 

plot(tail(GOLD$USD1,250), col="red", main= "Spot vs Futures")
lines(tail(GC1$Last,250), col="blue")
lines(tail(GC5$Last,250), col ="green")
lines(tail(GC10$Last,250), col ="orange")
