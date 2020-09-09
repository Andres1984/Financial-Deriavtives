### Graficas de opciones

## Call Opción de compra V(S,t)=max(S-K,0)
## Put Opción de venta V(S,t)=max(K-S,0)
S=seq(300,600,by=1)# Precio mínimo 300 y Precio Maximo 600
K=450 #Precio ejercicio
cv=NULL
pv=NULL  
for (i in 1:length(S)){
  cv[i]=max(S[i]-K,-1.74)
  pv[i]=max(K-S[i],-1.74)
}

plot(S,cv,type="l", col="orange", main="Opciones",xlab="Precio de la acción", ylab="V(S,t)")
lines(S,pv,col="blue")
abline(h=0)
