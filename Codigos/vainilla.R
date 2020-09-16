### Graficas de opciones

## Call Opción de compra V(S,t)=max(S-K,0)
## Put Opción de venta V(S,t)=max(K-S,0)
S=seq(300,600,by=1)# Precio mínimo 300 y Precio Maximo 600
K=450 #Precio ejercicio
cv=NULL
pv=NULL  
for (i in 1:length(S)){
  cv[i]=max(S[i]-K-1.74,0)
  pv[i]=max(K-S[i]-1.74,0)
}

plot(S,cv,type="l", col="orange", main="Opciones",xlab="Precio de la acción", ylab="V(S,t)")
lines(S,pv,col="blue")
abline(h=0)


###Opción en Corto call

sopcall<-function(S,K){
  
  ## S es un vector de precios spot
  ## K Es el precio strike
  call=NULL
  for(i in 1:length(S)){
    
    call[i]=-max(S[i]- K,0)
    
  }
  return(call)
  
}

## Opcion en corto Put
sopput<-function(S,K){
  
  ## S es un vector de precios spot
  ## K Es el precio strike
  put=NULL
  for(i in 1:length(S)){
    
    put[i]=-max(K-S[i],0)
    
  }
  return(put)
  
}


scall=sopcall(S,K)
sput=sopput(S,K)

plot(S,cv,type="l", col="orange", main="Opciones",xlab="Precio de la acción", ylab="V(S,t)",ylim=c(-150,150))
lines(S,pv,col="blue")
points(S,scall,col="orange", pch=16)
points(S,sput,col="blue",pch=16)
abline(h=0)


###Opción en largo call

opcall<-function(S,K){
  
  ## S es un vector de precios spot
  ## K Es el precio strike
  call=NULL
  for(i in 1:length(S)){
    
    call[i]=max(S[i]- K,0)
    
  }
  return(call)
  
}

## Opcion en largo Put
opput<-function(S,K){
  
  ## S es un vector de precios spot
  ## K Es el precio strike
  put=NULL
  for(i in 1:length(S)){
    
    put[i]=max(K-S[i],0)
    
  }
  return(put)
  
}


## Bull spread

### Necesitamos un call en largo precio strike K1
### y una call en corto precio strike K2 para K1<K2

K1=40
K2=60
S=20:100

call=opcall(S,K1)
scall=sopcall(S,K2)

bull=scall+call

plot(S,bull,col="green", type="l", ylim = c(-40,40), main="Bull Spread",lwd=8,ylab="Pay Off", xlab="Precio")
lines(S,scall,col="red",type="l")
lines(S,call,col="blue",type="l")


### Bear spread


### Necesitamos una opción put en largo con precio strike K2
### Necestiamos una opción put en corto con precio strike K1

put=opput(S,K2) 
sput=sopput(S,K1)

bear=sput+put

plot(S,bear,col="green", type="l", ylim = c(-20,40), main="Bear Spread",lwd=8,ylab="Pay Off", xlab="Precio")
lines(S,sput,col="red",type="l")
lines(S,put,col="blue",type="l")


### Strangle



## Put en largo
## Call en largo


call=opcall(S,K2)
put=opput(S,K1) 

strang=call+put

plot(S,strang,col="green", type="l", ylim = c(-20,40), main="Strangle Spread",lwd=8,ylab="Pay Off", xlab="Precio")
lines(S,put,col="red",type="l")
lines(S,call,col="blue",type="l")
