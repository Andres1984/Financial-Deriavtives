BlackScholes <- function(S, K, r, T, sig, type){
  
  if(type=="C"){
    d1 <- (log(S/K) + (r + sig^2/2)*T) / (sig*sqrt(T))
    d2 <- d1 - sig*sqrt(T)
    
    value <- S*pnorm(d1) - K*exp(-r*T)*pnorm(d2)
    return(value)}
  
  if(type=="P"){
    d1 <- (log(S/K) + (r + sig^2/2)*T) / (sig*sqrt(T))
    d2 <- d1 - sig*sqrt(T)
    
    value <-  (K*exp(-r*T)*pnorm(-d2) - S*pnorm(-d1))
    return(value)}
}





S=80:120
K=100
r=0
T=1
sig=0.01

call=BlackScholes(S, K, r, T, sig, "C")
put=BlackScholes(S, K, r, T, sig, "P")


plot(S,call,type="l",col="blue", ylab="Pay Off",xlab="Precio", main="Opciones")
lines(S,put, col="red")




opcal<-function(S,K){
  
  for(i in 1:length(S)){
    
    call[i]=max(S[i]-K,0)
    
  }
  return(call)
  
}

sopcal<-function(S,K){
  
  for(i in 1:length(S)){
    
    call[i]=-max(S[i]-K,0)
    
  }
  return(call)
  
}



S=4; K=2

opcal(S,K)


S=20:100; K=60

call=opcal(S,K)


plot(S,call,col="blue", type="l")

opput<-function(S,K){
  
  for(i in 1:length(S)){
    
    put[i]=max(K-S[i],0)
    
  }
  return(put)
  
}


sopput<-function(S,K){
  
  for(i in 1:length(S)){
    
    put[i]=-max(K-S[i],0)
    
  }
  return(put)
  
}


put=opput(S,K)
sput=sopput(S,K)
scall=sopcal(S,K)

 

 
plot(S,call,col="blue", type="l", ylim = c(-40,40), main="Opciones",)
lines(S,put,col="red",type="l")
lines(S,scall,col="blue",type="p")
lines(S,sput,col="red",type="p")
legend(50, 42,legend = c("call","put","scall","sput"), col=c("blue","red","blue", "red"),pch = c("-","-","o","o")) 





## Bull spread


K1=40
K2=60

S=20:100


call=opcal(S,K1)
scall=sopcal(S,K2)


bull=function(S,K1,K2){
  
  call=opcal(S,K1) ; scall=sopcal(S,K2)

   bull=scall+call
  return(bull)
}

bull=bull(S,K1,K2)
plot(S,bull,col="green", type="l", ylim = c(-40,40), main="Bull Spread",lwd=8,ylab="Pay Off", xlab="Precio")
lines(S,scall,col="red",type="l")
lines(S,call,col="blue",type="l")

bear=function(S,K1,K2){
  
 put=opput(S,K2) ; sput=sopput(S,K1)
  
  bear=sput+put
  return(bear)
}


put=opput(S,K2)
sput=sopput(S,K1)
bear=bear(S,K1,K2)
plot(S,bear,col="green", type="l", ylim = c(-20,40), main="Bear Spread",lwd=8,ylab="Pay Off", xlab="Precio")
lines(S,sput,col="red",type="l")
lines(S,put,col="blue",type="l")


K1=40
K2=60
K3=80

buterfly=function(S,K1,K2,K3){
  
  call1=opcal(S,K1) ; scall1=sopcal(S,K2);scall2=sopcal(S,K2);call2=opcal(S,K3)
  
  buterfly=call1+call2+scall1+scall2
  
  return(buterfly)
}

buterfly=buterfly(S,K1,K2,K3)

call1=opcal(S,K1) ; scall1=sopcal(S,K2);scall2=sopcal(S,K2);call2=opcal(S,K3)

plot(S,buterfly,col="green", type="l" ,ylim = c(-20,40), main="Buterfly Spread",lwd=8,ylab="Pay Off", xlab="Precio")
lines(S,scall1,col="red",type="l")
lines(S,call2,col="blue",type="l")
lines(S,scall2,col="red",type="l")
lines(S,call1,col="blue",type="l")



## Black and Scholes


call.value <- function(S, X, t, r, v)
{
  d1 <- (log(S/X)+(r+ 0.5 * v^2) * t) / ( v * sqrt(t))
  d2 <- d1 - v * sqrt(t)
  result <- S * pnorm(d1) - X * exp(-r * t) * pnorm(d2)
  result
}


put.value  <- function(S, X, t, r, v)
{
  d1 <- (log(S/X) + (r + 0.5 * v^2) * t)/(v * sqrt(t))
  d2 <- d1 - v * sqrt(t)
  X * exp(-r * t) * pnorm(-d2)- S * pnorm(-d1)
}


delta <- function(type, S, X, t, r, v)
{
  d1 <- (log(S/X) + (r + 0.5 * v^2) * t)/(v * sqrt(t));
  if(type > 0)	# put
    pnorm(d1) -1
  else			# call
    pnorm(d1);
  
}


gamma <- function(type, S, X, t, r, v)
{
  d <- (log(S/X) + (r + 0.5 * v^2) * t)/(v * sqrt(t));
  top <- exp(-d^2/2)
  bottom <- S * v * sqrt(2*pi*t)
  top/bottom
}



# type: either 0 means call option and 1 means put option
rho <- function(type, S, X, t, r, v)
{
  d1 <- (log(S/X)+(r+ 0.5 * v^2) * t) / ( v * sqrt(t))
  d2 <- d1 - v * sqrt(t)
  if(type >0)	# put
    -X * t * exp(-r * t) * pnorm(-d2)
  else	# call	
    X * t * exp(-r * t) * pnorm(d2);
}


vega <- function(type, S, X, t, r, v)
{
  d1 <- (log(S/X)+(r+ 0.5 * v^2) * t) / ( v * sqrt(t))
  S * sqrt(t) * ( exp(-d1^2/2)/ sqrt(2 * pi) )
}


theta <- function(type, S, X, t, r, v)
{
  d1 <- (log(S/X)+(r+ 0.5 * v^2) * t) / ( v * sqrt(t))
  d2 <- d1 - v * sqrt(t)
  if(type >0)	# put
  {
    p1 <- -S * exp(-d1^2/2) * v / (2 * sqrt(2 * pi * t) )
    p2 <- r * X * exp(-r * t) * pnorm(1 - pnorm(d2))	        
    p1 + p2
  }
  else	# call	
  {
    p1 <- -S * exp(-d1^2/2) * v / (2 * sqrt(2 * pi * t) ) 
    p2 <- r * X * exp(-r * t) * pnorm(d2);	        
    p1 - p2
  }
}



greeks <- function(type, fnc, S, X, t, r, v)
{
  if (fnc=="delta")
  {
    delta(type, S, X, t, r, v)
  }
  else if(fnc=="vega")
  {
    vega(type, S, X, t, r, v)
  }
  else if(fnc=="gamma")
  {
    gamma(type, S, X, t, r, v)
  }
  else if(fnc=="rho")
  {
    rho(type, S, X, t, r, v)
  }
  else
  {
    theta(type, S, X, t, r, v);
  }
}


# The following code produces a 3D perspective 
# plot for the Black-Schole Call option.
CallOption3DPlot = function ( S, X, t, r, v, 
                              theta = 30, phi = 30, expand = 0.75, 
                              col = "lightblue", ltheta = 120, shade = 0.75, 
                              ticktype = "detailed", cex = 0.6, 
                              main = "Black-Scholes Call Option Price", ...)
{   
  
  premium3D = function(S, t,  X, r, v) 
  {
    call.value(S, X, t, r, v)
  }
  
  
  # Prices:
  Price = outer(S, t, FUN = premium3D,  X, r,  v)
  
  # Perspective Plot:
  persp(x = S, y = t, z = Price, xlab = "S", ylab = "Time",  
        theta = theta, phi = phi, expand = expand, col = col, 
        ltheta = ltheta, shade = shade, ticktype = ticktype, 
        cex = cex, main = main, ...) 
  
  # Return Value:
  invisible(list(S = S, t= t, Price = Price))  
}




CallOption3DPlot( S = seq(from = 75, to = 125, length = 40), 
                  X = 100, 
                  t = seq(from = 1/52, to = 1, length = 40), 
                  r = 0.1, 
                  v = 0.4)


### Graficos de las griegas


plot3DGreeks <- function (type, func, S, X, t, r, v, theta = 30, 
                          phi = 40, expand = 0.75, col = "cyan", ltheta = 120, shade = 0.75, 
                          ticktype = "detailed", cex = 0.6, 
                          main = paste("Black-Scholes Option Sensitivity for ", func, sep="") , ...)
{  
  greeks3D = function(S, t, X, r, v) 
  { 
    if (func == "delta")
    {
      delta(type, S, X, t, r, v)
    }
    else if (func == "gamma")
    {
      gamma(type, S, X, t, r, v)
    }
    else if (func == "vega")
    {
      vega(type, S, X, t, r, v)
    }
    else if (func == "theta")
    {
      theta(type, S, X, t, r, v)
    }
    else if (func == "rho")
    {
      rho(type, S, X, t, r, v)
    }
  }
  
  # Sensitivities:
  Greeks = outer(S, t, FUN = greeks3D, X, r, v)
  
  # Perspective Plot:
  persp(x=S, y=t, z=Greeks, xlab="S", ylab="Time", zlab="Griega", 
        theta=theta, phi=phi, expand=expand, col=col, ltheta=ltheta,
        shade=shade, ticktype=ticktype, cex=cex, main=main, ...) 
  
  # Return Value:
  invisible(list(S = S, t = t, Sensitivity = Greeks))
}


## Grafica de una Delta



S=80:120
K=100
r=0.05
T=1
sig=0.5

calldelta=delta(0,S,K,T,r,sig)
calldelta1=delta(0,S,K,T,r,0.1)
calldelta2=delta(0,S,K,10,r,0.01)
calldelta3=delta(0,S,K,T,0.05,0.01)

plot(S,calldelta, type="l",col="blue")
lines(S,calldelta1, col="red")
lines(S,calldelta2, col="green")
lines(S,calldelta3, col="orange")
abline(v=100*exp(-0.05))
abline(v=100)


types <- c(0, 1)
S <- seq(from = 75, to = 125, length = 25) 
t <- seq(from = 1/52, to = 1, length = 25)
for (i in types)
{
  plot3DGreeks(i, "theta", S = S, X = 100, t = t, r = 0.1, v = 0.40)
}


### Grafica Gamma


gammacall=gamma(0,S,K,T,r,sig)
gammacall1=gamma(0,S,K,T,r,0.1)
gammacall2=gamma(0,S,K,10,r,0.01)
gammacall3=gamma(0,S,K,T,0.05,0.01)

par(mfrow=c(2,1))
plot(S,gammacall, type="l",col="blue")
lines(S,gammacall1, col="red")
lines(S,gammacall2, col="green")
lines(S,gammacall3, col="orange")
abline(v=100*exp(-0.05))
abline(v=100)

plot(S,calldelta, type="l",col="blue")
lines(S,calldelta1, col="red")
lines(S,calldelta2, col="green")
lines(S,calldelta3, col="orange")
abline(v=100*exp(-0.05))
abline(v=100)


### Grafica theta


thetacall=theta(0,S,K,T,r,sig)
thetacall1=theta(0,S,K,0.2,r,sig)
t <- seq(from = 1/52, to = 1, length = 41)
plot(S,thetacall, type="l",col="blue")
lines(S,thetacall1, col="red")



