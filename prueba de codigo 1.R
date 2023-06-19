##Funcion que calcula sumatorias de la forma 1
##Parametros:
##x es el numero que se mutiplicara i+1 veces
##limI es el limite inferior de la sumatoria
##limS es el limite superior de la sumatoria
Sumatoria1<-function(x,limI,limS){
  Suma1<-vector("numeric",limS)
  Suma1[1]<-1
  for(i in limI:(limS-1) ){
    Suma1[i+1]<-Suma1[i]+( (x^{i+1})/factorial(i+1) )
  }
  return( Suma1[limS] )
}

##Funcion Megumin
##Parametros
##s son los servidores
##CAb, CAn que son llamadas abandondadas, llamadas contestadas
##TW es el tiempo de espera 
##DC es la duracion de la llamada (excluyendo el tiempo de espera)
##CAb, CAn, TW, DC son vectores columnas de la misma longitud
Megumin<-function(s,CAb,CAn,TW,DC){
  ##Calculo de las llamadas que hubo en el turno
  Total<-CAb+CAn
  
  ##Calculo de lambda: tasa de llegada de llamadas
  lambda<-ceiling( sum(TW/60)/sum(Total) ) ##clientes por minuto
  
  ##Calculo de mu: tasa de atención
  mu<-ceiling( sum(DC/60)/sum(Total) ) ##clientes por minuto
  
  ##Factor de utilizacion
  rho<-lambda/(s*mu)
  
  ##Probabilidad cero
  p0_1<-( ((lambda/mu)^{s})/factorial(s) )*( 1/(1-rho) )
  p0_2<- Sumatoria1( (lambda/mu),1,s-1 )
  P0<-1/( p0_2 + p0_1 )
  
  ##Numero de clientes en la cola (excluyendo los que
  ##estan siendo atendidos)
  Lq<-( P0*( (lambda/mu)^{s} )*rho )/(factorial(s)*( (1-rho)^{2}  ) )
  Lq<-ceiling(Lq)
  ## Longitud esperada excluyendo a los que estan en servicio
  L<-Lq+(lambda/mu)
  
  ##Tiempo de espera en la cola (excluye el tiempo de servicio) 
  ##para cada cliente
  Wq<-Lq/lambda
  ##Tiempo de espera en el sistema (incluye tiempo de servicio)
  ##para cada cliente
  W<-Wq+(1/mu)
  
  DatosObtenidos<-c(s,lambda,mu,rho,P0,L,Lq,W,Wq)
  return(DatosObtenidos )
  
}

getwd()
setwd("/Users/Migue y Juan/Desktop/FCFM 1/Simulación/proyecto f")

CCD<-read.csv("CCD.csv.", header=TRUE)

##Notacion general
t<-1:500
Cab1<-CCD[t,4]
Cab2<-CCD[t,2]
TW1<-CCD[t,7]
DC1<-CCD[t,6]

#Caso s<#
M1<-matrix(0,0,nrow=10,ncol=9)
for(m in 1:10){
  M1[m,]<-Megumin( (50+(m-1)*50) ,Cab1,Cab2,TW1,DC1)
}

#Caso s<#
M2<-matrix(0,0,nrow=10,ncol=9)
for(k in 1:10){
  M2[k,]<-Megumin( (500+(k*50)) ,Cab1,Cab2,TW1,DC1)
}

#Caso s=#
M3<-matrix(0,0,nrow=5,ncol=9)
for(i in 1:5){
  M3[i,]<-Megumin(i*100,Cab1[1:(i*100)],Cab2[1:(i*100)],
          TW1[1:(i*100)],DC1[1:(i*100)])
}

