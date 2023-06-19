##Funcion que calcula sumatorias de la forma 1
Sumatoria1<-function(x,limI,limS){
  Suma1<-vector("numeric",limS)
  Suma1[1]<-1
  for(i in limI:(limS-1) ){
    Suma1[i+1]<-Suma1[i]+( (x^{i+1})/factorial(i+1) )
  }
  return( Suma1[limS] )
}

getwd()
setwd("/Users/Migue y Juan/Desktop/FCFM 1/Simulación/proyecto f")

CCD<-read.csv("CCD.csv.", header=TRUE)

##100 llamadas con 10 servidores de 12 am - 1 am
s<-10
Datos<-cbind(CCD[1:100,1:2], CCD[1:100,4], 
            CCD[1:100,6:8]  )
Periodos<-12 
CallAbandoned<-Datos[,3] ##No contestadas
CallAnswered<-Datos[,2] ##Contestadas
TiempoEspera<-CCD[1:100,7]
Total<- CallAbandoned+CallAnswered ##Llamadas en el día

##lambda: tasa de llamadas que llegan
##En dos minutos ya que en ese tiempo es el tiempo
##maximo para responder a la llamada
lambda<- ceiling(sum(TiempoEspera)/sum(Total))

## mu: tasa de llamadas atendidas
mu<-ceiling(sum(CCD[1:100,6])/sum(Total)) 

##Probabilidad cero 
p0_1<-lambda/(s*mu)
p0_2<-( ((lambda/mu)^{s})/factorial(s) )*( 1/(1-p0_1) )
p0_3<- Sumatoria1( (lambda/mu),1,s-1 )
P0<-1/( p0_3 + p0_2 )

##Factor de ultilización
rho<- lambda/(s*mu)
##Numero de clientes en el sistema
Lq<-( P0*( (lambda/mu)^{s} )*rho )/(factorial(s)*( (1-rho)^{2}  ) )
## Longitud esperada excluyendo a los que estan en servicio
L<-Lq+(lambda/mu)
##Tiempo de espera en la cola (excluye el tiempo de servicio) 
##para cada cliente
Wq<-Lq/lambda
##Tiempo de espera en el sistema (incluye tiempo de servicio)
##para cada cliente
W<-Wq+(1/mu)
