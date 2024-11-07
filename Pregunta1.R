M<-95.3
sd<-5.7

#poblacion

curve(dnorm(x,mean=M,sd), xlim=c(80,120)) #density

set.seed(123)
rnorm(4, M, sd) #muestra aleatoria tamaño 4

rnorm(1,M,sd)
pnorm(90,M,sd)

#a
Y<-function(i)(sum(rnorm(4, M, sd)))
Y100000<-sapply(1:100000,Y)
mean(Y100000)
var(Y100000)
hist(Y100000, freq=FALSE)
  #en teoria
    #media suma muestral
    4*M
    #varianza suma muestral
    4*sd^2
    
hist(Y100000, freq=FALSE)
curve(dnorm(x,mean=4*M,sqrt(4*sd^2)), add=TRUE)

#b
Y<-function(i)(sum(rnorm(100, M, sd)))
Y100000<-sapply(1:100000,Y)
var(Y100000)
  #en teoria
  100*sd^2

#c
  #P(X>103)
1-pnorm(103,M,sd)
Y<-function(i)(sum(rnorm(1, M, sd)))
Y100000<-sapply(1:100000,Y)
hist(Y100000)

mean(Y100000>103)

#d
Xbar<-function(i)(mean(rnorm(4, M, sd)))
Xbar100000<-sapply(1:100000,Xbar)
hist(Xbar100000)

mean(Xbar100000<98)
pnorm(98,M,sd/sqrt(4))

#e  más grande que 32kg (error del enunciado)
ssq<-function(i)(var(rnorm(100, M, sd)))
ssq100000<-sapply(1:100000,ssq)
mean(ssq100000>32)

1-pchisq((100-1)*32/sd^2,100-1)