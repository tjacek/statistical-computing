gmp <- read.table("data/gmp-2013.dat")
gmp$pop <- round(gmp$gmp/gmp$pcgmp)

power.law <- function(x,alpha=1.0,y0=1.0){
  return( y0 * (x^alpha))
}

plot(gmp$pop,gmp$pcgmp)
curve(power.law,from = 0.0, to = 2.0 *10^7)

curve(power.law(x,alpha=0.1), from = 0.0, to = 2.0 *10^7)
curve(power.law(x,alpha=0.15),from = 0.0, to = 2.0 *10^7)

mse <- function(params,X=gmp$pop,Y=gmp$pcgmp){
  y0<-params[1]
  a<-params[2]
  pred <- y0*(X^a)
  error <- (Y-pred)^2
  return( mean(error))
}

nlm(mse, c(y0=6611,a=1.0/8.0))

plm <- function(y0,a,X1=gmp$pop,Y1=gmp$pcgmp){
  optim <-  c(y0,a)
  result<-nlm(mse, optim)
  return( result$estimate )
}

pcgmp.mean <- mean(gmp$pcgmp)
pcgmp.sd   <- sd(gmp$pcgmp)

mean_i <- function(i,X=gmp$pcgmp){
  N <- length(X)
  index<-seq(1, N, length.out=N)
  return( mean(X[index!=i]))
}

jackknifed.means <-  function(X=gmp$pcgmp){
  N <- length(X)
  index<-seq(1, N, length.out=N)
  theta <- function(i) { return(mean_i(i,X))}
  return(lapply(index,theta))
}

jackknifed.error <- function(X=gmp$pcgmp){
  N <- length(X)
  mean.jk <- jackknifed.means(X)
  mean.nr <- mean(X)
  mean.err <- (unlist(mean.jk)-mean.nr)^2
  return(sum(mean.err) / (N-1))
}

X_i <- function(i,X=gmp$pcgmp){
  N <- length(X)
  index<-seq(1, N, length.out=N)
  return( X[index!=i])
}

plm_i <- function(i,y0,a,X=gmp$pop,Y=gmp$pcgmp){
  X_i=X_i(i,X)
  Y_i=X_i(i,Y)
  return(plm(y0,a,X1=X_i,Y1=Y_i))
}

jackknifed.pl <- function(y0=6661,a=0.15,X=gmp$pop,Y=gmp$pcgmp){
  lambda <- function(i){ return(plm_i(i,y0,a,X,Y)) }
  N <- length(X)
  index<-seq(1, N, length.out=N)
  return( lapply(index,lambda)) 
} 

jackknife <-function(N,mean_n,mean_i){
  mean.err <- (unlist(mean_i)-mean_n)^2
  return(sqrt(sum(mean.err) / (N-1)))
}

plm.jackknife <- function(y0=6661,a=0.15,X=gmp$pop,Y=gmp$pcgmp){
  params<-jackknifed.pl(y0,a,X,Y)
  y_i <- unlist(lapply(params,function(x){return(x[1])}))
  a_i <- unlist(lapply(params,function(x){return(x[2])}))
  full.par <- plm(y0,a,X,Y)
  N <- length(X)
  a_err <- jackknife(N,full.par[1],y_i)
  y_err <- jackknife(N,full.par[2],a_i)
  return( c(a_err,y_err))
}