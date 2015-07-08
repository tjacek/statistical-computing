library(MASS)
data(cats)
summary(cats)

hist(cats$Bwt,probability=TRUE)
h=hist(cats$Hwt,probability=TRUE)
plot(h$mids,h$density)
abline(h=0.05)

fake.mean <- 10 
fake.var <- 8

gamma.a <- function(mean,var){
  return(mean^2 / var^2)
}

gamma.s <- function(mean,var){
  return(var^2/mean)
}

true.mean <- mean(cats$Hwt)
true.var <- var(cats$Hwt)
gamma.a(true.mean,true.var)
gamma.s(true.mean,true.var)

gamma.cat <- function(vect){
  cats.mean <- mean(vect)
  cats.var <- var(vect)
  cats.a <- gamma.a(cats.mean,cats.var)
  cats.s <- gamma.s(cats.mean,cats.var)
  return( c(cats.mean,cats.var,cats.a,cats.s))
}

cats.male <- cats[cats$Sex=='M',3] 
cats.female <- cats[cats$Sex=='F',3] 

gamma.cat(cats.female) 
gamma.cat(cats.male) 

hist(cats.female,probability=TRUE) 
curve(dgamma(x,shape=24.92,scale=0.37),from = 0.0, to = 20.0)

hist(cats.male,probability=TRUE) 
curve(dgamma(x,shape=3.0,scale=3.7),from = 0.0, to = 20.0)



