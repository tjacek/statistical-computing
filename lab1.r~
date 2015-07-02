exp.draws.1 <- rexp(n=200)
mean.exp.1  <- mean(exp.draws.1)
sd.exp.1    <- sd(exp.draws.1)

exp.draws.01 <- rexp(n=200,rate=0.1)
exp.draws.05 <- rexp(n=200,rate=0.5)
exp.draws.5  <- rexp(n=200,rate=5)
exp.draws.10 <- rexp(n=200,rate=10)

hist.exp=hist(exp.draws.1)
plot(hist.exp)
plot(exp.draws.01,exp.draws.5)

exp.draws <- list(exp.draws.01,exp.draws.05,exp.draws.1,exp.draws.5,exp.draws.10)
exp.means <- lapply(exp.draws,mean) 
exp.sds   <- lapply(exp.draws,sd)
exp.rates <- list(0.1,0.5,1.0,5.0,10.0)

plot(exp.rates,exp.means)
plot(exp.rates,exp.sds)
plot(exp.means,exp.sds)

big.exp.draws.1 <- rexp(n=1100000)
big.mean.exp <- mean(big.exp.draws.1)
big.sd.exp <- sd(big.exp.draws.1)

big.hist.exp=hist(big.exp.draws.1)
plot(big.hist.exp)

filtered.big.exp <- big.exp.draws.1[ big.exp.draws.1 > 1.0]
filtered.mean    <- mean(filtered.big.exp)

big.exp.draws.1.mat <- array(big.exp.draws.1,dim=c(1000,1100))
hist.mat=hist(big.exp.draws.1.mat)

mean(big.exp.draws.1.mat[371,])

columns.means <- apply(big.exp.draws.1.mat,2,mean)
hist(columns.means) 

big.exp.square <- big.exp.draws.1* big.exp.draws.1
mean(big.exp.square)
sd(big.exp.square)
