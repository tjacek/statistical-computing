rain.df <-  read.table("data/rnf6080.dat")

nrow(rain.df)
ncol(rain.df)

names(rain.df)
rain.df[5,7]
rain.df[2,]

names(rain.df) <- c("year","month","day",seq(0,23))
head(rain.df)

newRow <-apply(rain.df[,3:27],1,mean)
names(newRow) <- c("daily")
rain.df <-rbind(rain.df,newRow)

rain.df[-999==rain.df]=0

seq(from=1,to=10000,by=372)
seq(from=1,to=10000,length.out=50)


