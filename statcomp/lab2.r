library(MASS)
data(Cars93)

View(Cars93)
summary(Cars93)
nrow(Cars93)
rear.mean <- mean(Cars93[Cars93$DriveTrain=="Rear","Price"])

min(Cars93[Cars93$Passengers==7,"Horsepower"])
min(Cars93[Cars93$Passengers>5,"Horsepower"])

cars.eff <- Cars93$Fuel.tank.capacity / Cars93$Rev.per.mile

cars.eff[cars.eff==min(cars.eff)]
cars.eff[cars.eff==max(cars.eff)]
cars.eff[cars.eff==median(cars.eff)]
