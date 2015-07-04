ca_pa <-  read.csv("data/calif_penn_2011.csv" )
ca_pa <- na.omit(ca_pa)

nrow(ca_pa)
ncol(ca_pa)
colSums(apply(ca_pa,c(1,2),is.na))

house.new <-ca_pa$Built_2005_or_later
house.mediana <-ca_pa$Mediana_hose_value
plot(house.new,house.mediana)

ca_pa.cl <- ca_pa[ca_pa$STATEFP==6,]
ca_pa.pe <- ca_pa[ca_pa$STATEFP==42,]

plot(ca_pa.cl$Built_2005_or_later,ca_pa.cl$Mediana_hose_value)
plot(ca_pa.pe$Built_2005_or_later,ca_pa.pe$Mediana_hose_value)

vacant_ratio <- ca_pa$Vacant_units / ca_pa$Total_units 

ca_pa_new <-cbind(ca_pa,vacant_ratio)

min(vacant_ratio) 
max(vacant_ratio)
median(vacant_ratio)
mean(vacant_ratio) 

plot(ca_pa_new$vacant_ratio,ca_pa_new$Median_house_value)

ca_pa.cl <- ca_pa_new[ca_pa_new$STATEFP==6,]
ca_pa.pe <- ca_pa_new[ca_pa_new$STATEFP==42,]
plot(ca_pa.cl$vacant_ratio,ca_pa.cl$Mediana_hose_value)
plot(ca_pa.pe$vacant_ratio,ca_pa.pe$Mediana_hose_value)

median(ca_pa[ca_pa$STATEFP==6 & ca_pa$COUNTYFP==1,][,10])

Alameda <-   ca_pa[ca_pa$STATEFP==6 & ca_pa$COUNTYFP==1,]
Sa <-        ca_pa[ca_pa$STATEFP==6 & ca_pa$COUNTYFP==85,]
Allegheny <- ca_pa[ca_pa$STATEFP==42 & ca_pa$COUNTYFP==3,]

mean(Alameda$Built_2005_or_later)
mean(Sa$Built_2005_or_later)
mean(Allegheny$Built_2005_or_later)

calif <- ca_pa[ca_pa$STATEFP==6,]

cor(ca_pa$Median_house_value,ca_pa$Built_2005_or_later)
cor(calif$Median_house_value,calif$Built_2005_or_later)
cor(Alameda$Median_house_value,Alameda$Built_2005_or_later)
cor(Sa$Median_house_value,Sa$Built_2005_or_later)
cor(Allegheny$Median_house_value,Allegheny$Built_2005_or_later)

plot(Sa$Median_house_value,Sa$Median_household_income)
plot(Alameda$Median_house_value,Alameda$Median_household_income)
plot(Allegheny$Median_house_value,Allegheny$Median_household_income)
