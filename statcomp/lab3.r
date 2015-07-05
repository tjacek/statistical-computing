richhtml <- readLines("data/rich.html") 
rich.names <-  grep(x=richhtml,pattern="<h3>(.)+</h3></a>")

rich.value <- richhtml[rich.names+1]
rich.value <-gsub("((.)+\\$)|(B(.)+)","",rich.value)
rich.value <-gsub(",",".",rich.value)
rich.value <-gsub("(\\s)+","",rich.value)

networths <- lapply(rich.value,as.double)

networths <- unlist(networths) 

length(networths)
length(networths[networths>1.0])
length(networths[networths==72])
length(networths[networths==5.3])

median(networths)
mean(networths)
length(networths[networths>5])
length(networths[networths>10])
length(networths[networths>25])
sum(networths)

networths <- rev(sort(networths))
sum(networths[1:5]) / sum(networths)
sum(networths[1:20]) / sum(networths)
sum(networths[1:55]) / sum(networths)

sum(networths) / 820000
