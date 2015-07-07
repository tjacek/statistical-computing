nh11415 <- readLines("date/NHLHockeySchedule2.html") 
length(nh11415)
sum(unlist(lapply(nh11415,nchar)))
max(unlist(lapply(nh11415,nchar))) 

nha.raw <- grep(x=nh11415,pattern="</td>(.)+<!-- Network -->",value=TRUE)
nha.raw <- unlist(nha.raw)

getdate <- function(s){
  date.matches <-regexpr(s,pattern="[A-Za-z]+ [A-Za-z]+ [0-9]+, [0-9]+")
  date <- regmatches(s,m=date.matches)
  return(date)
}

date <- lapply(nha.raw,getdate)
date <- unlist(date)

getTeams <- function(s){
  team.matches <-gregexpr(s,pattern=">([A-Za-z]|(\\s))+</a></div></td>")
  team <- unlist(regmatches(s,m=team.matches))
  team <- unlist(team)
  team.visit <- gsub("(>)|(</a></div></td>)","",team[1])
  team.home <- gsub("(>)|(</a></div></td>)","",team[2])
  return(list(team.visit,team.home) )
}

teams <- lapply(nha.raw,getTeams)

getVisit <-function(x){
  x<-unlist(x)
  return(x[1])
}

getHome <-function(x){
  x<-unlist(x)
  return(x[2])
}

teams.visit <- unlist(lapply(teams,getVisit))
teams.home <- unlist(lapply(teams,getHome))

getTime <- function(s){
  time.matches <-regexpr(s,pattern="[0-9]+:[0-9]+ PM ET")
  time <- regmatches(s,m=time.matches)
  return(time)
}

time <- unlist(lapply(nha.raw,getTime))

getTicket <- function(s){
  ticket <- gsub("(><span>TICKETS(.)+)","",s)
  ticket <- gsub("(.)+href=\"","",ticket)
  return(ticket)
}
ticket <- grep(x=nh11415,pattern="href=\"(.)+\"><span>BUY",value=TRUE)
ticket <- unlist(lapply(nha.raw,getTicket))


nh <- data.frame(date,teams.home,teams.visit,time,ticket)
nh[1220:1230,]

