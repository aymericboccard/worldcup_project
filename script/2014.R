### Script to process data for Gephi

#Importing data
data <- read.csv2("WorldCup2014-Squadclubs.csv", header = TRUE) #Sheet 2014 World Cup squads
data<-data[,-11]

# Data for one-mode graphs example 
cols.names <- list("Id", "Label", "Club", "Country(club)", "Country", "year")
arcs.names <- list("Source", "Target", "Label", "Type")

nodes <- matrix(data = NA, ncol = length(cols.names), nrow = nrow(data), dimnames = list(NULL, cols.names))
nodes[,'Id'] <- c(1:nrow(nodes))
nodes[,'Label'] <- as.matrix(data$Player)
nodes[,'Club'] <- as.matrix(data$Club)
nodes[,'Country(club)'] <- as.matrix(data$Club..country.)
nodes[,'Country'] <- as.matrix(data$Country)
nodes[,'year'] <- rep(2014, times = nrow(nodes))

arcs.countries <- matrix(data = NA, ncol = length(arcs.names), nrow = 0, dimnames = list(NULL, arcs.names))
country <- unique(nodes[,'Country'])

for(c in 1:length(country)){
  players <- which(nodes[,'Country'] == country[c])
  a <- matrix(data = NA, ncol = length(arcs.names), nrow = (length(players)*(length(players)-1))/2, dimnames = list(NULL, arcs.names))
  v<-0
  for(p1 in 1:(length(players)-1)){
    for(p2 in (p1+1):length(players)){
      v<-v+1
      a[v,]<-c(players[p1],players[p2],country[c], "Undirected")
    }
  }
  arcs.countries <- rbind(arcs.countries,a)
}

arcs.clubs <- matrix(data = NA, ncol = length(arcs.names), nrow = 0, dimnames = list(NULL, arcs.names))
clubs <- unique(nodes[,'Club'])

for(c in 1:length(clubs)){
  players <- which(nodes[,'Club'] == clubs[c])
  a <- matrix(data = NA, ncol = length(arcs.names), nrow = (length(players)*(length(players)-1))/2, dimnames = list(NULL, arcs.names))
  v<-0
  if(length(players)!=1){
    for(p1 in 1:(length(players)-1)){
      for(p2 in (p1+1):length(players)){
        v<-v+1
        a[v,]<-c(players[p1],players[p2],clubs[c], "Undirected")
        print(c(players[p1],players[p2],clubs[c], "Undirected"))
      }
    }
    arcs.clubs <- rbind(arcs.clubs,a)
  }
}

# Data for two-mode graphs example 
nodes.clubs.country.names <- list("Id", "Label", "Country","Players", "Class")
arcs.clubs.country.names <- list("Source","Target", "Type", "Weight")

arcs.clubs.country <- matrix(data = NA, ncol = length(arcs.clubs.country.names), nrow = 0, dimnames = list(NULL, arcs.clubs.country.names))
nodes.clubs.country <- matrix(data = NA, ncol = length(nodes.clubs.country.names), nrow = 0, dimnames = list(NULL, nodes.clubs.country.names))

nodes.clubs.country.temp <- unique(nodes[,3:5])

for(i in 1:nrow(nodes.clubs.country.temp)){
  c1 <-(which(nodes[,3]==nodes.clubs.country.temp[i,1]))
  c2 <-(which(nodes[,4]==nodes.clubs.country.temp[i,2]))
  c3 <-(which(nodes[,5]==nodes.clubs.country.temp[i,3]))
  count <- length(intersect(intersect(c1,c2),c3))
  arcs.clubs.country <- rbind(arcs.clubs.country,c(nodes.clubs.country.temp[i,1],nodes.clubs.country.temp[i,3],"Undirected",count))
}

nodes.clubs.country.temp <- unique(nodes.clubs.country.temp[,1:2])
n <-NULL
for(i in 1:nrow(nodes.clubs.country.temp)){
  n[i]<-length(which(nodes.clubs.country.temp[i,1]==nodes[,3]))
}

nodes.clubs.country <- rbind(nodes.clubs.country,cbind(1:nrow(nodes.clubs.country.temp),nodes.clubs.country.temp, n, rep("Club",nrow(nodes.clubs.country.temp))))
nodes.clubs.country <- rbind(nodes.clubs.country,cbind(800:831, country,NA,32,"Country"))

for(i in 1:nrow(nodes.clubs.country)){
  arcs.clubs.country <- replace(arcs.clubs.country, which(arcs.clubs.country==nodes.clubs.country[i,2]), nodes.clubs.country[i,1])
}

#Exporting data. Do not forget to create a database folder

write.csv2(nodes, file = "database/nodes-2014.csv", row.names = FALSE)
write.csv2(arcs.countries, file = "database/arcs-countries-2014.csv", row.names = FALSE)
write.csv2(arcs.clubs, file = "database/arcs-clubs-2014.csv", row.names = FALSE)

write.csv2(arcs.clubs.country, file = "database/arcs-clubs-countries-2014.csv", row.names = FALSE)
write.csv2(nodes.clubs.country, file = "database/nodes-clubs-countries-2014.csv", row.names = FALSE)

