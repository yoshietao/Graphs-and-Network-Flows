library(igraph)
library(data.table)
library(rjson)

filename = 'san_francisco-censustracts-2017-4-All-MonthlyAggregate.csv'
#filename = 'test.csv'

MyData <- read.csv(file=filename, header=TRUE, sep=",")
setDT(MyData)

json_file = 'san_francisco_censustracts.json'
json_data <- fromJSON(file=json_file)

#json_data$features[[1000]]$geometry$coordinates[[1]][[1]][[19]]
#json_data$features[[1000]]$properties$DISPLAY_NAME
geo_add <- c()
geo_x <- c()
geo_y <- c()
for (p in 1:length(json_data$features)){
  tmp <- json_data$features[[p]]
  geo_x <- c(geo_x, mean(split(unlist(tmp$geometry$coordinates),1:2)[[1]]))
  geo_y <- c(geo_y, mean(split(unlist(tmp$geometry$coordinates),1:2)[[2]]))
  geo_add <- c(geo_add, tmp$properties$DISPLAY_NAME)
}
geo = data.table(id=c(1:length(json_data$features)), add=geo_add, x=geo_x, y=geo_y)

g <- graph_from_edgelist(as.matrix(MyData[, .(sourceid, dstid)]), directed = FALSE)

E(g)$mtt <- as.matrix(MyData[, .(mean_travel_time)])
V(g)$st_add <- as.matrix(geo[, .(geo_add)])
V(g)$x <- as.matrix(geo[, .(geo_x)])
V(g)$y <- as.matrix(geo[, .(geo_y)])
# To access: V(g)[i]$y

#Q6:
clu <- components(g)
g_sub <- induced.subgraph(g, which(clu$membership == which.max(clu$csize)))
vcount(g_sub)
ecount(g_sub)

#Q7
g_mst <- mst(g_sub)
end_points <- list()
i<-1
total <- 0
for (i in 1:10){
  e <- E(g_mst)[i]
  vs <- ends(g_mst, E(g_mst)[i])
  end_points[[i]] <- c(V(g_mst)[vs[1]]$st_add,V(g_mst)[vs[2]]$st_add,e$mtt)
  total <- total + e$mtt
  i <- i+1
}
end_points
total
# Not so intuitive lol

