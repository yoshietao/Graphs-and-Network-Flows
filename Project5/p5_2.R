library(igraph)
library(data.table)
library(rjson)

DO_6 = TRUE
DO_7 = FALSE
DO_8 = TRUE
DO_9 = FALSE
DO_11= TRUE

#setwd("/Users/evelyn/Documents/master_first_year/third_quarter/ECE232/Random-Graphs-and-Random-Walks/Project5/")
filename = 'san_francisco-censustracts-2017-4-All-MonthlyAggregate.csv'
#filename = 'test.csv'

MyData <- read.csv(file=filename, header=TRUE, sep=",")
setDT(MyData)
MyData <- MyData[month==12]

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
# calculate mean travel time/distance ratio to approximate missing data
# de <- 0
# nu <- 0
# for(i in 1:vcount(g)){
#   for(j in i:vcount(g)){
#     d <- sqrt((V(g)[i]$x-V(g)[j]$x)^2+(V(g)[i]$y-V(g)[j]$y)^2)
#     id <- get.edge.ids(g,c(i,j), directed=FALSE, error=FALSE)
#     if(id != 0){
#       de <- de + d
#       nu <- nu + E(g)[id]$mtt
#     }
#   }
# }
#mtt_d_ratio <- nu/de
mtt_d_ratio <- 6233.923

#Q6:
if (DO_6 == TRUE || DO_8 == TRUE){
  g <- simplify(g, remove.multiple = TRUE, remove.loops = TRUE, edge.attr.comb="mean")
  clu <- components(g)
  g_sub <- induced.subgraph(g, which(clu$membership == which.max(clu$csize)))
  vcount(g_sub)
  ecount(g_sub)
}

#Q7
if (DO_7 == TRUE){
  g_mst <- mst(g_sub, weights = E(g_sub)$mtt)
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
}

#Q8
if (DO_8 == TRUE){
  tri_ineq_true <- 0
  tri_ineq_false <- 0
  count <- 0
  while (count < 1000){
    v <- sample(1:vcount(g_sub), 1, replace=FALSE)
    neigh_list <- neighbors(g_sub, v)
    random_two <- sample (neigh_list, size=2, replace=FALSE)
    while ( (are.connected(g_sub, random_two[1], random_two[2]) == FALSE)){
      random_two <- sample (neigh_list, size=2, replace=FALSE)
    }
    are.connected(g_sub, random_two[1], random_two[2])
    v_list <- get.edge.ids(g_sub, c(v, random_two[1], v, random_two[2], random_two[1], random_two[2]))
    weights <- edge_attr(g_sub, "mtt", index = v_list)
    if (weights[1]+weights[2]>weights[3] && weights[1]+weights[3]>weights[2] && weights[2]+weights[3]>weights[1])
      tri_ineq_true <- tri_ineq_true + 1
    else 
      tri_ineq_false <- tri_ineq_false + 1
    count <- count +1
  }
  tri_ineq_true
  tri_ineq_false
  
}

#Q9---->map missing path to time weighted shortest path(distance)
if(DO_9 == TRUE){
  #visited <- c()
  #next = 1
  g_mst_2 <- make_empty_graph(n = vcount(g_mst), directed = FALSE)
  g_dfs <- dfs(g_mst, root=1, neimode ="all")
  g_order <- as.matrix(g_dfs$order)
  cost <- 0.0
  att_wt <- c()
  for(i in 1:(vcount(g_mst)-1)){
    g_mst_2 <- add_edges(g_mst_2,c(g_order[i],g_order[i+1]))
    wt <- E(g_mst)[get.edge.ids(g_mst, c(g_order[i],g_order[i+1]))]$mtt
    if(length(wt)>0){
      cost <- cost + wt
      att_wt <- c(att_wt,wt)
    }
    else{
      wt <- mtt_d_ratio*sqrt((V(g_mst)[g_order[i]]$x-V(g_mst)[g_order[i+1]]$x)^2+(V(g_mst)[g_order[i]]$y-V(g_mst)[g_order[i+1]]$y)^2)
      cost <- cost + wt
      att_wt <- c(att_wt,wt)
      #flush.console()
      #cat(sqrt((V(g_mst)[g_order[i]]$x-V(g_mst)[g_order[i+1]]$x)^2+(V(g_mst)[g_order[i]]$y-V(g_mst)[g_order[i+1]]$y)^2),"\n")
      #Sys.sleep(time=0.05)
    }
  }
  cost
  #Q10
  g_mst_2 <- set_edge_attr(g_mst_2,"weight", value = att_wt)
  plot(g_mst_2,vertex.size=3, vertex.label=NA)
}

#Part 3

if(DO_11){
  X <- ppp(V(g_sub)$x, V(g_sub)$y, c(-150,150), c(-40,40))
  d_X <- deldir(X)
  plot(d_X)
  
  detail <- d_X$dirsgs
  
  eid_keep <- c()
  for(i in 1:dim(detail)[1]){
    eid <- get.edge.ids(g_sub, c(detail[i,5],detail[i,6]))
    #if(eid != 0)
    eid_keep <- c(eid_keep, eid)
    #else
    
  }
  g_3 <- subgraph.edges(g_sub, eid_keep[eid_keep!=0], delete.vertices = TRUE)
}




