library(igraph)
library(data.table)
library(rjson)

DO_6 = TRUE
DO_7 = TRUE
DO_8 = FALSE
DO_9 = TRUE

#setwd("/Users/evelyn/Documents/master_first_year/third_quarter/ECE232/Random-Graphs-and-Random-Walks/Project5/")
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
if (DO_6 == TRUE || DO_8 == TRUE){
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
  triangles_index <- triangles(g_sub) # this will output a list of triangles, first three vertices represent first triangle...etc 
  # pick 1000 from number of triangles
  rand_1000_triangles_index <- sample(1:length(triangles_index)/3, 1000, replace=F)
  length(rand_1000_triangles_index)
  rand_1000_triangles <- c()
  tri_ineq_true <- 0
  tri_ineq_false <- 0
  euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))
  for (t in rand_1000_triangles_index){
    pt1 <- triangles_index[(t-1)*3+1]
    pt2 <- triangles_index[(t-1)*3+2]
    pt3 <- triangles_index[(t-1)*3+3]
    v_list <- get.edge.ids(g_sub, c(pt1, pt2, pt1, pt3, pt2, pt3))
    weights <- edge_attr(g_sub, "mtt", index = v_list)
    # v_list = c(pt1,pt2,pt3)
    # x_coord <- vertex_attr(g_sub, "x", v_list)
    # y_coord <- vertex_attr(g_sub, "y", v_list)
    # dist1_2 <- euc.dist(c(x_coord[1],y_coord[1]),c(x_coord[2],y_coord[2]))
    # dist2_3 <- euc.dist(c(x_coord[2],y_coord[2]),c(x_coord[3],y_coord[3]))
    # dist1_3 <- euc.dist(c(x_coord[1],y_coord[1]),c(x_coord[3],y_coord[3]))
    # if (dist1_2 + dist2_3 > dist1_3 && dist1_2 + dist1_3 > dist2_3 && dist2_3 + dist1_3 > dist1_2){
    #  tri_ineq_true <- tri_ineq_true + 1
    #}
    #else{
    #  tri_ineq_false <- tri_ineq_false + 1
    #}
    if (weights[1]+weights[2]>weights[3] && weights[1]+weights[3]>weights[2] && weights[2]+weights[3]>weights[1])
      tri_ineq_true <- tri_ineq_true + 1
    else
      tri_ineq_false <- tri_ineq_false + 1
  }
  tri_ineq_true
  tri_ineq_false
  
  # return tri_ineq_true / (tri_ineq_true + tri_ineq_false)
}

#Q9---->I ignored the invalid path, so the total cost is smaller than it should be
if(DO_9 == TRUE){
  #visited <- c()
  #next = 1
  #g_mst_2 <- as.directed(g_mst, mode = "mutual")
  g_dfs <- dfs(g_mst, root=1, neimode ="all")
  g_order <- as.matrix(g_dfs$order)
  cost <- 0.0
  for(i in 1:(vcount(g_mst)-1)){
    if(length(E(g_mst)[get.edge.ids(g_mst, c(g_order[i],g_order[i+1]))]$mtt)>0){
      cost <- cost + E(g_mst)[get.edge.ids(g_mst, c(g_order[i],g_order[i+1]))]$mtt
    }
    #flush.console()
    #cat(is.numeric(E(g_mst)[get.edge.ids(g_mst, c(g_order[i],g_order[i+1]))]$mtt),"\n")
    #Sys.sleep(time=0.05)
  }
  cost
}

#Q10





