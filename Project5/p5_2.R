library(igraph)
library(data.table)
library(rjson)
library(spatstat)
library(deldir)

DO_6 = TRUE
DO_7 = TRUE
DO_8 = FALSE
DO_9 = TRUE
DO_11= TRUE
DO_12 = TRUE
DO_13 = TRUE
DO_14 = TRUE
DO_15 = TRUE

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

mtt_d_ratio <- 6233.923

#Q6:
if (DO_6 == TRUE || DO_8 == TRUE || DO_11 == TRUE){
  g <- simplify(g, remove.multiple = TRUE, remove.loops = TRUE, edge.attr.comb="mean")
  clu <- components(g)
  g_sub <- induced.subgraph(g, which(clu$membership == which.max(clu$csize)))
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
}

# g is g_sub for Q8, g is delta_g_3 for Q15, e_attr is "mtt" for Q8, e_attr is $time for Q15
# percentage_tri_ineq(g_sub, "mtt")
# percentage_tri_ineq(delta_g_3, "time")
percentage_tri_ineq <-function(g, e_attr){
  tri_ineq_true <- 0
  tri_ineq_false <- 0
  count <- 0
  while (count < 1000){
    v <- sample(1:vcount(g), 1, replace=FALSE)
    neigh_list <- neighbors(g, v)
    random_two <- sample (neigh_list, size=2, replace=FALSE)
    while ( (are.connected(g, random_two[1], random_two[2]) == FALSE)){
      random_two <- sample (neigh_list, size=2, replace=FALSE)
    }
    are.connected(g, random_two[1], random_two[2])
    #v_list <- c(get_weight(g_sub,v,random_two[1]),get_weight(g_sub,v,random_two[2]),get_weight(g_sub,random_two[2],random_two[1]))
    v_list <- get.edge.ids(g, c(v, random_two[1], v, random_two[2], random_two[1], random_two[2]))
    for(i in v_list)
      weights <- edge_attr(g, e_attr, index = v_list) # "mtt" for e_attr for Q8 
    if (weights[1]+weights[2]>weights[3] && weights[1]+weights[3]>weights[2] && weights[2]+weights[3]>weights[1])
      tri_ineq_true <- tri_ineq_true + 1
    else 
      tri_ineq_false <- tri_ineq_false + 1
    count <- count +1
  }
  tri_ineq_true
  tri_ineq_false
  rate = tri_ineq_true/(tri_ineq_true+tri_ineq_false)
}

#Q8
if (DO_8 == TRUE){
  percentage_tri_ineq(g_sub, "mtt")
}

#Q9---->map missing path to time weighted shortest path(distance)
if(DO_9 == TRUE){
  
  get_weight <-function(a,b){
    if(get.edge.ids(g_sub,c(a,b))==0){
      path <- shortest_paths(g_sub, from = a, to = b, weights=E(g_sub)$mtt)
      tot <- 0
      for(k in 1:(length(path$vpath[[1]])-1))
        tot <- tot+E(g_sub)[get.edge.ids(g_sub,c(path$vpath[[1]][k],path$vpath[[1]][k+1]))]$mtt
      return(tot)
    }else
      return(E(g_sub)[get.edge.ids(g_sub,c(a,b))]$mtt)
  }
  
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
      #flush.console()
      #cat(g_order[i]," ",g_order[i+1]," ",wt,"\n")
      #Sys.sleep(time=0.05)
      wt <- get_weight(g_order[i],g_order[i+1])
      cost <- cost + wt
      att_wt <- c(att_wt,wt)
    }
  }
  cost <- cost + mtt_d_ratio*sqrt((V(g_mst)[g_order[1]]$x-V(g_mst)[g_order[1880]]$x)^2+(V(g_mst)[g_order[1]]$y-V(g_mst)[g_order[1880]]$y)^2)
  #Q10
  g_mst_2 <- set_edge_attr(g_mst_2,"weight", value = att_wt)
  coord <- data.frame(V(g_mst)$x,V(g_mst)$y)
  coord <- as.matrix(coord)
  plot(g_mst_2,vertex.size=3, vertex.label=NA, layout=coord)
}

#Part 3



if(DO_11 || DO_12){
  #Q11
  X <- ppp(V(g_sub)$x, V(g_sub)$y, c(-150,150), c(-40,40))
  d_X <- deldir(X)
  #plot(d_X)
  
  detail <- d_X$delsgs
  
  g_3 <- make_empty_graph(n = vcount(g_sub), directed = FALSE)
  #eid_keep <- c()
  for(i in 1:dim(detail)[1]){
    g_3 <- add_edges(g_3,c(detail[i,5],detail[i,6]))
    #eid <- get.edge.ids(g_sub, c(detail[i,5],detail[i,6]))
    #if(eid != 0)
    #eid_keep <- c(eid_keep, eid)
    #else
  }
  for(i in 1:vcount(g_3)){
    V(g_3)[i]$x <- V(g_mst)[i]$x
    V(g_3)[i]$y <- V(g_mst)[i]$y
  }
  plot(g_3,vertex.size=3, vertex.label=NA, layout=coord)
  
  # Q12
  euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))
  
  totalpath_distance <- function (path_list) {
    l <- length(path_list$vpath[[1]])
    sum <- 0
    for (i in 1: (l-1)){
      node_1 = path_list$vpath[[1]][i]
      node_2 = path_list$vpath[[1]][i+1]
      x_coord = vertex_attr(g_sub, "x", c(node_1, node_2))
      y_coord = vertex_attr(g_sub, "y", c(node_1, node_2))
      sum <- sum + euc.dist (c(x_coord[1], y_coord[1]), c(x_coord[2], y_coord[2]))
    }
    return (sum)
  }
  
  # g = g_3 for Q12, g = delta_g_3 for Q15 
  get_car_per_hour <- function (g) {
    # return car per hour vector for each edge 
    car_length <- 0.003
    mi_per_deg <- 69
    bet_car_t <- 2
    
    num_car_per_hour <- c()
    t_list <- c()
    count <- 0
    
    for (e in 1:ecount(g)){
      # for (e in 1:10){
      
      # when edge weight isn't null in g_sub 
      
      # get two nodes on each side, and check location
      node_1_2 <- ends (g, e)
      x_coord <- vertex_attr(g, "x", node_1_2)
      y_coord <- vertex_attr(g, "y", node_1_2)
      # get edge id from original graph 
      e_sub <- get.edge.ids(g_sub, node_1_2)
      # if there's no edge in original graph, then we approx by shortest path 
      if (e_sub == 0){
        path_list <- shortest_paths(g_sub, node_1_2[1], node_1_2[2], weights = E(g_sub)$mtt)
        t <- distances(g_sub, node_1_2[1], node_1_2[2], weights = E(g_sub)$mtt)
        d <- totalpath_distance(path_list)
      }
      else {
        t <- edge_attr(g_sub, "mtt", e_sub)
        d <- euc.dist (c(x_coord[1], y_coord[1]), c(x_coord[2], y_coord[2]))
      }
      t_list <- c(t_list, t)
      speed <- d * mi_per_deg / t
      t_per_car <- car_length / speed
      
      # total time per car = time for between car and for car
      total_t_car <- t_per_car + bet_car_t
      # in hour, times 2 because two on each side on the road
      num_car_per_hour <- c(num_car_per_hour, 60*60/total_t_car*4)
      
      count <- count +1
      
    }
    # list of num_car_per_hour for each edge, first is edge 1...etc
    num_car_per_hour
    length(num_car_per_hour)
    
    t_list 
    length (t_list)
    
    return (num_car_per_hour)
  }
  
  Q13_num_car_per_hour <- get_car_per_hour(g_3)
}

if (DO_13){
  traffic_flow <- function (path_list) {
    l <- length(path_list$vpath[[1]])
    sum <- 0
    flow <- c()
    for (i in 1: (l-1)){
      node_1 = path_list$vpath[[1]][i]
      node_2 = path_list$vpath[[1]][i+1]
      e <- get.edge.ids(g_3, c(node_1,node_2))
      flow <- c(flow, num_car_per_hour[e])
    }
    return (flow)
  }
  
  get_maxflow_stanford_to_cruz <- function (g,num_car_per_hour){
    source_addr <- "100 Campus Drive, Stanford"
    dest_addr <- "700 Meder Street, Santa Cruz"
    source_node <- match(source_addr, V(g_sub)$st_add)
    dest_node <- match(dest_addr, V(g_sub)$st_add)
    edge_connectivity(g,source_node, dest_node)
    d <- distances(g, source_node, dest_node, weights = num_car_per_hour)
    path_list <- shortest_paths(g, source_node, dest_node, weights = num_car_per_hour)
    # traffic_list <- traffic_flow (path_list)
    # max(traffic_list)
    flow <- max_flow(g, source_node, dest_node, capacity = num_car_per_hour)
    flow $value
  }
  get_maxflow_stanford_to_cruz (g_3, Q13_num_car_per_hour)

}

if (DO_14){
  golden_gate_bridge = list (c(-122.475, 37.806), c(-122.479, 37.83))
  san_rafael_bridge = list(c(-122.501, 37.956), c(-122.387, 37.93))
  san_mateo_bridge = list(c(-122.273, 37.563), c(-122.122, 37.627))
  dambarton_bridge = list(c(-122.142, 37.486), c(-122.067, 37.54))
  oakland_bay_bridge = list(c(-122.388, 37.788), c(-122.302,37.825))
  
  E(g_3)$time <- as.matrix(t_list)
  plot(g_3,vertex.size=3, vertex.label=NA, layout=coord)
  max_threshold = 1000 
  delta_g_3 <- delete.edges(g_3, which(E(g_3)$time >= max_threshold))
  plot(delta_g_3,vertex.size=3, vertex.label=NA, layout=coord)
  
  # have to check the time for those bridges 
  # get node id first 
  # golden_node <- match(golden_gate_bridge[[1]], c(V(g_sub)$x, V(g_sub)$y))
  # vertex_attr(g_sub, "x", 1)
  # match(-122.2194, V(g_sub)$x)
  # v <- (which(V(g_sub)$x ==-122.2194)) #can't find bridge in original graph
  # e_list <- get.edge.ids(g_3, c(golden_gate_bridge[[1]], golden_gate_bridge[[2]]))
  # edge_attr(g_3, "time", e_list)
}

if (DO_15){
  # repeat Q12 
  Q15_num_car_per_hour <- get_car_per_hour(delta_g_3)
  
  # repeat Q13 
  get_maxflow_stanford_to_cruz (delta_g_3, Q15_num_car_per_hour)
}




