library('igraph')
library('Matrix')
library('pracma')
library('data.table')

file = 'facebook_combined.txt'

common_neighbors = function(sg, v, i, len_Ri){
  Si = neighbors(sg,i)
  CNeighbor = c()
  for(j in v){
    Sj = neighbors(sg, j)
    CNeighbor = c(CNeighbor, length(intersection(Si,Sj)))
  }
  re = sort(CNeighbor, decreasing = TRUE, index.return=TRUE)
  return(v[re$ix[1:len_Ri]])
}

# convert the edge list into a list
graph_list = c() 
d <- read.table(file, sep = " ")
for (i in 1:dim(d)[1]){
  for (j in 1:dim(d)[2]){
    graph_list = c(graph_list, d[i,j])
  }
}
# create a graph using the list. add 1 to each node in edgelist
g <- make_graph(graph_list+1, directed = FALSE)
plot(g, vertex.size=4, vertex.label=NA)

# create personalized network centered around node 415
center = 415
node_center <- neighbors(g, 415)
nn <- as_ids(node_center)
nn <- c(nn,center)
sg <- induced_subgraph(g, nn)

# plot personalized network
plot(sg, vertex.size=4, vertex.label=NA)

# create a list that has degree 24
deg <- degree(sg)
list_24 <- which ((deg==24)==TRUE)
list_24

# compute the length of the list
length(list_24)

#vertices in personalized network centered at 415
v = V(sg)

for (i in list_24){
  neigh <- neighbors(sg,i)
  p <- runif(length(neigh),0,1)
  # Ri is a list of friends deleted 
  Ri <- neigh[which(p<0.25)]
  len_Ri <- length(Ri)
  print(len_Ri)
  print(Ri)
  #
  t <- !(neigh %in% Ri)
  nei <- neigh[t]
  t <- !(v %in% nei)
  Pi <- common_neighbors(sg,v[t],i,len_Ri)
  print(Pi)
}

#intersection(n1,n7)
#length(intersection(n1,n7))
#union(n1,n7)
#length(union(n1,n7))

