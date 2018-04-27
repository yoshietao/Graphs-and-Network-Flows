library('igraph')
library('Matrix')
library('pracma')
library('data.table')

file = 'facebook_combined.txt'

handle_144 = function(Ri,Pi){
  print(Ri)
  print(Pi)
  acc = length(intersect(Ri,Pi))/length(Ri)
  print(acc)
  return (acc)
}

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

Jaccard = function(sg, v, i, len_Ri){
  Si = neighbors(sg,i)
  CNeighbor = c()
  for(j in v){
    Sj = neighbors(sg, j)
    CNeighbor = c(CNeighbor, length(intersection(Si,Sj))/length(union(Si,Sj)))
  }
  re = sort(CNeighbor, decreasing = TRUE, index.return=TRUE)
  return(v[re$ix[1:len_Ri]])
}

adamic = function(sg, v, i, len_Ri){
  Si = neighbors(sg,i)
  CNeighbor = c()
  for(j in v){
    Sj = neighbors(sg, j)
    inter = intersection(Si,Sj)
    summ = 0
    for(k in inter){
      summ = summ + 1/log10(length(neighbors(sg,k)))
    }
    CNeighbor = c(CNeighbor, summ)
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

measure1 <- c()
measure2 <- c()
measure3 <- c()

for (i in list_24){
  neigh <- neighbors(sg,i)
  p <- runif(length(neigh),0,1)
  # Ri is a list of friends deleted 
  Ri <- neigh[which(p<0.25)]
  len_Ri <- length(Ri)
  
  #preprocess nodes that belongs to j
  t <- !(neigh %in% Ri)
  nei <- neigh[t]
  t <- !(v %in% nei)
  # common neighbors
  #Pi <- common_neighbors(sg,v[t],i,len_Ri)
  #measure1 <- c(measure1, handle_144(Ri,Pi))
  # Jaccard
  #Pi <- Jaccard(sg,v[t],i,len_Ri)
  #measure2 <- c(measure1, handle_144(Ri,Pi))
  #adamic
  Pi <- adamic(sg,v[t],i,len_Ri)
  measure3 <- c(measure1, handle_144(Ri,Pi))

}

#intersection(n1,n7)
#length(intersection(n1,n7))
#union(n1,n7)
#length(union(n1,n7))

