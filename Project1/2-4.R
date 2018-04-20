#install.packages("igraph",lib="/Users/evelyn/Library/R")
#install.packages("Matrix",lib="/Users/evelyn/Library/R")
#install.packages("pracma",lib="/Users/evelyn/Library/R")
#install.packages("data.table",lib="/Users/evelyn/Library/R")

library('igraph', lib="/Users/evelyn/Library/R")
library('Matrix', lib="/Users/evelyn/Library/R")
library('pracma', lib="/Users/evelyn/Library/R")
library('data.table', lib="/Users/evelyn/Library/R")

create_transition_matrix = function (g, pr, alpha=0){
  adj = as_adjacency_matrix(g)
  
  adj = matrix(adj)
  dim(adj) <- c(1000,1000)
  
  adj_ = adj
  adj_ = matrix(adj_)
  dim(adj_) <- c(1000,1000)
  
  adj_[adj==1] = 0
  adj_[adj==0] = repmat(pr,1000,1)[adj==0]
  
  t1 <- get_t_matrix(g, adj)
  t2 <- get_t_matrix(g, adj_)
  return(alpha*t2+(1-alpha)*t1)
}

create_transition_matrix2 = function (g, i1, i2, alpha=0){
  print(i1)
  print(i2)
  adj = as_adjacency_matrix(g)
  
  adj = matrix(adj)
  dim(adj) <- c(1000,1000)
  
  adj_ = matrix(0,1000,1000)
  adj_[,i1] = 0.5
  adj_[,i2] = 0.5
  
  t1 <- get_t_matrix(g, adj)
  t2 <- get_t_matrix(g, adj_)
  return(alpha*t2+(1-alpha)*t1)
}

get_t_matrix <- function(g,adj){
  adj[diag(rowSums(adj) == 0)] = 1  # handle if the user is using the function for networks with isolated nodes by creating self-edges
  z = matrix(rowSums(adj, , 1))
  transition_matrix = adj / repmat(z, 1, vcount(g))  # normalize to get probabilities
  return(transition_matrix)
}

random_walk = function (g, num_steps, start_node, t, alpha){
  
  his = c()
  v = start_node
  for(i in 1:num_steps){
    #fprintf('Step %d: %d\n', i, v)  # COMMENT THIS
    his = c(his,v)
    PMF = t[v, ]
    v = sample(1:vcount(g), 1, prob = PMF) 
  }
  return(his)
}

#2-4a
g <- sample_pa(1000,m=4)
p_rank <- page_rank(g)$vector
trans_mat <- create_transition_matrix(g, p_rank, 0.15)
p = matrix(0,1,1000)

for(i in c(1:100)){
  print(i)
  his= random_walk(g, 1000, i, trans_mat, 0.15)
  for(i in his){p[i] = p[i]+1}
}

p = p/100000
print(p)
plot(1:1000,p,type='l',main="Probability that the walker visits each node",xlab="Node added at time t",ylab="Probability")

#2-4b
sor = sort(p_rank,index.return = TRUE)
x1 = sor$x[500]
x2 = sor$x[501]
ix1 = 0
ix2 = 0
tmp = 0
for (i in 1:1000){
  if(tmp!=x1 && sor$x[i]==x1){ix1 = sor$ix[i]}
  if(tmp==x2 && sor$x[i]!=x2){ix2 = sor$ix[i-1]}
  tmp = sor$x[i]
}

trans_mat <- create_transition_matrix2(g, ix1, ix2, 0.15)
p = matrix(0,1,1000)

for(i in c(1:100)){
  print(i)
  his= random_walk(g, 1000, i, trans_mat, 0.15)
  for(i in his){p[i] = p[i]+1}
}

p = p/100000
print(p)
plot(1:1000,p,type='l',main="Probability that the walker visits each node",xlab="Node added at time t",ylab="Probability")



