#install.packages("igraph",lib="/Users/evelyn/Library/R")
#install.packages("Matrix",lib="/Users/evelyn/Library/R")
#install.packages("pracma",lib="/Users/evelyn/Library/R")
#install.packages("data.table",lib="/Users/evelyn/Library/R")

library('igraph', lib="/Users/evelyn/Library/R")
library('Matrix', lib="/Users/evelyn/Library/R")
library('pracma', lib="/Users/evelyn/Library/R")
library('data.table', lib="/Users/evelyn/Library/R")

create_transition_matrix = function (g,alpha=0){
  
  adj = as_adjacency_matrix(g)
  
  adj_ = adj
  adj_[adj==1] = 0
  adj_[adj==0] = 1
  
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

random_walk = function (g, num_steps, start_node, transition_matrix = NULL, alpha=0){
  if(is.null(transition_matrix))
    transition_matrix = create_transition_matrix(g,alpha)
  
  his = c()
  v = start_node
  for(i in 1:num_steps){
    #fprintf('Step %d: %d\n', i, v)  # COMMENT THIS
    his = c(his,v)
    if(v==1 && alpha==0){
      break
    }
    PMF = transition_matrix[v, ]
    v = sample(1:vcount(g), 1, prob = PMF) 
  }
  
  return(his)
}

g1 = sample_pa(1000,m=4)
trans_mat <- create_transition_matrix(g1)
his_arr = matrix(0,1,1000)

#2-3a
for(i in c(1:1000,1:1000)){
  dists = c();
  print(i)
  his= random_walk(g1, 10, i, trans_mat)
  his_arr[his] = his_arr[his]+1
}
his_arr = his_arr/2000
print(his_arr)
plot(1:1000,his_arr,type='l',main="Probability that the walker visits each node",xlab="Node added at time t",ylab="Probability")

#2-3b
trans_mat <- create_transition_matrix(g1,0.15)
his_arr = matrix(0,1,1000)

for(i in c(1:1000,1:1000)){
  dists = c();
  print(i)
  his= random_walk(g1, 20, i, trans_mat, 0.15)
  his_arr[his] = his_arr[his]+1
}
his_arr = his_arr/2000
print(his_arr)
plot(1:1000,his_arr,type='l',main="Probability that the walker visits each node",xlab="Node added at time t",ylab="Probability")

