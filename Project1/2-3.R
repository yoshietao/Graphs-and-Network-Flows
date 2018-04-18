#install.packages("igraph",lib="/Users/evelyn/Library/R")
#install.packages("Matrix",lib="/Users/evelyn/Library/R")
#install.packages("pracma",lib="/Users/evelyn/Library/R")
#install.packages("data.table",lib="/Users/evelyn/Library/R")

library('igraph', lib="/Users/evelyn/Library/R")
library('Matrix', lib="/Users/evelyn/Library/R")
library('pracma', lib="/Users/evelyn/Library/R")
library('data.table', lib="/Users/evelyn/Library/R")

# create_transition_matrix = function (g,alpha=0){
#   
#   adj = as_adjacency_matrix(g)
#   
#   adj_ = adj
#   adj_[adj==1] = 0
#   adj_[adj==0] = 1
#   
#   t1 <- get_t_matrix(g, adj)
#   t2 <- get_t_matrix(g, adj_)
#   return(alpha*t2+(1-alpha)*t1)
# }

get_t_matrix <- function(g,adj){
  adj[diag(rowSums(adj) == 0)] = 1  # handle if the user is using the function for networks with isolated nodes by creating self-edges
  z = matrix(rowSums(adj, , 1))
  transition_matrix = adj / repmat(z, 1, vcount(g))  # normalize to get probabilities
  return(transition_matrix)
}

random_walk = function (g, num_steps, start_node, t1, t2, alpha=0){
    
  his = c()
  v = start_node
  for(i in 1:num_steps){
    #fprintf('Step %d: %d\n', i, v)  # COMMENT THIS
    his = c(his,v)
    if(v==1 && alpha==0){
      break
    }
    x = sample(0:1,1)
    if(x<=0.15){
      PMF = t2[v, ]
      v = sample(1:vcount(g), 1, prob = PMF) 
    }
    else{
      PMF = t1[v, ]
      v = sample(1:vcount(g), 1, prob = PMF) 
    }
  }
  return(his)
}

#2-3a
g = sample_pa(1000,m=4)
trans_mat <- create_transition_matrix(g1)
p = matrix(0,1,1000)

adj = as_adjacency_matrix(g)
adj_ = adj
adj_[adj==1] = 0
adj_[adj==0] = 1
t1 <- get_t_matrix(g, adj)
t2 <- get_t_matrix(g, adj_)

for(i in c(1:1000)){
  print(i)
  his= random_walk(g, 10, i, t1, t2)
  for(i in his){p[i] = p[i]+1}
}
p = p/10000
print(p)
plot(1:1000,p,type='l',main="Probability that the walker visits each node",xlab="Node added at time t",ylab="Probability")

#2-3b
adj = as_adjacency_matrix(g)
adj_ = adj
adj_[adj==1] = 0
adj_[adj==0] = 1
t1 <- get_t_matrix(g, adj)
t2 <- get_t_matrix(g, adj_)
#trans_mat <- create_transition_matrix(g1,0.15)
p = matrix(0,1,1000)

for(i in c(1:100)){
  print(i)
  his= random_walk(g, 1000, i, t1, t2, 0.15)
  for(i in his){p[i] = p[i]+1}
}

p = p/100000
print(p)
plot(1:1000,p,type='l',main="Probability that the walker visits each node",xlab="Node added at time t",ylab="Probability")


