#install.packages("igraph",lib="/Users/evelyn/Library/R")
#install.packages("Matrix",lib="/Users/evelyn/Library/R")
#install.packages("pracma",lib="/Users/evelyn/Library/R")
#install.packages("data.table",lib="/Users/evelyn/Library/R")

library('igraph', lib="/Users/evelyn/Library/R")
library('Matrix', lib="/Users/evelyn/Library/R")
library('pracma', lib="/Users/evelyn/Library/R")
library('data.table', lib="/Users/evelyn/Library/R")



#functions from ccle

create_transition_matrix = function (g){
  
  # WARNING: make sure your graph is connected (you might input GCC of your graph)
  
  vs = V(g)
  n = vcount(g)
  adj = as_adjacency_matrix(g)
  adj[diag(rowSums(adj) == 0)] = 1  # handle if the user is using the function for networks with isolated nodes by creating self-edges
  z = matrix(rowSums(adj, , 1))
  
  transition_matrix = adj / repmat(z, 1, n)  # normalize to get probabilities
  
  return(transition_matrix)
}

random_walk = function (g, num_steps, start_node, transition_matrix = NULL){
  if(is.null(transition_matrix))
    transition_matrix = create_transition_matrix(g)
  
  v = start_node
  for(i in 1:num_steps){
    fprintf('Step %d: %d\n', i, v)  # COMMENT THIS
    PMF = transition_matrix[v, ]
    v = sample(1:vcount(g), 1, prob = PMF)        
  }
  
  return(v)
}

#2-2a

n <- 1000
g1 <- barabasi.game(n, m=1, directed=F)
plot(g1,vertex.size=4, vertex.label=NA)

#2-2b

s_t_list = c()
va_list = c()

t_range = 1:20
sample_times = 20

trans_mat <- create_transition_matrix(g1)
paths_ <- shortest.paths(g1)

for (t in t_range) {
  dists = c()
  samp = sample(1:n, sample_times)
  for (s in samp) {
    d = random_walk(g1, t, s, trans_mat)
    dists = c(dists, paths_[s,d])
  }
  s_t_list = c(s_t_list, mean(dists))
  va_list = c(va_list, var(dists))
  
}
#png(sprintf("p2b_%f_st.png", n), 900, 900)
plot(t_range, s_t_list, main = "Average distance vs time", xlab = "t", ylab = "Mean Shortest Distance")
#dev.off()
#png(sprintf("p2b_%f_ot.png", n), 900, 900)
plot(t_range, va_list, main = "Variance vs time", xlab="t", ylab="Variance of Shortest Distance")
#dev.off()
