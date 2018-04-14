#install.packages("igraph",lib="/Users/evelyn/Library/R")
#install.packages("Matrix",lib="/Users/evelyn/Library/R")
#install.packages("pracma",lib="/Users/evelyn/Library/R")

library('igraph', lib="/Users/evelyn/Library/R")
library('Matrix', lib="/Users/evelyn/Library/R")
library('pracma', lib="/Users/evelyn/Library/R")

#1-2a
g1 <- barabasi.game(1000, m=1, directed=F)
plot(g1,vertex.size=8, vertex.label=NA)
is.connected(g1)

#1-2b
fc <- cluster_fast_greedy(g1)
modularity(fc)

#1-2c
g2 <- barabasi.game(10000, m=1, directed=F)
plot(g2,vertex.size=8, vertex.label=NA)
fc2 <- cluster_fast_greedy(g2)
modularity(fc2)

#1-2d
plot(degree.distribution(g1),main="Degree distribution of the network with 1000 nodes",xlab="Degree",ylab="Frequency",log='xy')
plot(degree.distribution(g2),main="Degree distribution of the network with 10000 nodes",xlab="Degree",ylab="Frequency",log='xy')

#1-2e
# x is a random vertex
x <- sample(1:1000, 1)
x
n1 <- neighbors(g1, x)
n1
# this outputs a random neighbor of x
sample(n1,1)

#y <- sample(1:10000, 1)
#n2 <- neighbors(g2, y)
#length(n2)