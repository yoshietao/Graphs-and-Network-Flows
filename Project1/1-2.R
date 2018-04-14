#install.packages("igraph",lib="/Users/evelyn/Library/R")
#install.packages("Matrix",lib="/Users/evelyn/Library/R")
#install.packages("pracma",lib="/Users/evelyn/Library/R")
#install.packages("data.table",lib="/Users/evelyn/Library/R")

library('igraph', lib="/Users/evelyn/Library/R")
library('Matrix', lib="/Users/evelyn/Library/R")
library('pracma', lib="/Users/evelyn/Library/R")
library('data.table', lib="/Users/evelyn/Library/R")

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
func_1e <- function(g){
  degreeVec <- c()
  for(i in 1:2000) {
    if(g == "g1") {
      n <- 1000
      x <- sample(1:n, 1)
      n1 <- neighbors(g1, x)
      j <- sample(n1,1)
      degreeVec <- c(degreeVec, degree(g1,j))
    }
    if(g == "g2") {
      n <- 10000
      x <- sample(1:n)
      n1 <- neighbors(g2, x)
      j <- sample(n1,1)
      degreeVec <- c(degreeVec, degree(g2,j))
    }
  }
  degrees <- data.table(value = degreeVec)
  freqs <- degrees[, .(N = .N), by = value][, freq := N / sum(N)]
  setorder(freqs, value)
  plot(freqs$value, freqs$freq, log='xy', main=paste("Degree distribution of the network with", n, "nodes"), xlab="Degree",ylab="Frequency")
}

func_1e("g1")
func_1e("g2")