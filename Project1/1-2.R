#install.packages("igraph",lib="/Users/evelyn/Library/R")
#install.packages("Matrix",lib="/Users/evelyn/Library/R")
#install.packages("pracma",lib="/Users/evelyn/Library/R")
#install.packages("data.table",lib="/Users/evelyn/Library/R")

library('igraph', lib="/Users/evelyn/Library/R")
library('Matrix', lib="/Users/evelyn/Library/R")
library('pracma', lib="/Users/evelyn/Library/R")
library('data.table', lib="/Users/evelyn/Library/R")

func_atof <- function(m){
  #1-2a
  g1 <- barabasi.game(1000, m=m, directed=F)
  plot(g1,vertex.size=4, vertex.label=NA)
  print(is.connected(g1))
  
  #1-2b
  fc <- cluster_fast_greedy(g1)
  print(modularity(fc))
  
  #1-2c
  g2 <- barabasi.game(10000, m=m, directed=F)
  plot(g2,vertex.size=4, vertex.label=NA)
  fc2 <- cluster_fast_greedy(g2)
  print(modularity(fc2))
  
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
  
  #1-2f
  deg_age <-rev(degree(g2)[1:1000])
  plot(deg_age,type='l',main="Degrees of the age 1 to 1000 with n=10000",xlab="Age",ylab="Degree")
  return (modularity(fc))
}

#1-2g
mod1 <- func_atof(1)
mod2 <- func_atof(2)
mod3 <- func_atof(5)

print(mod1)
print(mod2)
print(mod3)

#1-2h
g3 <- barabasi.game(1000, m=1, directed=F)
#plot(g3,vertex.size=4, vertex.label=NA)
deg_seq = degree(g3)

fc3 <- cluster_fast_greedy(g3)
plot(fc3,g3,vertex.size=4,vertex.label=NA)
print(modularity(fc3))

#g4 <- sample_degseq(deg_seq,method='simple.no.multiple')
g4 <- sample_degseq(deg_seq,method='vl')
#plot(g4,vertex.size=4, vertex.label=NA)

fc4 <- cluster_fast_greedy(g4)
plot(fc4,g4,vertex.size=4,vertex.label=NA)
print(modularity(fc4))

