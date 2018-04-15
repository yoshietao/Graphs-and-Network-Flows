library('igraph', lib="/Users/evelyn/Library/R")
library('Matrix', lib="/Users/evelyn/Library/R")
library('pracma', lib="/Users/evelyn/Library/R")

#1-3a
g1 <- sample_pa_age(1000, pa.exp=1, aging.exp=-1, aging.bin=1000, directed=F)
plot(g1,vertex.size=4, vertex.label=NA)
plot(degree.distribution(g1),main="Degree distribution of 1-3",xlab="Degree",ylab="Frequency")
plot(degree.distribution(g1),main="Degree distribution-loglog of 1-3",xlab="Degree",ylab="Frequency",log='xy')

#1-3b
fc1 <- cluster_fast_greedy(g1)
print(modularity(fc1))