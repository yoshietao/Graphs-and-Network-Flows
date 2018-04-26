library('igraph')
library('Matrix')
library('pracma')
library('data.table')

file = 'facebook_combined.txt'

d <- read.table(file, sep = " ")

neighbor = c(0)
g1_data = matrix(0,0,2)

for (i in 1:dim(d)[1]){
  if (d[i,1]==0)
    neighbor = c(neighbor,d[i,2])
}

for (i in 1:dim(d)[1]){
  print(d[i,1])
  if(d[i,1]>neighbor[length(neighbor)])
    break
  else{
    for(j in 1:length(neighbor))
      if(d[i,1]==neighbor[j])
        for(k in j:length(neighbor))
          if(d[i,2]==neighbor[k]){
            g1_data = rbind(g1_data,matrix(c(d[i,1],d[i,2]),1,2))
          }
  }
}

print(g1_data)
write.table(g1_data, 't2.txt', append = FALSE, sep = " ",row.names = FALSE, col.names = FALSE)
my_g <- read_graph('t2.txt',directed=FALSE)
plot(my_g,vertex.size=4, vertex.label=NA)
