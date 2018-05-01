library('igraph')
library('Matrix')
library('pracma')
library('data.table')

DO_18 = FALSE; 
DO_19 = FALSE;
DO_20 = FALSE;

# Problem 18 
# create directed personal networks for users who have more than 2 circles
path = "gplus/"

if (DO_18){
  out.file<-""
  file.names <- dir(path, pattern =".circles")
  file.names
  count <- 0
  for (i in 1:length(file.names)){
    # skip empty files
    if (file.size(paste0('gplus/',file.names[i])) == 0){
      print (file.names[i])
      next
    }
    else {
    # count number of circles files with more than 2 circles 
      d <- read.table(paste0('gplus/',file.names[i]), sep = " ")
      if (dim(d)[1]>2){
        count <- count +1
      }
    }
  }
  # return count 
  count
}

# Problem 19, 20
# Create Directed Graphs from the nodeId.edges files 
# file = "test.txt"
node_list <- c("109327480479767108490", 
               "115625564993990145546",
               "101373961279443806744")
extension <- ".edges"
for (node_num in node_list){
    graph_list = c() 
    d <- read.table(paste0(path,node_num,extension), sep = " ")
    # d <- read.table("test.txt", sep = " ")
    # add edges from ego node to all other nodes 
    graph_list = c() 
    for (i in 1:dim(d)[1]){
      for (j in 1:dim(d)[2]){
        graph_list = c(graph_list, d[i,j])
      }
    }
    for (unique_node in unique(graph_list)){
      d <- rbind( d, data.frame("V1"=node_num, "V2"=unique_node))
    }
    # create directed graph from data frame
    g <- graph_from_data_frame(d,directed=TRUE)
    plot(g, vertex.size=4, vertex.label=NA)
    
  if (DO_19){
    # Problem 19
    # plot degree distribution for out degree 
    degrees <- degree(g, mode="out")
    degreeNum <- as.numeric(names(degrees))
    degreeFreq <- unname(degrees)/sum(degrees)
    plot(degreeNum, degreeFreq,main=paste("Degree Distribution for out-degree for node", node_num),xlab="Degree",ylab="Frequency")
    
    # plot degree distribution for in degree 
    degrees <- degree(g, mode="in")
    degreeNum <- as.numeric(names(degrees))
    degreeFreq <- unname(degrees)/sum(degrees)
    plot(degreeNum, degreeFreq,main=paste("Degree Distribution for in-degree for node", node_num),xlab="Degree",ylab="Frequency")
  }
  if (DO_20){
    # Problem 20
    cluster <- cluster_walktrap(g)
    # plot communities using color
    plot(cluster, g, vertex.label = NA, vertex.size = 3, main=paste("2.1 Graph (Walktrap) for node", node_num))
    # modularity score
    print(sprintf("2.1 Modularity (Walktrap) for node %s: %f", node_num, modularity(cluster)))
  }
}
