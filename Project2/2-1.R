library('igraph')
library('Matrix')
library('pracma')
library('data.table')

# Problem 18 
# create directed personal networks for users who have more than 2 circles
path = "gplus/"
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

# Problem 19
# Create Directed Graphs from the nodeId.edges files 
# file = "test.txt"
node_list <- c("109327480479767108490", 
               "115625564993990145546",
               "101373961279443806744")
extension <- ".edges"
for (node_num in node_list){
  graph_list = c() 
  d <- read.table(paste0(path,node_num,extension), sep = " ")
  g <- graph_from_data_frame(d,directed=TRUE)
  plot(g, vertex.size=4, vertex.label=NA)
  
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

