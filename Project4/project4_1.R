library(igraph)
library(data.table)

setwd("/Users/evelyn/Documents/master_first_year/third_quarter/ECE232/Random-Graphs-and-Random-Walks/Project4/")
# fileName="project4_data/actor_movies.txt"
# files = c("test2.txt", "test3.txt")
files = c("project4_data/actor_movies.txt", "project4_data/actress_movies.txt")
#con=file(fileName,open="r")
#lines = readLines(con)

StartTime <- Sys.time()

# DT = data.table(A=NA, M=NA)
data_list <- list()
i <- 1
for (filename in files){
  print (filename)
  con=file(filename,open="r")
  lines = readLines(con)
  for (l in lines){
    tokens = unlist(strsplit (l, '\t\t'))
    act = tokens[1]
    if (length(tokens)>=11) # at least 10 movie
    {
      mov = trimws(tokens [2:length(tokens)])
      mov = gsub ("(.+)\\s\\s.*", "\\1", mov)
      #DT_tmp = data.table(A=act, M=mov)
      #DT <- rbind(DT, DT_tmp)
      # datalist[[i]] <- data.table(name=, movie=)
      data_list[[i]] <- data.table(A=act, M=mov)
      i <- i+1 
    }
  }
  close (con)
}

DT <-rbindlist(data_list)
# get rid of NA row 
# DT = na.omit(DT)
print (paste0("unique number of actor/actress: ", length (unique(DT$A))))
print (paste0("unique number of movies: ", length (unique(DT$M))))

# cleaned data in DT now 
saveRDS(DT, file = "my_data.rds")

EndTime <-Sys.time()

EndTime-StartTime

DT <- readRDS(file = "my_data.rds")
DT[,id:=match(A,unique(A))]
id_DT <- unique (DT[,.(A,id), by = c("A","id")])

DT[,A:=NULL]
pair_DT = merge(DT, DT, by="M", allow.cartesian = TRUE)
pair_DT[,M:=NULL]
pair_DT <- pair_DT[,.(count = .N), by = .(id.x, id.y)]
pair_DT[,weight := count / max(count), by = .(id.x)] 
pair_DT <- pair_DT [id.x != id.y]

saveRDS(pair_DT, file = "pair_DT_data.rds")

g <- graph_from_edgelist(as.matrix(pair_DT[,.(id.x,id.y)]))
plot(degree.distribution(g, mode="in"), main=paste("Degree Distribution for in-degree for graph"), xlab="degree", ylab="frequency", cex = 0.7, cex.axis=0.8, cex.main=0.7)

