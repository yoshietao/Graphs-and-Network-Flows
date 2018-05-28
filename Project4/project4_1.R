library(igraph)
library(data.table)

setwd("/Users/evelyn/Documents/master_first_year/third_quarter/ECE232/Random-Graphs-and-Random-Walks/Project4/")
files = c("project4_data/actor_movies.txt", "project4_data/actress_movies.txt")
StartTime <- Sys.time()

# DT = data.table(A=NA, M=NA)
data_list <- list()
i <- 1
for (filename in files){
  print (filename)
  con=file(filename,open="r",encoding='ISO-8859-1')
  lines = readLines(con)
  for (l in lines){
    tokens = unlist(strsplit (l, '\t\t'))
    act = tokens[1]
    if (length(tokens)>=11) # at least 10 movie
    {
      mov = trimws(tokens [2:length(tokens)])
      mov = gsub ("(.+)\\s\\s.*", "\\1", mov)
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

getPairing <- function(nameToLookup, pairDT, idDT) {
  setorder(pairDT, id.x, -wt)
  id <- idDT[name == nameToLookup]$id
  outputId <- pairDT[id.x == id]$id.y[1]
  outputName <- idDT[id == outputId]$name
  outputWt <- pairDT[id.x == id]$wt[1]
  cat("Input: ", nameToLookup, ", Output: ", outputName, ", Weight: ", outputWt, "\n")
}

actorVec <- c("Cruise, Tom", "Watson, Emma (II)", "Clooney, George", "Hanks, Tom", "Johnson, Dwayne (I)",
              "Depp, Johnny", "Smith, Will (I)", "Streep, Meryl", "DiCaprio, Leonardo", "Pitt, Brad")

invisible(sapply(actorVec, getPairing, pairDT, idDT))

print("get pair time:")
Sys.time()-init2

init3 <-Sys.time()

pageRank <- page_rank(g)
saveRDS(pageRank, paste0(inputDir, "pageRank.rds"))

ranking <- rank(-(pageRank$vector))
top10Id <- match(c(1:10), ranking)
top10DT <- idDT[top10Id]
top10DT <- merge(top10DT, dataDT[, .(numMovies = .N), by = .(id)], by = "id", sort = F)
top10DT <- merge(top10DT, pairDT[, .(inDegree = .N), by = id.y], by.x = "id", by.y = "id.y", sort = F)

print("page rank time: ")
Sys.time()-init3

top10DT
