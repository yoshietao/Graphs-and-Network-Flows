rm(list=ls())

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
    tokens = unlist(strsplit (trimws(l), '\t\t'))
    act = tokens[1]
    if (length(tokens)>=11) # at least 10 movie
    {
      mov = trimws(tokens [2:length(tokens)])
      mov=mov[mov!=""]
      #mov = gsub ("(.+)\\s\\s.*", "\\1", mov)
      mov = strsplit(mov,'  ')
      mov = unlist(lapply(mov, `[[`, 1))
      data_list[[i]] <- data.table(name=act, movie=mov)
      i <- i+1 
    }
  }
  close (con)
}

dataDT <-rbindlist(data_list)
# get rid of NA row 
# DT = na.omit(DT)
print (paste0("unique number of actor/actress: ", length (unique(dataDT$name))))
print (paste0("unique number of movies: ", length (unique(dataDT$movie))))

# cleaned data in DT now 
saveRDS(dataDT, file = "my_data.rds")

EndTime <-Sys.time()

EndTime-StartTime

dataDT <- readRDS(file = "my_data.rds")
dataDT[,id:=match(name,unique(name))]

idDT <- unique(dataDT[, .(name, id)], by = c("name", "id"))
cat("Number of actors/actress: ", length(unique(dataDT$name)))
cat("Number of movies: ", length(unique(dataDT$movie)))



init2 <- Sys.time()

dataDT[, name := NULL]

pairDT <- merge(dataDT, dataDT, by = "movie", allow.cartesian = T)
pairDT[, movie := NULL]
pairDT <- pairDT[, .(count = .N), by = .(id.x, id.y)]
pairDT[, wt := count / max(count), by = .(id.x)]
pairDT <- pairDT[id.x != id.y]

saveRDS(pairDT, paste0("wtData.rds"))

g <- graph_from_edgelist(as.matrix(pairDT[, .(id.x, id.y)]))
plot(degree.distribution(g, mode = "in"), main = paste("Degree Distribution for in-degree for graph"), xlab = "Degree", ylab = "Frequency", cex = 0.7, cex.axis = 0.8, cex.main = 0.7)

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
saveRDS(pageRank, paste0("pageRank.rds"))

ranking <- rank(-(pageRank$vector))
top10Id <- match(c(1:10), ranking)
top10DT <- idDT[top10Id]
top10DT <- merge(top10DT, dataDT[, .(numMovies = .N), by = .(id)], by = "id", sort = F)
top10DT <- merge(top10DT, pairDT[, .(inDegree = .N), by = id.y], by.x = "id", by.y = "id.y", sort = F)

# top 10 actor/actress and their page rank scores
cbind(idDT[top10Id], data.table(pagerank = pageRank$vector[top10Id]))

print("page rank time: ")
Sys.time()-init3

# top 10 actor/actress and their num movie and in degree
top10DT
