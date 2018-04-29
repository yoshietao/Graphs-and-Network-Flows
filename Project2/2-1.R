library('igraph')
library('Matrix')
library('pracma')
library('data.table')

# create directed personal networks for users who have more than 2 circles
#file = 'gplus/101541879642294398860.circles'
#d <- read.table(file, sep = " ")
#dim(d)[1] # number of circles in the file 

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

