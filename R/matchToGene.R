#'@description
#'
#'@title matchToGene
#'
#'
#'
#'@importFrom dplyr between
matchToGene <- function(positions, start, stop, name, range=0){
  counts <- data.frame(name, count=0)
  if(range>0){
    start <- start-range
    stop <- stop+range
  }
  for (i in 1:length(start)){
    counts$count[i]<-length((positions[between(positions, start[i], stop[i])]))
  }
  return(counts)
}
