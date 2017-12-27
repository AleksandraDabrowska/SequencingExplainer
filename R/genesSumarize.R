#'@description Function genesSumarize counts the coverage of each gene
#'
#'@title genesSumarize
#'
#'
#'
#'@importFrom dplyr filter
#'@importFrom tidyr separate
#'
#'@export

genesSummarize <- function(bamDataFrame, geneData=TAIR10_genes, chromosome=NULL, range=0){
  if(exists("chromosome")){
    geneData <- filter(geneData, V1=="Chr1")
  }
  geneData <- filter(geneData, V3=="gene")
  geneData <- separate(geneData, col=V9, into=c("id","note","name"), sep="\\;")
  geneData$id <- substr(geneData$id, 4, length(geneData$id))
  counts <- matchToGene(positions = bamDataFrame$pos, start=geneData$V4, stop=geneData$V5, name=geneData$id, range = range)
  uniqueCounts <- matchToGene(positions = unique(bamDataFrame$pos),start=geneData$V4, stop=geneData$V5, name=geneData$id, range = range)
  genesSummary <- data.frame(name = geneData$id, counts = counts$count, uniqueCounts = uniqueCounts$count, length = (geneData$V5 - geneData$V4))

  return(genesSummary)
}
