uri2norm <- 
function(X){
  X <- as.matrix(X)
  class(X) <- "character"
  X <- gsub(".*/|>", "", X)
  X <- gsub("(^\\s*|\\s*$)","",X, perl=T)
  return(as.data.frame(X))
}