uri2norm <- 
function(X){
  X <- as.matrix(X)
  class(X) <- "character"
  X <- sub(".*/", "", X)
  X <- sub(">", "", X)
  X <- sub("(^\\s*|\\s*$)","",X, perl=T)
  return(as.data.frame(X))
}