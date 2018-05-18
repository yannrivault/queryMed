uri2norm <- 
function(X){
  if(!is.null(dim(X))){
    X <- as.matrix(X)
    class(X) <- "character"}
  
  X <- gsub(".*/|>", "", X)
  X <- gsub("(^\\s*|\\s*$)","",X, perl=T)
  
  if(!is.null(dim(X))) return(as.data.frame(X))
  else return(X)
}