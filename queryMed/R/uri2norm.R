uri2norm <- 
function(X){
  if(!is.null(dim(X))){
    X <- as.matrix(X)
    class(X) <- "character"}
  
  uris <- str_detect(X,"http")
  X[uris] <- gsub(".*/|>", "", X[uris])
  X[uris] <- gsub("(^\\s*|\\s*$)","",X[uris], perl=T)
  
  if(!is.null(dim(X))) return(as.data.frame(X))
  else return(X)
}