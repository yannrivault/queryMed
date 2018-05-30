uri2norm <- 
function(X){
  if(!is.null(dim(X))){
    X <- as.matrix(X)
    class(X) <- "character"}
  
  uris <- apply(X,FUN=function(x) str_detect(x,"http"),MARGIN=2)
  X[uris] <- gsub(".*/|>", "", X[uris])
  X[uris] <- gsub("(^\\s*|\\s*$)","",X[uris], perl=T)
  
  if(!is.null(dim(X))) return(as.data.frame(X))
  else return(X)
}