uri2norm <- function(X){

  if(!is.null(dim(X))){
    X <- as.matrix(X)
    class(X) <- "character"
    uris <- apply(X,FUN=function(x) str_detect(x,"http"), MARGIN=2)

    X[which(uris)] <- gsub(".*/|>", "", X[which(uris)])
    X[which(uris)] <- gsub("(^\\s*|\\s*$)","",X[which(uris)], perl=T)
    return(as.data.frame(X))
    }
  else {
    X <- gsub(".*/|>", "", X)
    X <- gsub("(^\\s*|\\s*$)","",X, perl=T)
    return(X)
   }
}
