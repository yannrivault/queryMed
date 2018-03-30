uri2norm <-
function(X){
  X <- as.matrix(X)
  for(i in 1:length(X)){
    if (X[i] %in% c(NA,"")){}
    else {
      A <- as.character(X[i])
      A <- strsplit(A,"/")
      A <- gsub("^.*#","",A[[1]][length(A[[1]])])
      A <- gsub("^.*:","",A)
      A <- gsub(">","",A)
      X[i] <- gsub("(^\\s*|\\s*$)","",A, perl=T)
    }
  }
  return(X)
}
