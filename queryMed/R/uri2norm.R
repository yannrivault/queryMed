# NLM: uri2norm optimized
uri2norm <- function(X){
        X <- as.character(X)
        X[X %in% c(NA,"")] <- NA
        X <- strsplit(X,"/")
        X <- sapply(X, function(x) x[4])
        X <- gsub("^.*#","",X)
        X <- gsub("^.*:","",X)
        X <- gsub(">","",X)
        X <- gsub("(^\\s*|\\s*$)","",X, perl=T)
    return(X)
  }