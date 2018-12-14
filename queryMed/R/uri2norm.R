#' Conversion of URI to variable
#' 
#' Fonction that turn URI(s) (address type syntax) into computable variable
#' 
#' @param X One URI, a vector composed of URIs, a matrix of URIs or a dataframe of URIs.
#' 
#' @importFrom dplyr as_tibble
#' @importFrom stringr str_detect
#' @return Character vector
#' 
#' @export
#' 
#' @author  Y. Rivault
#' @examples
#'  uri2norm("http://purl.bioontology.org/ontology/UATC/A05A")
#'  uri2norm(c("http://purl.bioontology.org/ontology/UATC/A05A",
#'  "http://purl.bioontology.org/ontology/UATC/A05A01"))

uri2norm <- function(X){
  
  class.X <- class(X)
  
  if(!is.null(dim(X))){
    X <- as.matrix(X)
    class(X) <- "character"
    uris <- apply(X,FUN=function(x) str_detect(x,"http"), MARGIN=2)

    X[which(uris)] <- gsub(".*[#/]|>", "", X[which(uris)])
    X[which(uris)] <- gsub("(^\\s*|\\s*$)","",X[which(uris)], perl=T)
    X[which(uris)] <- gsub("[A-z0-9]*[:]","",X[which(uris)])
    
    if("tbl" %in% class.X){return(as_tibble(X))}
    else{return(as.data.frame(X))}
    }
  else {
    X <- llply(X,function(x){gsub(".*[#/]|>", "", x)})
    X <- llply(X,function(x){gsub("(^\\s*|\\s*$)", "", x)})
    X <- llply(X,function(x){gsub("[A-z0-9]*[:]", "", x)})
    return(X)
   }
}
