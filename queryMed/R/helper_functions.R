#' Test validity of ATC or ICD10 code
#'
#' `is.atc` verify that the given character vector is an ATC code
#'
#' @param x Character vector to test as ATC code.
#' 
#' @return logical vector that indicates which elements are ATC code
#' 
#' @export
#' 
#' @examples
#' is.atc(c("C10AB01", "N07BB01"))
#' is.atc(c("C10AB01", "N07"))

is.atc <-function(x){
  res <- grep("^[A-D,G,H,J,L,M,N,P,R,V][0-9][0-9][A-Z][A-Z][0-9][0-9]$", x, value=TRUE)
  return(x%in%res)
}
#' @rdname is.atc
#' 
is.icd10 <- function(x){
  x1<-which(length(x)>3)
  x1 <- x[x1]
  if(length(x1)>0)  {
    res1 <- grep("^[A-Z][0-9][0-9]\\.[0-9]$", x1, value=TRUE)
  }
  x2<-which(length(x)==2)
  x2 <- x[x2]
  if(length(x2)>0)  {
    res2 <- grep("^[A-Z][0-9][0-9]", x2, value=TRUE)
  }
  
    return(x%in%c(res1, res2))
}
#' @rdname is.icd10