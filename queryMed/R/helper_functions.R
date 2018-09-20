#' Test validity of ATC or ICD10 code
#'
#' `is.atc` verify that the given character vector is an ATC code
#' `is.icd10` verify that the given character vector is an ICD10 code
#' `icd10decimal` convert given character vector of plain ICD10 codes (ex S743) to decimal ICD10 codes (ex S74.3)
#'
#' @param x Character vector to test or convert
#' 
#' @return For the is.atc and is.icd10 function call a logical vector is retruened. 
#' It indicates which elements are ATC or ICD10 codes. 
#' For icd10decimal fuction call a Character vector of ICD10 codes with decimal is return.
#' 
#' @export 
#' @examples
#' is.atc(c("C10AB01", "N07BB01"))
#' is.atc(c("C10AB01", "N07"))
#' is.icd10(c("I70.2", "S74.3"))
#' icd10decimal(c("I70.21", "S743")) 

is.atc <-function(x){
  res <- grep("^[A-D,G,H,J,L,M,N,P,R,V][0-9][0-9][A-Z][A-Z][0-9][0-9]$", x, value=TRUE)
  return(x%in%res)
}

#' @rdname is.atc
#' @export is.icd10
is.icd10 <- function(x){
  x1<-which(nchar(x)>3)
  res1 <- c()
  if(length(x1)>0)  {
    x1 <- x[x1]
    res1 <- grep("^[A-Z][0-9][0-9]\\.[0-9]", x1, value=TRUE)
  }
  x2<-which(nchar(x)==3)
  res2 <- c()
  if(length(x2)>0)  {
    x2 <- x[x2]
    res2 <- grep("^[A-Z][0-9][0-9]", x2, value=TRUE)
  }
  
    res <- c(res1, res2)
    return(x%in%res)
}



#' @rdname is.atc
#' @export icd10decimal
icd10decimal<- function(x){
  verif<- is.icd10(x) 
  x[!verif] <- sub("(.{3})(.*)", "\\1.\\2", x[!verif])
  return(x)
}


