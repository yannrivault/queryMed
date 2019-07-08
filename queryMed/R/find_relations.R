#' Find relation between medical terms 
#' 
#' Function to find intersection between medical terms (disease, drugs...) and pairs defining a biomedical relationship like interactions, contraindications and indications listed in Knowledge database.
#' 
#' @importFrom dplyr filter_at vars all_vars
#' @importFrom tidyr gather 
#' @importFrom plyr dlply
#' 
#' @param data.x Data frame with one column of identifier and one column of medical terms
#' @param data_indices column names of data.x that contains identifiers.
#' @param data_elements.x column names of data.x that contains medical term of interest.
#' @param data.y  Data frame with one column of identifier and one column of medical terms
#' @param data_elements.y column names of data.y that contains medical term of interest.
#' @param target Knowledge database (e.g. DIKB, DID, NDF-RT, etc)
#' @param target_elements Column names where to look for relations (in pairs).
#' @param progress Whether or not to display progress bar. Default is set to "none"
#' 
#' @references Ayvaz et al (2015) Toward complete dataset of drug-drug interaction information from publicly available sources. Journal of Biomedical Informatics, 55: 206-217.
#' @references Sharp ME (2017). Toward a comprehensive drug ontology: extraction of drug-indication relations from diverse information sources. Journal of Biomedical Semantics,8:2.
#' @author Y. Rivault
#' @seealso [DIKB] dataset and [DID] dataset
#' 
#' @export 
#' 
#' @examples 
#' # Do patients have drug interaction
#' data(drug_set)
#' data(DIKB)
#' interactions <- find_relations(data.x=drug_set,   data_indices = "patient",  
#' data_elements.x = "ATC", target=DIKB, target_elements = c("atc1","atc2"),  progress="none")
#'  interactions[[1]]

find_relations <- function(data.x,data_indices,data.y=NULL,data_elements.x=NULL,data_elements.y=NULL,target,target_elements,progress="none"){
  
  options(warn=2)
  
  if(!all(target_elements %in% colnames(target))){
    warning("target_elements must be column name(s) from target")
  }
  
  if(!is.null(data.y)){
    
    if(!(data_indices %in% colnames(data.x)) & (data_indices %in% colnames(data.y))){
      warning("data_indices must be a column name from both data.x and data.y")
    }
    
    if(!(data_elements.x %in% colnames(data.x))){
      warning("data_elements.x must be a column name from data.x")
    }
    
    if(!(data_elements.y %in% colnames(data.y))){
      warning("data_elements.y must be a column name from data.y")
    }
    
    data_vocab = unique(c(as.vector(unlist(t(data.x[,data_elements.x]))),as.vector(unlist(t(data.y[,data_elements.y])))))
    target_vocab = unique(as.vector(unlist(t(target[,target_elements]))))
    vocab = intersect(data_vocab,target_vocab)
    
    data.x <- data.x[,c(data_indices,data_elements.x)]
    data.y <- data.y[,c(data_indices,data_elements.y)]
    
    data.x <- gather(data.x,key=data_indices,value="elements",data_elements.x)
    data.y <- gather(data.y,key=data_indices,value="elements",data_elements.y)
    
    data <- rbind(data.x,data.y)
    data <- data[(data[,"elements"]%in%vocab),]
  }
  
  else{
    
    if(!(data_indices %in% colnames(data.x))){
      warning("data_indices must be a column name from data.x")
    }
    
    if(!(data_elements.x %in% colnames(data.x))){
      warning("data_elements.x must be a column name from data.x")
    }
    
    data_vocab = as.vector(unlist(t(data.x[,data_elements.x])))
    target_vocab = as.vector(unlist(t(target[,target_elements])))
    vocab = intersect(data_vocab,target_vocab)
    
    data <- data.x[,c(data_indices,data_elements.x)]
    
    data <- gather(data,key=data_indices,value="elements",data_elements.x)
    data <- data[(data[,"elements"]%in%vocab),]
  }
  
  target <- filter_at(target, vars(target_elements),all_vars(. %in% vocab))
  
  
temp=dlply(data, data_indices, function(x) find_pairs(x[,"elements"],target=target,target_elements=target_elements),.progress=progress)
return(temp)
  
options(warn=1)
  
}
