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
