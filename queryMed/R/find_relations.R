find_relations <- function(data.x,data_indices,data.y=NULL,data_elements.x=NULL,data_elements.y=NULL,target,target_elements){
  
  if(!is.null(data.y)){
    data_vocab = unique(c(as.vector(unlist(t(data.x[,data_elements.x]))),as.vector(unlist(t(data.y[,data_elements.y])))))
    target_vocab = unique(as.vector(unlist(t(target[,target_elements]))))
    vocab = intersect(data_vocab,target_vocab)
    
    data.x <- data.x[,c(data_indices,data_elements.x)]
    data.y <- data.y[,c(data_indices,data_elements.y)]
    
    data.x <- gather(data.x,key=data_element_attribute,value=elements,data_elements.x)
    data.y <- gather(data.y,key=data_element_attribute,value=elements,data_elements.y)
    
    data <- rbind(data.x,data.y)
    data <- data[(data[,"elements"]%in%vocab),]
  }
  
  else{
    data_vocab = as.vector(unlist(t(data.x[,data_elements.x])))
    target_vocab = as.vector(unlist(t(target[,target_elements])))
    vocab = intersect(data_vocab,target_vocab)
    
    data <- data.x[,c(data_indices,data_elements.x)]
    
    data <- gather(data,key=data_element_attribute,value=elements,data_elements.x)
    data <- data[(data[,"elements"]%in%vocab),]
  }
  
  target <- target[as.character(target[,target_elements[1]])%in%vocab & as.character(target[,target_elements[2]])%in%vocab, ]
  
  
  temp=dlply(data, data_indices, function(x) find_pairs(x[,"elements"],target=target,target_elements=target_elements),.progress="text")
  return(temp)
  
}