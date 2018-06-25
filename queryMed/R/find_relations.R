find_relations <- function(data,data_indices,data_elements,target,target_elements){
  
  data_vocab = as.vector(unlist(t(data[,data_elements])))
  target_vocab = as.vector(unlist(t(target[,target_elements])))
  vocab = intersect(data_vocab,target_vocab)
  
  data <- data[,c(data_indices,data_elements)]
  
  data <- gather(data,key=data_element_attribute,value=elements,data_elements)
  data <- data[(data[,"elements"]%in%vocab),]
  
  target <- target[as.character(target[,target_elements[1]])%in%vocab & as.character(target[,target_elements[2]])%in%vocab, ]
  
  
  temp=dlply(data, data_indices, function(x) find_pairs(x[,"elements"],target=target,target_elements=target_elements),.progress="text")
  return(temp)
  
}