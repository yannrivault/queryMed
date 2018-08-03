find_pairs<- function(items,target,target_elements){
  
  items <- items[!is.na(items)]
  
  if(length(items)<2){
    return("No known relations")
  }
  
  target2 <- filter_at(target,vars(target_elements), all_vars(. %in% items))
  
  res = target2[apply(target2[,target_elements], 1, function(x){ length(intersect(items, x)) }) ==2, ]
  
  if(nrow(res)==0){
    return("No known relations")
  }
  if(nrow(res)>0){  
    return(res)
  }
}