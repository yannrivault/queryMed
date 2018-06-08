check_interaction<- function(drugs=""){
  if(length(drugs)<2){
    return("A vector of at least 2 drugs is requiered")
  }
  data(DIKB)
  dikb2 = DIKB[as.character(DIKB$atc1)%in%drugs & as.character(DIKB$atc2)%in%drugs, ]
  res = dikb2[apply(dikb2[,c("atc1", "atc2")], 1, function(x){ length(intersect(drugs, x)) }) ==2, ]
  
  if(nrow(res)==0){
    return("No known interaction in DIKB")
  }
  if(nrow(res)>0){  
    return(res)
  }
}