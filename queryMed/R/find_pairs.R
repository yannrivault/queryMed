#' Find pairs of relation
#'
#'Internal function for the find_relations function call
#'
#' @importFrom dplyr filter_at vars all_vars
#'
#' @param items database where to search for relations.
#' @param target Knowledge database (e.g. DIKB, DID, NDF-RT, etc)
#' @param target_elements Rows representing the relations.
#'
#' @details No meant to be used by itself
#' 
#' @export
#'
#' @author Y. Rivault
#' @note internal function
#' @seealso [find_relations()]

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