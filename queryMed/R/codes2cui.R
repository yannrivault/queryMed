#' Mapping medical nomenclatures to CUI
#' 
#' The function allows mapping from concept unique identifier (cui - UMLS) to medical nomenclatures available through \href{http://bioportal.bioontology.org/}{BioPortal}, and vice versa.
#' 
#' @param codes Codes the user want to map. It can be cui's codes or any terminology codes available on Bioportal.
#' @param ontologies A set of ontologies where to find mappings if codes are cui. Or a set of ontologies which codes belongs if they are not cui.
#' @param api_key An API Key is required to access any API call on BioPortal and SIFR. To have one, you need to register at \href{http://bioportal.bioontology.org/}{BioPortal}, respectively to \href{http://bioportal.lirmm.fr/}{SIFR BioPortal}.
#' 
#' @return data.frame of mapped codes
#' 
#' @author  Y. Rivault
#' 
#' @export           
#' @examples
#' \dontrun{
#'  mcui <-  codes2cui(codes="B01AA01",ontologies="ATC",api_key="youAPIKey")
#'  mcui
#'  }

codes2cui <- function(codes=NULL, ontologies="", api_key=""){
  options(warn=2)
  
  codes <- unique(codes)
  
  step=800
  
  n <- length(codes) %/% step
  rest <- length(codes) %% step
  results <- data.frame()
  
  cat("Querying Bioportal search REST API to retrieving mapping\n")
  
  if (n>0){
    for(i in 1:n){
      S=suppressMessages(search(term = codes[((i-1)*step):(i*step-1)], ontologies = ontologies, service = "bioportal", api_key = api_key, extra_args = "&display_context=false&display_links=false"))
      if (!is.null(S)){
        for(j in 1:length(S$collection)){
          if((uri2norm(S$collection[[j]]$'@id') %in% codes) | (length(intersect(unlist(S$collection[[j]]$cui),codes))>0)){
            for(k in 1:length(S$collection[[j]]$cui)){
              results[dim(results)[1]+1,"cui"]=S$collection[[j]]$cui[[k]]
              results[dim(results)[1],"classe"]=S$collection[[j]]$"@id"
            }
          }
        }
      }
    }
    
  }
  if (rest>0){
    S=suppressMessages(search(term=paste(codes[(n*step):(n*step+rest)],collapse="+"), ontologies = ontologies, service = "bioportal", api_key = api_key, extra_args = "&display_context=false&display_links=false"))
    if (!is.null(S)){
      for(j in 1:length(S$collection)){
        if((uri2norm(S$collection[[j]]$'@id') %in% codes) | (length(intersect(unlist(S$collection[[j]]$cui),codes))>0)){
          for(k in 1:length(S$collection[[j]]$cui)){
            results[dim(results)[1]+1,"cui"]=S$collection[[j]]$cui[[k]]
            results[dim(results)[1],"classe"]=S$collection[[j]]$"@id"
          }
        }
      }
    }
  }
  
  options(warn=1)
  
  return(uri2norm(results))
}
