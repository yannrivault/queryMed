#' Retrieve Drug Indication Database (DID) data
#' 
#' Function that load Drug Indication Database (DID) from source.
#' 
#' @param mapping_atc By default the argument is FALSE. When TRUE an additional mapping to ATC code is performed but a valid API Key is required to connect to and query BIOPORTAL services.
#' @param mapping_icd10 By default the argument is FALSE. When TRUE an additional mapping to ICD10 code is performed but a valid API Key is required to connect to and query BIOPORTAL services.
#' @param api_key Optional argument. A valid API Key is required to access any API call on Bioportal. To have one, you need to register at http://bioportal.bioontology.org/.
#'
#' @importFrom openxlsx read.xlsx
#'
#' @details See https://figshare.com/articles/Additional_file_1_of_Toward_a_comprehensive_drug_ontology_extraction_of_drug-indication_relations_from_diverse_information_sources/4535021 for details 
#' 
#' @return  Data frame with all Drug Indication Database variables and possible mapping to ATC and/or ICD10 if arguments are set to TRUE and a valid API Key is provided.
#' 
#' @references Sharp ME (2017). Toward a comprehensive drug ontology: extraction of drug-indication relations from diverse information sources. Journal of Biomedical Semantics,8:2.
#' 
#' @export
#' 
#' @author Y. Rivault
#' @seealso [get_DIKB()] and [mapping_cui()]
#' @examples
#'   DID <- get_DID(mapping_atc=FALSE,mapping_icd10=FALSE)
#'   head(DID)
#'   dim(DID)
#'   colnames(DID)
#' 
#'   # If you want only indications from ATC to ICD10 codes :
#'   atc_icd10_DID <- DID[!(is.na(DID$atc) | is.na(DID$icd10)),]

# I don't know yet if it is preferable to load the database in the package, instead of having a function that load it.

get_DID <- function(mapping_atc=FALSE,mapping_icd10=FALSE,api_key=""){
  
  if(mapping_icd10==TRUE & api_key=="" | mapping_atc==TRUE & api_key==""){
    warning("A valid api_key value is required to perform the mapping")
    return(NULL)
  }  
  
  url="https://static-content.springer.com/esm/art%3A10.1186%2Fs13326-016-0110-0/MediaObjects/13326_2016_110_MOESM1_ESM.xlsx"
  DID <- read.xlsx(url, sheet=3, colNames=F)
  colnames(DID) = gsub("[[:space:]]","_",gsub("NA.","",paste(c("NA","NA","NA",DID[1,4:length(DID)]),DID[2,],DID[3,],sep=".")))
  DID <- DID[4:dim(DID)[1],]
  
  
  if(mapping_icd10 & api_key!=""){
    cui_icd10 <- codes2cui(codes=DID$indication.UMLS_phenotype.CUI,ontologies=c("ICD10","ICD10CM"),api_key=api_key)
    DID <- merge(DID,cui_icd10,by.x="indication.UMLS_phenotype.CUI" ,by.y="cui",all.x=T)
    colnames(DID)[colnames(DID)=="classe"]="icd10"
  }
  
  if(mapping_atc & api_key!=""){
    atc_cui <- codes2cui(codes=DID$drug.UMLS.CUI,ontologies="ATC",api_key=api_key)
    DID <- merge(DID,atc_cui,by.x="drug.UMLS.CUI",by.y="cui",all.x=T)
    colnames(DID)[colnames(DID)=="classe"]="atc"
  }
  
  return(DID)
  
}
