get_DID <- function(mapping_atc=TRUE,mapping_icd10=TRUE,api_key=""){
  url="https://static-content.springer.com/esm/art%3A10.1186%2Fs13326-016-0110-0/MediaObjects/13326_2016_110_MOESM1_ESM.xlsx"
  DID <- read.xlsx(url, sheet=3, colNames=F)
  colnames(DID) = gsub("[[:space:]]","_",gsub("NA.","",paste(c("NA","NA","NA",DID[1,4:length(DID)]),DID[2,],DID[3,],sep=".")))
  
  if(mapping_icd10 & api_key!=""){
    cui_icd10 <- mapping_icd10_cui(api_key=api_key)
    DID <- merge(DID,cui_icd10,by.x="indication.UMLS_phenotype.CUI" ,by.y="cui",all.x=T)
  }
  
  if(mapping_atc & api_key!=""){
    atc_cui <- mapping_atc_cui(unique(DID$drug.UMLS.CUI),api_key=api_key)
    DID <- merge(DID,atc_cui,by.x="drug.UMLS.CUI",by.y="cui",all.x=T)
  }
  
  return(DID)
  
}