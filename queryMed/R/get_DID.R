# I don't know yet if it is preferable to load the database in the package, instead of having a function that load it.

get_DID <- function(mapping_atc=TRUE,mapping_icd10=TRUE,api_key=""){
  url="https://static-content.springer.com/esm/art%3A10.1186%2Fs13326-016-0110-0/MediaObjects/13326_2016_110_MOESM1_ESM.xlsx"
  DID <- read.xlsx(url, sheet=3, colNames=F)
  colnames(DID) = gsub("[[:space:]]","_",gsub("NA.","",paste(c("NA","NA","NA",DID[1,4:length(DID)]),DID[2,],DID[3,],sep=".")))
  DID <- DID[4:dim(DID)[1],]
  
  
  if(mapping_icd10 & api_key!=""){
    cui_icd10 <- mapping_cui(codes=DID$indication.UMLS_phenotype.CUI,ontologies=c("ICD10","ICD10CM"),api_key=api_key,progress=T)
    DID <- merge(DID,cui_icd10,by.x="indication.UMLS_phenotype.CUI" ,by.y="cui",all.x=T)
    colnames(DID)[colnames(DID)=="classe"]="icd10"
  }
  
  if(mapping_atc & api_key!=""){
    atc_cui <- mapping_cui(codes=DID$drug.UMLS.CUI,ontologies="ATC",api_key=api_key,progress=T)
    DID <- merge(DID,atc_cui,by.x="drug.UMLS.CUI",by.y="cui",all.x=T)
    colnames(DID)[colnames(DID)=="classe"]="atc"
  }
  
  return(DID)
  
}
