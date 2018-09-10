#' Retrieve Drug Interaction Knowledge Base (DIKB) data
#'
#' This function downloads the Drug Interaction Knowledge Base (DIKB) from source.
#'
#' @param path filepath where to download the Drug Interaction Knowledge Base. By default it is the current working directory.
#' @param url URL of the location of the Drug Interaction Knowledge Base (DIKB). By default it is <https://dbmi-icode-01.dbmi.pitt.edu/dikb-evidence/pddi-sets/> but the argument exists in case the url happens to change.
#' @param mapping Optional. Possible argument is ATC and the mapping is performed between DIKB and ATC information from  <http://bio2rdf.org and https://dbpedia.org/>
#'
#' @importFrom httr set_config config write_disk
#'
#' @details The function download the less conservative version of all PDDI datasets merged by Richard D. Boyce's group at University of Pittsburgh. See <https://dbmi-icode-01.dbmi.pitt.edu/dikb-evidence/pddi-sets/> for details.
#' @return Data frame with all variables available from Drug Interaction Knowledge Base (DIKB).
#' 
#' @references Ayvaz et al (2015) Toward complete dataset of drug-drug interaction information from publicly available sources. Journal of Biomedical Informatics, 55: 206-217.
#' @export
#' 
#' @author Y. Rivault
#' @seealso [get_DIKB()] and [mapping_atc_db()]
#' @examples
#' \dontrun{
#' DIKB <- get_DIKB(path="/tmp",mapping="ATC")
#' head(DIKB)
#' colnames(DIKB)
#' dim(DIKB)
#' }

get_DIKB <- function(path=getwd(),url=NULL,mapping=NULL){
  # In case of replaced url, user can specify the new url
  if(is.null(url)) url="https://dbmi-icode-01.dbmi.pitt.edu/dikb-evidence/pddi-sets/CombinedDatasetNotConservative.csv.bz2"
  set_config(config(ssl_verifypeer = 0L))
  get.DIKB <- GET(url,write_disk(paste(path,"DIKB.csv.bz2",sep="/"),overwrite=T))
  
  # Issue with a byte order marker (BOM) that unforunately cannot be handled by "UTF-8-BOM" option in read.csv function
  # First the colnames are loaded
  # Then the database, except the first line
  # And the first row, containing the BOM issue, is writted manualy and added as the last row of the database
  
  colnames <- utils::read.csv(file.path(path,"DIKB.csv.bz2"),sep="\t",header=T,fileEncoding = "UTF-8",nrows=1)
  DIKB <- utils::read.csv(file.path(path,"DIKB.csv.bz2"),sep="\t",header=F,fileEncoding = "UTF-8",skip=2,na.strings=c("","None","unclassified"))
  first_row <- c("http://bio2rdf.org/drugbank:DB00073","Rituximab","http://bio2rdf.org/drugbank:DB00519","Trandolapril",rep(NA,16),"Drugbank",rep(NA,6))
  DIKB[dim(DIKB)[1]+1,]=first_row
  colnames(DIKB)=colnames(colnames)
  
  # So it works. Obviously this is not realy stable ...
  
  # keeping the db drugs only :
  DIKB <- DIKB[grepl("http://bio2rdf.org/drugbank:DB",DIKB$drug1) & grepl("http://bio2rdf.org/drugbank:DB",DIKB$drug2),]
  DIKB$drug1=uri2norm(DIKB$drug1)
  DIKB$drug2=uri2norm(DIKB$drug2)
  
  if(!is.null(mapping) && mapping=="ATC"){
    atc_db <- mapping_atc_db()
    DIKB=merge(DIKB,atc_db,by.x="drug1",by.y="db",all.x=T)
    DIKB=merge(DIKB,atc_db,by.x="drug2",by.y="db",all.x=T)
    colnames(DIKB)[28:29]=c("atc1","atc2")
  }
  
  DIKB[,28]<- factor(DIKB[,28])
  DIKB[,29]<- factor(DIKB[,29])
  
  
  return(subset(DIKB,select=-c("dateAnnotated","ddiPkEffect","homepage","numericVal","objectUri","pathway","precipUri","whoAnnotated","ddiType","evidence","evidenceSource","researchStatementLabel","researchStatement","certainty")))
}
