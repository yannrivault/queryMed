get_DIKB <-
function(path=getwd(),url=NULL,mapping=NULL){
  # In case of replaced url, user can specify the new url
  if(is.null(url)) url="https://dbmi-icode-01.dbmi.pitt.edu/dikb-evidence/pddi-sets/CombinedDatasetNotConservative.csv.bz2"
  set_config(config(ssl_verifypeer = 0L))
  get.DIKB <- GET(url,write_disk(paste(path,"DIKB.csv.bz2",sep="/"),overwrite=T))
  
  # Issue with a byte order marker (BOM) that unforunately cannot be handled by "UTF-8-BOM" option in read.csv function
  # First the colnames are loaded
  # Then the database, except the first line
  # And the first row, containing the BOM issue, is writted manualy and added as the last row of the database
  
  colnames=read.csv(file.path(path,"DIKB.csv.bz2"),sep="\t",header=T,fileEncoding = "UTF-8",nrows=1)
  DIKB <- read.csv(file.path(path,"DIKB.csv.bz2"),sep="\t",header=F,fileEncoding = "UTF-8",skip=2)
  first_row <- c("http://bio2rdf.org/drugbank:DB00073","Rituximab","http://bio2rdf.org/drugbank:DB00519","Trandolapril",rep("None",16),"Drugbank",rep("None",6))
  DIKB[dim(DIKB)[1]+1,]=first_row
  colnames(DIKB)=colnames(colnames)
  
  # So it works. Obviously this is not realy stable ...
  
  # keeping the db drugs only :
  DIKB <- DIKB[grepl("http://bio2rdf.org/drugbank:DB",DIKB$drug1) & grepl("http://bio2rdf.org/drugbank:DB",DIKB$drug2),]

  if(mapping=="ATC"){
    atc_db <- mapping_atc_db()
    DIKB=merge(DIKB,atc_db,by.x="drug1",by.y="db",all.x=T)
    DIKB=merge(DIKB,atc_db,by.x="drug2",by.y="db",all.x=T)
    colnames(DIKB)[28:29]=c("atc1","atc2")
  }
  
  DIKB$object<- toupper(DIKB$object)
  DIKB$precipitant <- toupper(DIKB$precipitant)
  DIKB$drug1<- substr(DIKB$drug1, 29, 35)
  DIKB$drug2<- substr(DIKB$drug1, 29, 35)
  
  #FrenchDB - severity PC -AD??

  return(DIKB)
}
