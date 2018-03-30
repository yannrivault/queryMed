require(jsonlite)
require(httr)
require(openxlsx)
require(RCurl)
require(hash)
require(stringr)

# NB: query execution time that is send to a SPARQL endpoint can be longer for the first time
# If you want to have "real" execution time, launch it once again

#####################################################
#### fonction that turn URI(s) in exploitable data: ####
  # Arguments : 1 URI, a vector composed of URIs, a URI(s) matrix, a URI(s) dataframe.
  # Returns a matrix of exploitable string.

uri2norm <- function(X){
  X <- as.matrix(X)
  for(i in 1:length(X)){
    if (X[i] %in% c(NA,"")){}
    else {
      A <- as.character(X[i])
      A <- strsplit(A,"/")
      A <- gsub("^.*#","",A[[1]][length(A[[1]])])
      A <- gsub("^.*:","",A)
      A <- gsub(">","",A)
      X[i] <- gsub("(^\\s*|\\s*$)","",A, perl=T)
    }
  }
  return(X)
}

uri2norm("http://purl.bioontology.org/ontology/UATC/A05A")
uri2norm(c("http://purl.bioontology.org/ontology/UATC/A05A","http://purl.bioontology.org/ontology/UATC/A05A01"))

##################################################################
#### Fonction that allow to send queries on sparql endpoints: ####
  # Alternative to initial SPARQL() fonction in SPARQL package.
  # SPARQL() does not allow to extract JSON data, but only XML, TSV or CSV.
  # JSON is much more ligther than XML.
  # So this function inspired from SPARQL() aims to reduce its execution time.
  # Arguments : a SPARQL query, an URL to a SPARQL endpoint and a key API if need be
  # Returns the query results as a dataframe

sparql <- function(query="",url="",api_key=""){
  
  encoded_query=URLencode(gsub("[[:space:]]+", " ", query),reserved = TRUE)
  
  url=paste(url,"?query=",encoded_query,sep="")
  
  if(api_key!=""){
    url=paste(url,"&apikey=",api_key,sep="")
  }
  
  json_doc=getURL(url,httpheader=c(Accept = "application/sparql-results+json"))
  document <- fromJSON(json_doc)
  
  if(length(document$results$bindings)!=0){
  results=data.frame(matrix(nrow=dim(document$results$bindings)[1],ncol=dim(document$results$bindings)[2]))
  colnames(results)=names(document$results$bindings)
  for(i in colnames(results)){
    results[,i]=document[["results"]][["bindings"]][[i]][["value"]]
  }
  }
  else{results=NULL}
  
  return(results)
}

# Tests

query="PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
      SELECT DISTINCT ?x ?label
      WHERE  {?x skos:prefLabel ?label .}
      limit 10"

sparql(query=query,url="bio2rdf.org/sparql/") #test of null result
sparql(query=query,url="https://dbpedia.org/sparql")


############################################################################################
#### Fonction that send a "query" on bioportal search services on bioportal and SIFR : ####
  # Alternative to sparql endpoint querying
  # For example, the ATC is not avaiblable on the bioportal sparql endpoint, but it is on the search service
  # Nevertheless, the information is poorer and execution time for retriving an equivalent information can be longer
  # Arguments : terms (vector/list), an optional filter by cui (vector/list), the search service (bioportal or sifr), api key if need be, and optional argument(s)
  # Return the result as a JSON document

search_endpoint <- function(term="",cui="",ontologies="",service="bioportal",api_key="",extra_args=""){
  
  if(service=="bioportal"){service <- "http://data.bioontology.org/search?"}
  else if (service=="sifr"){service <- "http://data.bioportal.lirmm.fr/search?"}
  else {
    warning("Wrong search service given")
  }
  
  if(sum(ontologies=="")==0){
    ontologies=paste("&ontologies=",paste(ontologies,collapse=","),sep="")
  }
  
  if (cui=="" && term==""){
    warning("Give at least one term or CUI")
    return(NULL)
  }
  
  if (cui!="" || term!=""){
    term=paste("&q=",paste(term,collapse="+"),"",sep="")
    cui=paste("&cui={",paste(cui,collapse=","),"}",sep="")
  }
  
  url <- paste(service,term,cui,ontologies,extra_args,sep="")
  results<-GET(url,add_headers(Authorization= paste("apikey token=",api_key,sep="")))
  content <- content(results)
  
  if ("error" %in% names(content)){
    warning(content$error)
    return(NULL)
  }
  else if (content$totalCount==0){
    warning("Term, CUI or CUI and term combination unknown")
    return(NULL)
  }
  else return(content)
}

#examples


search_endpoint(term = c("B01AA02","N06AB08"), ontologies = "ATC", service = "bioportal", api_key = api_key, extra_args = "&display_context=false&display_links=false")

search_endpoint(term = c("C43","J00-J99"), ontologies = "ICD10", service = "bioportal", api_key = api_key, extra_args = "&display_context=false&display_links=false")
# Results can potentialy be larger than what is expected. For example with J00-J99 ICD10, it returns also informations about J00 class, and for C43, it returns also informations about C43's ancestor.


#######################################################################################################
#### Fonction qui permet de lancer des "requêtes" sur les service annotator de bioportal et SIFR : ####

#######################################################################################################
#### Fonction qui permet de lancer des "requêtes" sur les service recommendor de bioportal et SIFR : ####



##########################################################################################
#### Fonction that returns ancestors of a class from an ontology on Bioportal or SIFR ####
  # Works for only one class ...
  # A sparql querry could do the same for several class
  # But the ATC is'nt available on the bioportal sparql endpoint ...
  # maybe could be done with http://linked.opendata.cz/sparql/ ?
  # Also with sparql endpoints, transitivity is rarely available, so we can get parents, not ancestors

### Ancestors
get_ancestors <- function(term="",ontology="",api_key=""){
  
  if(length(c("",ontology))!=2 || !is.character(ontology)){
    warning("Give an unique character for ontology parameter")
    return(NULL)
  }
  
  if(term==""){
    warning("Give a term")
    return(NULL)
  }
  
  if (ontology=="ATC") {
    url=paste("http://data.bioontology.org/ontologies/ATC/classes/http%3A%2F%2Fpurl.bioontology.org%2Fontology%2FUATC%2F",term,"/ancestors",sep="")
  }
  else {
    url=paste("http://data.bioontology.org/ontologies/",ontology,"/classes/http%3A%2F%2Fpurl.bioontology.org%2Fontology%2F",ontology,"%2F",term,"/ancestors",sep="")
  }
  results<-GET(url,add_headers(Authorization= paste("apikey token=",api_key,sep="")))
  content <- content(results)
  
  
  if (any(grepl("error",names(content)))){
    warning(content$error)
    return(NULL)
  }
  else if (length(content)==0){
    warning("Term unknown or no ancestors")
    return(NULL)
  }
  
  ancestors_df=setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("id", "prefLabel"))
  for(j in 1:length(content)){
    ancestors_df[dim(ancestors_df)[1]+1,]=c(content[[j]]$"@id",content[[j]]$prefLabel)
  }
  
  return(ancestors_df)
}

api_key="your_api_key"

example1 <- get_ancestors("A05AA",ontology="ATC",api_key = api_key)
example1

example2 <- get_ancestors("C50",ontology="ICD10",api_key = api_key)
example2

example3 <- get_ancestors("N0000153235",ontology="NDFRT",api_key = api_key)
example3

example3 <- get_ancestors("test",ontology="NDFRT",api_key = api_key)
# error test


# Similar functions
  #Get parents
  #Get descendants
  #Get children
  #Get mapping

##############################################################
#### Fonction that match an ATC code to a cui from UMLS : ####
  # Based on search function
  # Arguments : ATC codes (vector/list), api key
  # Returns the mapping as a dataframe
  # There is a limit in the url length. It is necessary to set a limit for number of atc codes, cut the atc vector in several vector, launch the function with each of them and then concatenate results.
  # After some tests, 800 cui codes seems to be a optimal limit.

mapping_atc_cui <- function(codes=NULL,source="cui",api_key=""){
  
  if (source=="cui"){
    codes=unlist(str_extract_all(unique(codes),"C[0-9]{7}"))
  }
  
  n <- length(codes) %/% 800 
  rest <- length(codes) %% 800
  
  results <- data.frame()
  
  if (n>0){
    for(i in 1:n){
      S=search_endpoint(term = paste(codes[((i-1)*800):(i*800-1)],collapse="+"), ontologies = "ATC", service = "bioportal", api_key = api_key, extra_args = "&display_context=false&display_links=false")
      for(j in 1:length(S$collection)){
        results[dim(results)[1]+1,"atc"]=S$collection[[j]]$"@id"
        results[dim(results)[1],"cui"]=S$collection[[j]]$cui
      }
    }
  }
  if (rest>0){
    S=search_endpoint(term = paste(codes[(n*800):(n*800+rest)],collapse="+"), ontologies = "ATC", service = "bioportal", api_key = api_key, extra_args = "&display_context=false&display_links=false")
    for(j in 1:length(S$collection)){
      results[dim(results)[1]+1,"atc"]=S$collection[[j]]$"@id"
      results[dim(results)[1],"cui"]=S$collection[[j]]$cui
    }
  }
    
  return(results)
}

mapping_atc_cui(c("A01AA"),source="atc",api_key=api_key)
mapping_atc_cui(c("A01AA01","B01AA01","A01AA02","A01","A01AA","A02"),source="atc",api_key=api_key)
mapping_atc_cui(c("C0005640","C3653755"),source="cui",api_key=api_key)


##########################################
#### Mapping from icd10 codes to cui: ####
  # Based on sparql() function
  # http://sparql.bioontology.org/sparql/

mapping_icd10_cui <- function(api_key=""){
  query="PREFIX umls: <http://bioportal.bioontology.org/ontologies/umls/>
         SELECT ?icd10 ?cui
         FROM <http://bioportal.bioontology.org/ontologies/ICD10>
         WHERE {?icd10 umls:cui ?cui .}"
  return(sparql(url="http://sparql.bioontology.org/sparql/",query=query, api_key=api_key))
}

cui_icd10 <- mapping_icd10_cui(bioportal_api_key)
head(cui_icd10)

###################################################
#### Mapping from atc codes to drugbank codes: ####
 # Based on sparql() function
 # bio2rdf.org/sparql/

mapping_atc_db <- function(){
  
  query="SELECT DISTINCT ?atc ?db
         WHERE  {?db <http://bio2rdf.org/drugbank_vocabulary:x-atc> ?atc .}"
  
  return(sparql(query,url="bio2rdf.org/sparql/"))
}

atc_db <- mapping_atc_db()
head(atc_db)


############################################################
#### Fonction that load Drug Indication Database (DID): ####
  # It provides main drug indications sources, mapped to CUI. We use previous functions to provide mapping to ICD10 ant ATC codes.
  # Remains to deal with hierarchy !
  # Suggest other mapping (NDF-RT)

get_DID <- function(mapping_atc=TRUE,mapping_icd10=TRUE){
  url="https://static-content.springer.com/esm/art%3A10.1186%2Fs13326-016-0110-0/MediaObjects/13326_2016_110_MOESM1_ESM.xlsx"
  DID <- read.xlsx(url, sheet=3, colNames=F)
  colnames(DID) = gsub("[[:space:]]","_",gsub("NA.","",paste(c("NA","NA","NA",DID[1,4:length(DID)]),DID[2,],DID[3,],sep=".")))
  
  if(mapping_icd10){
    cui_icd10 <- mapping_icd10_cui(bioportal_api_key)
    DID <- merge(DID,cui_icd10,by.x="indication.UMLS_phenotype.CUI" ,by.y="cui",all.x=T)
  }
  
  if(mapping_atc){
    atc_cui <- mapping_atc_cui(unique(DID$drug.UMLS.CUI),api_key=bioportal_api_key)
    DID <- merge(DID,atc_cui,by.x="drug.UMLS.CUI",by.y="cui",all.x=T)
  }
  
  return(DID)
  
}

DID <- get_DID(mapping_atc=TRUE,mapping_icd10=TRUE)
head(DID)
dim(DID)
colnames(DID)

# If you want only indications from ATC to ICD10 codes :
atc_icd10_DID <- DID[!(is.na(DID$atc) | is.na(DID$icd10)),]


########################################################################
#### Fonction that load the Drug Interaction Knowledge Base (DIKB): ####
  # With the option to add mapping from drugbank codes to ATC codes
  # No hierarchy here, each DB code goes to a leaf ATC code.

get_DIKB <- function(path=getwd(),url=NULL,mapping=NULL){
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

  return(DIKB)
}

DIKB <- get_DIKB(path="/tmp",mapping="ATC")

head(DIKB)
colnames(DIKB)
dim(DIKB)

head(DIKB[DIKB$source=="FrenchDB",])



############################################################################################
#### Fonction qui pour un ensemble de code atc retourne leurs indications selon NDF-RT: ####
# Basée sur la fonction sparql
# Arguments : des codes ATC (vecteur/liste de char) et une clef api necessaire
# Retourne
# Il y a une limite de la taille de la requête sur les sparql endpoint
# Il faudrait donc fixer une limite du nombre de code atc, et couper la liste en plusieurs listes sur lesquelles on applique la fonction
# Puis tout concaténer


## A changer, il y a eu des modifs d'autres fonctions qui sont des dépendances

NDFRT_CI_with <- function(atc=NULL, api_key=""){
  
  cui_atc=atc2cui(atc,api_key = api_key)
  
  query_part1="
  prefix owl: <http://www.w3.org/2002/07/owl#>
  prefix ndf: <http://evs.nci.nih.gov/ftp1/NDF-RT/NDF-RT.owl#>
  prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
  prefix umls: <http://bioportal.bioontology.org/ontologies/umls/>
  
  SELECT DISTINCT ?cui_drug ?cui_diag
  FROM <http://bioportal.bioontology.org/ontologies/NDF-RT>
  WHERE {
  
  {?ndf_drug ndf:UMLS_CUI ?cui_drug .}
  UNION
  {?sc_drug_1 rdfs:subClassOf ?ndf_drug .
  ?sc_drug_1 ndf:UMLS_CUI ?cui_drug .}
  UNION
  {?sc_drug_2 rdfs:subClassOf ?ndf_drug .
  ?sc_drug_1 rdfs:subClassOf ?sc_drug_2 .
  ?sc_drug_1 ndf:UMLS_CUI ?cui_drug .}
  
  ?ndf_drug rdfs:subClassOf ?CI .
  ?CI owl:onProperty ndf:CI_with .
  ?CI owl:someValuesFrom ?ndf_diag .
  
  {?ndf_diag ndf:UMLS_CUI ?cui_diag .}
  UNION
  {?sc_diag_1 rdfs:subClassOf ?ndf_diag .
  ?sc_diag_1 ndf:UMLS_CUI ?cui_diag .}
  
  "
  
  query_part2=paste("\nFILTER(?cui_drug IN (",paste("\"",paste(cui_atc$cui,collapse="\",\""),"\"",sep=""),"))}",sep="")
  query=paste(query_part1,query_part2,sep="")
  results=sparql(url="http://sparql.bioontology.org/sparql/",query=query, api_key = api_key)
  results=merge(results,unique(merge(results,cui_atc,by.x="cui_drug",by.y="cui")))
  
  return(results)
  }


bioportal_api_key="your_api_key_here"
NDFRT_CI_with(atc=c("B01AA02","B01AC13","N06AB08","N05AD01"),api_key = bioportal_api_key)


# gérer la transitivité
# ça commence à faire beaucoup, ça met du temps ...

# Les causes :
#Atc n'est pas sur le sparql endpoint
#Les requêtes fédérées sur plusieurs ontologies (NDF-RT, CIM10) prennent beaucoup trop de temps ...
