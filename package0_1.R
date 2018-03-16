require(jsonlite)
require(httr)
require(openxlsx)
require(RCurl)
require(hash)

# Note : la première requête lancée sur un sparql endpoint peut s'avérer longue comparativement aux suivantes
# Relancer pour avoir le vrai temps d'execution

############################################################################
#### Fonction qui transforme les uri en données normales/exploitables : ####
  # Argument : une uri, un vecteur d'uri, une matrice d'uri, un dataframe d'uri
  # Retourne le même objet sous la forme d'une matrix sans les uri

uri2norm <- function(X){
  # X peut être une liste, un vecteur, une matrice, etc
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

##############################################################################
#### Fonction qui permet de lancer des requêtes sur des sparql endpoint : ####
  # Alternative à la fonction SPARQL du package SPARQL.
  # SPARQL() ne propose pas d'extraire des données au format JSON, mais uniquement XML/TSV/CSV
  # Or json est un format bien plus léger qu'XML
  # Arguments : une requête sparql (char), une url vers un sparql endpoint (char), et une clef api si necessaire (char)
  # Retourne le dataframe du résultat de la requête

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



query="PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
      SELECT DISTINCT ?x ?label
      WHERE  {?x skos:prefLabel ?label .}
      limit 10"

sparql(query=query,url="bio2rdf.org/sparql/") #test de résultat null
sparql(query=query,url="https://dbpedia.org/sparql")


####################################################################################################
#### Fonction qui permet de lancer des "requêtes" sur les service search de bioportal et SIFR : ####
  # Alternative au requêtage de sparql endpoints
  # L'ATC par exemple n'est pas disponible sur le sparql endpoint de Bioportal mais dispo sur le service search
  # Moins d'info disponibles que sur le sparql endpoint
  # Arguments : les termes recherchés (vecteur/liste de char), un filtre de cui optionel (vecteur/liste de char), le service search (bioportal ou sifr), la clef api necessaire, argument supplémentaire optionel
  # Retourne le résultat de la requête search en JSON

search <- function(term="",cui="",ontologies="",service="bioportal",api_key="",extra_args=""){
  
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



#######################################################################################################
#### Fonction qui permet de lancer des "requêtes" sur les service annotator de bioportal et SIFR : ####

#######################################################################################################
#### Fonction qui permet de lancer des "requêtes" sur les service recommendor de bioportal et SIFR : ####



######################################################################################################
#### Fonctions qui retourne les ancêtres d'un code d'une ontologie présente sur bioportal et SIFR ####

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

bioportal_api_key="your_api_key"

example1 <- get_ancestors("A05AA",ontology="ATC",api_key = bioportal_api_key)
example1

example2 <- get_ancestors("C50",ontology="ICD10",api_key = bioportal_api_key)
example2

example3 <- get_ancestors("N0000153235",ontology="NDFRT",api_key = bioportal_api_key)
example3

example3 <- get_ancestors("ssdfsdfg",ontology="NDFRT",api_key = bioportal_api_key)
# test d'erreur


# Fonctions très similaires :
  #Get parents
  #Get descendants
  #Get children
  #Get mapping

###################################################################################
#### Fonction qui fait le mapping entre un code atc et un code cui de l'UMLS : ####
  # Basée sur la fonction search
  # Arguments : des codes ATC (vecteur/liste de char), la clef api necessaire
  # Retourne un dataframe du mapping atc cui
  # Il y a une limite de la taille de la requête sur les sparql endpoint
  # Il faudrait donc fixer une limite du nombre de code atc, et couper la liste en plusieurs listes sur lesquelles on applique la fonction
  # Puis tout concaténer

atc2cui <- function(atc=NULL,api_key=""){
  S=search(term = paste(atc,collapse="+"), ontologies = "ATC", service = "bioportal", api_key = api_key, extra_args = "&display_context=false&display_links=false")
  res <- data.frame()
  for(i in 1:length(S$collection)){
    res[i,"atc"]=S$collection[[i]]$"@id"
    res[i,"cui"]=S$collection[[i]]$cui
  }
  return(res)
}

atc2cui(c("A01AA01"),api_key=bioportal_api_key)
atc2cui(c("A01AA01","B01AA01","A01AA02","A01","A01AA","A02"),api_key=bioportal_api_key)

###########################################################################
#### Fonction qui fait le mapping entre un code cui et un code icd10 : ####
  # Basée sur la fonction sparql
  # va chercher le mapping sur http://sparql.bioontology.org/sparql/
  # Ajouter des labels

mapping_icd10_cui <- function(api_key=""){
  query="PREFIX umls: <http://bioportal.bioontology.org/ontologies/umls/>
         SELECT ?icd10 ?cui
         FROM <http://bioportal.bioontology.org/ontologies/ICD10CM>
         WHERE {?icd10 umls:cui ?cui .}"
  return(sparql(url="http://sparql.bioontology.org/sparql/",query=query, api_key=api_key))
}

cui_icd10 <- mapping_icd10_cui(bioportal_api_key)
head(cui_icd10)

##############################################################################
#### Fonction qui fait le mapping entre un code atc et un code drugbank : ####
 # Basée sur la fonction sparql
 # va chercher le mapping sur bio2rdf.org/sparql/
 # Ajouter des labels

mapping_atc_db <- function(){
  
  query="SELECT DISTINCT ?atc ?db
         WHERE  {?db <http://bio2rdf.org/drugbank_vocabulary:x-atc> ?atc .}"
  
  return(sparql(query,url="bio2rdf.org/sparql/"))
}

atc_db <- mapping_atc_db()
head(atc_db)

#############################################################################################
#### Fonction qui cherche dans un ensemble de code atc les intéractions selon DrugBank : ####
  # Basée sur la fonction sparql
  # Arguments : des codes ATC (vecteur/liste de char)
  # Retourne un dictionnaire (hash), à voir pour le format, peut être en proposer plusieurs (data.frame, hash, etc)
  # Il y a une limite de la taille de la requête sur les sparql endpoint
  # Il faudrait donc fixer une limite du nombre de code atc, et couper la liste en plusieurs listes sur lesquelles on applique la fonction
  # Puis tout concaténer

db_interactions <- function(atc=NULL){
  #On s'assure que ce sont bien des codes atc:
  atc=unique(atc[grep("[A-Z]{1}[0-9]{2}[A-Z]{2}[0-9]{2}",atc)])
  if(length(atc)==0){return(NULL)}
  else{
    query=paste("PREFIX atc: <http://bio2rdf.org/atc:>
                PREFIX db: <http://bio2rdf.org/drugbank_vocabulary:>
                SELECT DISTINCT ?atc_1 ?atc_2 ?label
                WHERE  {
                ?DB_drug_1 db:x-atc ?atc_1 .
                ?DB_drug_1 db:ddi-interactor-in ?DDI .
                ?DB_drug_2 db:x-atc ?atc_2 .
                ?DB_drug_2 db:ddi-interactor-in ?DDI .
                
                ?DDI dcterms:title ?label .
                
                FILTER(?DB_drug_1 != ?DB_drug_2)
                FILTER(?atc_1 in (",paste("atc:",atc,sep="",collapse=","),"))
                FILTER(?atc_2 in (",paste("atc:",atc,sep="",collapse=","),"))
                FILTER(?atc_1 != ?atc_2)
                }
                ",sep="")
    
    
    temp=sparql(query,"bio2rdf.org/sparql/")
    
    if(length(temp)==0){return(NULL)}
    else{
      temp[,c("atc_1","atc_2")]=uri2norm(temp[,c("atc_1","atc_2")])
      res=hash()
      for(i in 1:dim(temp)[1]){
        if(temp[i,"atc_1"] %in% keys(res)){
          if(temp[i,"atc_2"] %in% keys(res[temp[i,"atc_1"]])){
            res[[temp[i,"atc_1"]]][[temp[i,"atc_2"]]]=c(res[temp[i,"atc_1"]][temp[i,"atc_2"]],temp[i,"label"])
          }
          else{
            res[[temp[i,"atc_1"]]][[temp[i,"atc_2"]]]=temp[i,"label"]
          }
        }
        else{
          res[[temp[i,"atc_1"]]]=hash(temp[i,"atc_2"],temp[i,"label"])
        }
      }
      return(res)
    }
  }
}

db_interactions(c("B01AA02","B01AC13","N06AB08"))

# Est-ce vraiment utile compte tenu des dernières avancées avec l'import des bases de données des intéractions ?

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
  
  cui_atc=atc2cui(atc,api_key=api_key)
  
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
  
  results=merge(results,cui2icd10(results$cui_diag,api_key=api_key),by.x="cui_diag",by.y="cui")
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



##################################################################################
#### Fonction qui charge la base de données des indications d'un médicament : ####
  # On a donc les grandes sources d'indication entre médicaments et diagnostics, il faut ensuite faire les mappings et gérer la hiérarchie, avec plusieurs autres terminologies, l'ATC dans un premier temps

get_DID <- function(){
  url="https://static-content.springer.com/esm/art%3A10.1186%2Fs13326-016-0110-0/MediaObjects/13326_2016_110_MOESM1_ESM.xlsx"
  DID <- read.xlsx(url, sheet=3, colNames=F)
  colnames(DID) = gsub("[[:space:]]","_",gsub("NA.","",paste(c("NA","NA","NA",DID[1,4:length(DID)]),DID[2,],DID[3,],sep=".")))
  
  return(DID[4:dim(DID)[1],])
}

DID <- get_DID()
head(DID)
dim(DID)
colnames(DID)

# Le mapping cui icd10 est facile
# Le mapping cui atc est compliqué ... et reste à faire
# Proposer en option différents mappings ou pas de mappings (conserver cui donc)

#####################################################################################
#### Fonction qui charge la base de données des interactions entre médicaments : ####
  # On a donc les grandes sources d'intéractions entre médicaments, il faut ensuite faire les mappings et gérer la hiérarchie, avec plusieurs autres terminologies
  # Avec l'ATC c'est hyper simple, chaque code DB mène à un code (ou plusieurs ?) code ATC feuille

get_DIKB <- function(path=getwd(),url=NULL,mapping=NULL){
  # Au cas où l'url change
  if(is.null(url)) url="https://dbmi-icode-01.dbmi.pitt.edu/dikb-evidence/pddi-sets/CombinedDatasetNotConservative.csv.bz2"
  set_config(config(ssl_verifypeer = 0L))
  get.DIKB <- GET(url,write_disk(paste(path,"DIKB.csv.bz2",sep="/"),overwrite=T))
  DIKB <- read.csv(file.path(path,"DIKB.csv.bz2"),sep="\t",header=T,fileEncoding = "UTF-8")
  

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
DIKB[1:100,c("drug1","drug2")]

head(DIKB[DIKB$source=="FrenchDB",])

# Certaines interactions ne mènent à aucun code DB (http://bio2rdf.org/drugbank:), et donc à aucun code ATC
# On tout de même la source d'intéractions entre code atc la plus importante qu'il soit, avec des contre-indicaitons ...

