NDFRT_CI_with <-
function(atc=NULL, api_key=""){
  
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
