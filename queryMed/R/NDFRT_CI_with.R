NDFRT_CI_with <- function(api_key=""){
  
  query="
  prefix owl: <http://www.w3.org/2002/07/owl#>
  prefix ndf: <http://evs.nci.nih.gov/ftp1/NDF-RT/NDF-RT.owl#>
  prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
  prefix umls: <http://bioportal.bioontology.org/ontologies/umls/>
  
  SELECT DISTINCT ?ndf_drug ?ndf_diag ?cui_drug ?cui_diag
  FROM <http://evs.nci.nih.gov/ftp1/NDF-RT/NDF-RT.owl>
  WHERE {
  
  ?ndf_drug ndf:UMLS_CUI ?cui_drug .
  
  ?ndf_drug rdfs:subClassOf ?CI .
  ?CI owl:onProperty ndf:CI_with .
  ?CI owl:someValuesFrom ?ndf_diag .
  
  ?ndf_diag ndf:UMLS_CUI ?cui_diag .
  }
  "
  
  results=sparql(url="http://sparql.hegroup.org/sparql/",query=query)
  
  cui_atc=mapping_cui_search(codes=results$cui_drug,ontology="ATC",source="cui",api_key=api_key)
  cui_icd10=mapping_cui_search(codes=results$cui_diag,ontology="ICD10",source="cui",api_key=api_key)
  
  results=merge(results,unique(merge(results,cui_atc,by.x="cui_drug",by.y="cui")),all.x=T)
  results=merge(results,unique(merge(results,cui_icd10,by.x="cui_diag",by.y="cui")),all.x=T)
  
  return(results)
  }