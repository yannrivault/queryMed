NDFRT_CI_with <- function(drug_mapping=NULL,diagnostic_mapping=NULL,api_key=""){
  
  query="
  prefix ndf: <http://evs.nci.nih.gov/ftp1/NDF-RT/NDF-RT.owl#>
  prefix umls: <http://bioportal.bioontology.org/ontologies/umls/>
  
  SELECT DISTINCT ?ndf_drug ?ndf_diag ?cui_drug ?cui_diag
  FROM <http://evs.nci.nih.gov/ftp1/NDF-RT/NDF-RT.owl>
  WHERE {
  
  {?ndf_drug ndf:UMLS_CUI ?cui_drug .}
  UNION
  {?ndf_drug_low rdfs:subClassOf ?ndf_drug .
  ?ndf_drug_low ndf:UMLS_CUI ?cui_drug .
  }

  ?ndf_drug rdfs:subClassOf ?CI .
  ?CI owl:onProperty ndf:CI_with ;
      owl:someValuesFrom ?ndf_diag .
  
  ?ndf_diag ndf:UMLS_CUI ?cui_diag .

  }
  "
  
  results=sparql(url="http://sparql.hegroup.org/sparql/",query=query)
  
  if(!is.null(drug_mapping)){
    drugs=mapping_cui(codes=results$cui_drug,ontologies=drug_mapping,api_key=api_key)
    if("cui" %in% colnames(drugs)){
      results=merge(results,unique(merge(results,drugs,by.x="cui_drug",by.y="cui")),all.x=T)
      colnames(results)[colnames(results)=="mapping"]="drug_mapping"
    }
  }
  
  if(!is.null(diagnostic_mapping)){
    diagnostics=mapping_cui(codes=results$cui_diag,ontologies=diagnostic_mapping,api_key=api_key)
    if("cui" %in% colnames(diagnostics)){
      results=merge(results,unique(merge(results,diagnostics,by.x="cui_diag",by.y="cui")),all.x=T)
      colnames(results)[colnames(results)=="mapping"]="diag_mapping"
    }
  }
  
  return(results)
}
