mapping_icd10_cui <-
function(api_key=""){
  query="PREFIX umls: <http://bioportal.bioontology.org/ontologies/umls/>
         SELECT ?icd10 ?cui
         FROM <http://bioportal.bioontology.org/ontologies/ICD10>
         WHERE {?icd10 umls:cui ?cui .}"
  return(sparql(url="http://sparql.bioontology.org/sparql/",query=query, api_key=api_key))
}
