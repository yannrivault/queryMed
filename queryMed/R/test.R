test <- function(){
  download.file("http://data.bioontology.org/ontologies/ATC/submissions/9/download?apikey=8b5b7825-538d-40e0-9e9e-5ab9274a9aeb","/home/ehesp/Téléchargements/ATC.ttl")
  exData = load.rdf("/home/ehesp/Téléchargements/ATC.ttl", format="TURTLE")
  print(exData)
  
}
