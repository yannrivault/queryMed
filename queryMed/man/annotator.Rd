\name{annotator}
\alias{annotator}

\title{
Bioportal and SIFR annotator 
}
\description{
Function to use annotator API from Bioportal (https://bioportal.bioontology.org/annotator) and SIFR (http://bioportal.lirmm.fr/annotator) through R. Annotator API recognizes relevant ontology classes in a text given by users.
}
\usage{
annotator(text="",ontologies="",service= "bioportal",api_key="")
}

\arguments{
  \item{text}{
Text the user wants to annotate by ontology classes.
}
  \item{ontologies}{
A set of ontologies that can serve to annotate text. By default, all available ontologies are used.
}
  \item{service}{
Character vector that specifies the web service to query either "bioportal" or "sifr".
}
  \item{api_key}{
An API Key is required to access any API call on Bioportal and SIFR. To have one, you need to register at http://bioportal.bioontology.org/, respectively to http://bioportal.lirmm.fr/ for SIFR.
}
}
\value{
Returns a data frame describing terms from the text, their relative annotations and ontologies.
}
\references{
Clement Jonquet, Amina Annane, Khedidja Bouarech, Vincent Emonet & Soumia Melzi. SIFR BioPortal : Un portail ouvert et générique d’ontologies et de terminologies biomédicales françaises au service de l’annotation sémantique, In 16th Journées Francophones d'Informatique Médicale, JFIM'16. Genève, Suisse, July 2016. pp. 16.

Whetzel PL, Noy NF, Shah NH, Alexander PR, Nyulas C, Tudorache T, Musen MA. BioPortal: enhanced functionality via new Web services from the National Center for Biomedical Ontology to access and use ontologies in software applications. Nucleic Acids Res. 2011 Jul;39(Web Server issue):W541-5. Epub 2011 Jun 14.
}
\author{
Y. Rivault
}
\examples{
\dontrun{
annotator(text="",ontologies="",service="bioportal",api_key="yourAPIKey")
}
}

