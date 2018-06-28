\name{get_ancestors}
\alias{get_ancestors}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
get_ancestors
}
\description{
Retrieving a list of ancestors of a class from ontologies available on Bioportal or SIFR web service.
}
\usage{
get_ancestors(term = "", ontology = "", api_key = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{term}{
Term as character vector
}
  \item{ontology}{
Name of the ontology as character vector
}
  \item{api_key}{
An API Key is required to access any API call on Bioportal and SIFR. To have one, you need to register at http://bioportal.bioontology.org/, respectively to http://bioportal.lirmm.fr/ for SIFR.
}
}
\details{
The available ontologies are for instance: ATC, ICD10, NDFRT,SNOMEDCT. See http://bioportal.bioontology.org/ for more details.
}
\value{
Returns a list of ancestors.
%%  If it is a LIST, use
\item{id }{URI with ancestors' code}
\item{prefLabel}{Labels of the retrieve term}
%% ...
}
\references{
Clement Jonquet, Amina Annane, Khedidja Bouarech, Vincent Emonet & Soumia Melzi. SIFR BioPortal : Un portail ouvert et générique d’ontologies et de terminologies biomédicales françaises au service de l’annotation sémantique, In 16th Journées Francophones d'Informatique Médicale, JFIM'16. Genève, Suisse, July 2016. pp. 16.

Whetzel PL, Noy NF, Shah NH, Alexander PR, Nyulas C, Tudorache T, Musen MA. BioPortal: enhanced functionality via new Web services from the National Center for Biomedical Ontology to access and use ontologies in software applications. Nucleic Acids Res. 2011 Jul;39(Web Server issue):W541-5. Epub 2011 Jun 14.
}
\author{
Y. Rivault
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
\dontrun{
get_ancestors("A05AA",ontology="ATC",api_key = "yourAPIkey")
get_ancestors("C50",ontology="ICD10",api_key = "yourAPIkey")
get_ancestors("N0000153235",ontology="NDFRT",api_key = "yourAPIkey")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
