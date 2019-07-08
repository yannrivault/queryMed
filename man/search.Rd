\name{search}
\alias{search}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Access to BioPortal or SIFR search REST API
}
\description{
This function allows retrieving knowledge from medical ontologies through BioPortal or SIFR search REST API.
}
\usage{
search(term = "", ontologies = "", service = "bioportal", api_key = "", extra_args = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{term}{
A list of terms to search for informations on Bioportal. It can be codes from nomenclatures.
}
  \item{ontologies}{
A list of ontologies to constrain the search of informations.
}
  \item{service}{
Character vector that specifies the web service to query either "bioportal" or "sifr".
}
  \item{api_key}{
An API Key is required to access any API call on BioPortal and SIFR. To have one, you need to register at http://bioportal.bioontology.org/, respectively to http://bioportal.lirmm.fr/ for SIFR.
}
  \item{extra_args}{
Extra arguments to parse into the query.
}
}
\details{
This is a low level function. The return are return as JSON list of lists.
}
\value{
The return value is a list of lists of elements that define the request term
}
\references{
Clement Jonquet, Amina Annane, Khedidja Bouarech, Vincent Emonet & Soumia Melzi. SIFR BioPortal : Un portail ouvert et générique d’ontologies et de terminologies biomédicales françaises au service de l’annotation sémantique, In 16th Journées Francophones d'Informatique Médicale, JFIM'16. Genève, Suisse, July 2016. pp. 16.

Whetzel PL, Noy NF, Shah NH, Alexander PR, Nyulas C, Tudorache T, Musen MA. BioPortal: enhanced functionality via new Web services from the National Center for Biomedical Ontology to access and use ontologies in software applications. Nucleic Acids Res. 2011 Jul;39(Web Server issue):W541-5. Epub 2011 Jun 14.

Salvadores M, Horridge M, Alexander PR, Fergerson RW, Musen MA, and Noy NF. Using SPARQL to Query BioPortal Ontologies and Metadata. International Semantic Web Conference. Boston US. LNCS 7650, pp. 180195, 2012.
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
disease72 = search(term = "I72", ontologies = "ICD10", service = "bioportal", api_key = "yourAPIkey")
disease72$collection[[1]]$prefLabel
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
