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
%% ~put references to the literature/web site here ~
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
# require api_key to run

# get_ancestors("A05AA",ontology="ATC",api_key = api_key)
# get_ancestors("C50",ontology="ICD10",api_key = api_key)
# get_ancestors("N0000153235",ontology="NDFRT",api_key = api_key)


#example3 <- get_ancestors("test",ontology="NDFRT",api_key = api_key)
# error test
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
