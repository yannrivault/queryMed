\name{annotator}
\alias{annotator}

\title{
annotator
}
\description{
Function to use annotator API from Bioportal (https://bioportal.bioontology.org/annotator) and SIFR (http://bioportal.lirmm.fr/annotator) through R. Annotator API recognizes relevant ontology classes in a text given by users.
}
\usage{
annotator(text="",ontologies="",service="bioportal",api_key="")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{text}{
Text the user wants to annotate by ontology classes.
}
  \item{ontologies}{
A set of ontologies that can serve to annoptate text. By default, all available ontologies are used.
}
  \item{api_key}{
An API Key is required to access any API call on Bioportal and SIFR. To have one, you need to register at http://bioportal.bioontology.org/, respectively to http://bioportal.lirmm.fr/ for SIFR.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a data frame describing terms from the text, their relative annotations and ontologies.
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
