\name{search}
\alias{search}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Acces to search bioportal or SIFR REST API
}
\description{
This function allows retrieving knowledge from medical ontologies through Bioportal or SIFR search REST API.
}
\usage{
search(term = "", cui = "", ontologies = "", service = "bioportal", api_key = "", extra_args = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{term}{
A list of terms to search for informations on Bioportal.
}
  \item{cui}{
A list of cui to constrain the search of informations.
}
  \item{ontologies}{
A list of ontologies to constrain the search of informations.
}
  \item{service}{
"bioportal" or "sifr"
}
  \item{api_key}{
An API Key is required to access any API call on Bioportal and SIFR. To have one, you need to register at http://bioportal.bioontology.org/, respectively to http://bioportal.lirmm.fr/ for SIFR.
}
  \item{extra_args}{
Extra arguments to parse into the query.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
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
