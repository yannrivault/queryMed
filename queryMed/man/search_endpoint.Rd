\name{search_endpoint}
\alias{search_endpoint}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
search_endpoint(term = "", cui = "", ontologies = "", service = "bioportal", api_key = "", extra_args = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{term}{
%%     ~~Describe \code{term} here~~
}
  \item{cui}{
%%     ~~Describe \code{cui} here~~
}
  \item{ontologies}{
%%     ~~Describe \code{ontologies} here~~
}
  \item{service}{
%%     ~~Describe \code{service} here~~
}
  \item{api_key}{
%%     ~~Describe \code{api_key} here~~
}
  \item{extra_args}{
%%     ~~Describe \code{extra_args} here~~
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
api_key ="e6f2d058-f206-4ac7-a8f9-60b84c9e57dc"
search_endpoint(term = c("B01AA02","N06AB08"), ontologies = "ATC", service = "bioportal", api_key = api_key, extra_args = "&display_context=false&display_links=false")

search_endpoint(term = c("C43","J00-J99"), ontologies = "ICD10", service = "bioportal", api_key = api_key, extra_args = "&display_context=false&display_links=false")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
