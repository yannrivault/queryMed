\name{get_ancestors}
\alias{get_ancestors}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieving ancestors of a class from an ontology
}
\description{
Retrieving ancestors of a class from an ontology on Bioportal or SIFR
}
\usage{
get_ancestors(term = "", ontology = "", api_key = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{term}{
%%     ~~Describe \code{term} here~~
}
  \item{ontology}{
%%     ~~Describe \code{ontology} here~~
}
  \item{api_key}{
%%     ~~Describe \code{api_key} here~~
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
api_key="e6f2d058-f206-4ac7-a8f9-60b84c9e57dc"

example1 <- get_ancestors("A05AA",ontology="ATC",api_key = api_key)
example1

example2 <- get_ancestors("C50",ontology="ICD10",api_key = api_key)
example2

example3 <- get_ancestors("N0000153235",ontology="NDFRT",api_key = api_key)
example3

#example3 <- get_ancestors("test",ontology="NDFRT",api_key = api_key)
# error test
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
