\name{mapping_atc_cui}
\alias{mapping_atc_cui}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mapping_atc_cui(codes = NULL, source = "cui", api_key = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{codes}{
%%     ~~Describe \code{codes} here~~
}
  \item{source}{
%%     ~~Describe \code{source} here~~
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
mapping_atc_cui(c("A01AA"),source="atc",api_key=api_key)
mapping_atc_cui(c("A01AA01","B01AA01","A01AA02","A01","A01AA","A02"),source="atc",api_key=api_key)
mapping_atc_cui(c("C0005640","C3653755"),source="cui",api_key=api_key)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
