\name{NDFRT_CI_with}
\alias{NDFRT_CI_with}

\title{
Retrieve NDFRT indication from ATC codes
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
NDFRT_CI_with(atc = NULL, api_key = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{atc}{
%%     ~~Describe \code{atc} here~~
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
bioportal_api_key="e6f2d058-f206-4ac7-a8f9-60b84c9e57dc"
NDFRT_CI_with(atc=c("B01AA02","B01AC13","N06AB08","N05AD01"),api_key = bioportal_api_key)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
