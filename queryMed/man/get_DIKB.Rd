\name{get_DIKB}
\alias{get_DIKB}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
get_DIKB
}
\description{
Fonction that load the Drug Interaction Knowledge Base (DIKB)
}
\usage{
get_DIKB(path = getwd(), url = NULL, mapping = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path}{
%%     ~~Describe \code{path} here~~
}
  \item{url}{
%%     ~~Describe \code{url} here~~
}
  \item{mapping}{
%%     ~~Describe \code{mapping} here~~
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
DIKB <- get_DIKB(path="/tmp",mapping="ATC")

head(DIKB)
colnames(DIKB)
dim(DIKB)

head(DIKB[DIKB$source=="FrenchDB",])

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
