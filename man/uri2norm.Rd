\name{uri2norm}
\alias{uri2norm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
uri2norm
}
\description{
fonction that turn URI(s) into variable
}
\usage{
uri2norm(X)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
One URI, a vector composed of URIs, a matrix of URIs or a dataframe of URIs.
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
uri2norm("http://purl.bioontology.org/ontology/UATC/A05A")
uri2norm(c("http://purl.bioontology.org/ontology/UATC/A05A","http://purl.bioontology.org/ontology/UATC/A05A01"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
