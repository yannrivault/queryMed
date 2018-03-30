\name{get_DID}
\alias{get_DID}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
get_DID(mapping_atc = TRUE, mapping_icd10 = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mapping_atc}{
%%     ~~Describe \code{mapping_atc} here~~
}
  \item{mapping_icd10}{
%%     ~~Describe \code{mapping_icd10} here~~
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
DID <- get_DID(mapping_atc=TRUE,mapping_icd10=TRUE)
head(DID)
dim(DID)
colnames(DID)

# If you want only indications from ATC to ICD10 codes :
atc_icd10_DID <- DID[!(is.na(DID$atc) | is.na(DID$icd10)),]
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
