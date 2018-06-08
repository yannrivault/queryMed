\name{check_interaction}
\alias{check_interaction}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve potential drug-drug interaction from a list of ATC codes in DIKB
}
\description{
Given a list of ATC codes, this function search for potential drug-drug interaction as listed in DIKB data base.
}
\usage{
check_interaction(drugs="")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{drug}{
  a character vector of two or more ATC codes (http://www.who.int/classifications/atcddd/en/)
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
When one or more potential drug-drug interactions are found the corresponding subsetting of the DIKB data base is returned. 
}
\references{
Ayvaz et al. (2015) Toward a Complete Dataset of Drug–drug Interaction Information from Publicly Available Sources. Journal of Biomedial Informtics, 55: 206-217.
}
\author{
N. Le Meur
}

\seealso{
See Also as \code{\link{DIKB}}
}
\examples{
check_interaction(drugs=c("C01BD01", "C03CA01","C07AA05"))
}
\keyword{ ~interaction }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~pddi }% __ONLY ONE__ keyword per line
