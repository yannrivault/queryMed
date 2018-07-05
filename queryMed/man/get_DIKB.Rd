\name{get_DIKB}
\alias{get_DIKB}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve Drug Interaction Knowledge Base (DIKB) data
}
\description{
This function downloads the Drug Interaction Knowledge Base (DIKB) from source.
}
\usage{
get_DIKB(path = getwd(), url = NULL, mapping = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path}{
filepath where to download the Drug Interaction Knowledge Base. By default it is the current working directory.
}
  \item{url}{
URL of the location of the Drug Interaction Knowledge Base (DIKB). By default it is https://dbmi-icode-01.dbmi.pitt.edu/dikb-evidence/pddi-sets/ but the argument exists in case the url happens to change.
}
  \item{mapping}{
Optional. Possible argument is ATC and the mapping is performed between DIKB and ATC information from  http://bio2rdf.org and https://dbpedia.org/
.
}
}
\details{
The function download the less concervative version of all PDDI datasets merged by Richard D. Boyce's group at University of Pittsburgh. See https://dbmi-icode-01.dbmi.pitt.edu/dikb-evidence/pddi-sets/ for details.
}
\value{
Data frame with all variables avaible from Drug Interaction Knowledge Base (DIKB).
}
\references{
Ayvaz et al. (2015) Toward a Complete Dataset of Drug–drug Interaction Information from Publicly Available Sources. Journal of Biomedial Informtics, 55: 206-217.
}
\author{
Y. Rivault
}

\seealso{
See Also \code{\link{get_DIKB}} and \code{\link{mapping_atc_db}} functions
}
\examples{
\dontrun{
DIKB <- get_DIKB(path="/tmp",mapping="ATC")
head(DIKB)
colnames(DIKB)
dim(DIKB)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
