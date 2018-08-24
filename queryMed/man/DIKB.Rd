\name{DIKB}
\alias{DIKB}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Drug Interaction Knowledge Base (DIKB) data
}
\description{
Curated version of the Drug Interaction Knowledge Base (DIKB). The modalities of the various variables depend on the  data source (ie. KEGG, NDFRT, ANSM). See reference for more details.
}
\usage{
DIKB
}
%- maybe also 'usage' for other objects documented here.
\format{
\describe{
Data frame of 15 variables by 202 363 rows.
  \item{drug2}{Drug bank ID of the second (precipitant) interacting drug}
  \item{drug2}{Drug bank ID of the first (object) interacting drug}
  \item{object}{Full name of the the first interacting drug}
  \item{precipitant}{Full name of the the second interacting drug}
  \item{contraindication}{Logical vector that states whether or not it is a conterindication according to the KEGG database only}
\item{ddiPkMechanism}{Raw text explaining the drug-drug interaction}
\item{effectConcept}{Raw text explaining the drug-drug interaction effect}
\item{label}{Raw text explaining the drug-drug interaction and potential associated recommendations}
\item{precaution}{Logical vector that states whether or not precaution are needed according to the KEGG database only}
\item{severity}{Severity of the interactions coded differently according to the different sources. See references for details}
\item{uri}{Source uri}
\item{source}{Database from which are issued the data}
\item{evidenceStatement}{Raw text that states the evidence of the interaction}
\item{atc1}{ATC code the first (object) interacting drug}
\item{atc2}{ATC code the second (precipitant) interacting drug}
}
}
\references{
Ayvaz et al. (2015) Toward a Complete Dataset of Drug–drug Interaction Information from Publicly Available Sources. Journal of Biomedial Informtics, 55: 206-217.

Richard D. Boyce's group at University of Pittsburgh. See https://dbmi-icode-01.dbmi.pitt.edu/dikb-evidence/pddi-sets/
}
\author{
Y. Rivault & N. Le Meur
}

\seealso{
See Also \code{\link{get_DIKB}} functions
}
\examples{
\dontrun{
data(DIKB)
colnames(DIKB)
dim(DIKB)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
