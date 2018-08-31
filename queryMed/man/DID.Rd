\name{DID}
\alias{DID}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 Drug Indication Database (DID)
}
\description{
Curated version of the Drug Indication Database (DID) downloaded from supplementary material of Sharp ME (2017).
}
\usage{
DID
}
%- maybe also 'usage' for other objects documented here.
\format{
\describe{
Data frame of 66 variables by 464 056 rows. Drugs and their phenotype indications (first two variables) are coded according to Concept Unique Identifier (CUI) codification from the Unified Medical Language System (UMLS). The other variables define their characteristics. 
  }
}
\references{
Sharp ME (2017). Toward a comprehensive drug ontology: extraction of drug-indication relations from diverse information sources. Journal of Biomedical Semantics,8:2. 

Sharp ME (2017) supplementary material: https://figshare.com/articles/Additional_file_1_of_Toward_a_comprehensive_drug_ontology_extraction_of_drug-indication_relations_from_diverse_information_sources/4535021
}
\author{
Y. Rivault & N. Le Meur
}

\seealso{
See Also \code{\link{get_DID}} functions
}
\examples{
\dontrun{
data(DID)
colnames(DID)[1:10]
dim(DID)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
