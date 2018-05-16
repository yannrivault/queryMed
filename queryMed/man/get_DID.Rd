\name{get_DID}
\alias{get_DID}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
get_DID
}
\description{
Function that load Drug Indication Database (DID) from source.
}
\usage{
get_DID(mapping_atc = TRUE, mapping_icd10 = TRUE, api_key=="")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mapping_atc}{
  By default the ragulent is FALSE. When TRUE an additional maping to ATC code is performed but a valid API Key is required to connect to and query BIOPORTAL services.
}
  \item{mapping_icd10}{
  By default the ragulent is FALSE. When TRUE an additional maping to ICD10 code is performed but a valid API Key is required to connect to and query BIOPORTAL services.
}
\item{api_key}{
  Optional argumant. A valid API Key is required to access any API call on Bioportal. To have one, you need to register at http://bioportal.bioontology.org/.
}
}
\details{
See https://figshare.com/articles/Additional_file_1_of_Toward_a_comprehensive_drug_ontology_extraction_of_drug-indication_relations_from_diverse_information_sources/4535021 for details 
}
\value{
Data frame with all Drug Indication Database variables and possible mapping to ATC and/or ICD10 if arguments are set to TRUE and a valid API Key is provided.
}
\references{
Sharp ME (2017). Toward a comprehensive drug ontology: extraction of drug-indication relations from diverse information sources. Journal of Biomedical Semantics,8:2.
}
\author{
Y. Rivault
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See Also \code{\link{get_DIKB}} and \code{\link{mapping_cui}} functions
}
\examples{
DID <- get_DID(mapping_atc=FALSE,mapping_icd10=FALSE)
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
