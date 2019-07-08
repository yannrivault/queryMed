\name{NDFRT_CI_with}
\alias{NDFRT_CI_with}

\title{
Querying contraindications from the National Drug File - Reference Terminology (NDF-RT)
}
\description{
Retrieve NDF-RT contraindications from drugs to diagnostics. NDF-RT codes can be matched to CUI codes, then to ATC and ICD10 codes. The matching requires an api key to acces BioPortal REST API.
}
\usage{
NDFRT_CI_with(drug_mapping = NULL, diagnostic_mapping = NULL, api_key= "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{drug_mapping}{
Default value is NULL but a specific classification or thesaurus can be specified. For example, if you want to map NDF-RT drug codes to other drug classification available on BioPortal (e.g. "ATC").
}
  \item{diagnostic_mapping}{
Default value is NULL but a specific classification or thesaurus can be specified. For example, if you want to map NDF-RT diagnostic codes to other diagnostic classification available on BioPortal (e.g. "ICD10").
}
  \item{api_key}{
An API Key is required to access any API call on BioPortal. To have one, you need to register at http://bioportal.bioontology.org/.
}
}
\value{
Data table that includes uri, cui codes drug contraindicated for diagnostics 
}
\references{
Lincoln MJ, Brown SH, et al. U.S. Department of Veterans Affairs Enterprise Reference Terminology Strategic Overview. Stud Health Technol Inform. 2004;107(Pt 1):391-5.

Brown SH, Elkin PL, et al. VA National Drug File Reference Terminology: A Cross-institutional Content Coverage Study. Stud Health Technol Inform. 2004;107(Pt 1):477-81.
}
\author{
Y. Rivault
}

\examples{
\dontrun{
   cindication <- NDFRT_CI_with(drug_mapping = "ATC", diagnostic_mapping = c("ICD10","ICD10CM"), api_key= "yourAPIkey")
   }
}

\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
