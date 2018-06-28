\name{NDFRT_CI_with}
\alias{NDFRT_CI_with}

\title{
Querying contraindications from the National Drug File - Reference Terminology (NDF-RT)
}
\description{
Retrieve NDFRT contraindications from drugs to diagnostics. NDF-RT codes can be matched to CUI codes, then to ATC and ICD10 codes. The matching requires an api key to acces bioportal REST API.
}
\usage{
NDFRT_CI_with(drug_mapping = NULL, diagnostic_mapping = NULL, api_key= "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{drug_mapping}{
If you want to mapp NDF-RT drug codes to other drug classification available on BIOPORTAL (e.g. "ATC").
}
  \item{diagnostic_mapping}{
If you want to mapp NDF-RT diagnostic codes to other diagnostic classification available on BIOPORTAL (e.g. "ICD10").
}
  \item{api_key}{
An API Key is required to access any API call on Bioportal. To have one, you need to register at http://bioportal.bioontology.org/.
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
\dontrun{
   cindication <- NDFRT_CI_with(drug_mapping = "ATC", diagnostic_mapping = c("ICD10","ICD10CM"), api_key= "yourAPIkey")
   }
}

\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
