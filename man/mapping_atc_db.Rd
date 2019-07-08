\name{mapping_atc_db}
\alias{mapping_atc_db}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Mapping the Anatomical Therapeutic and Chemical classidication to DrugBank
}
\description{
The function allows to get all the mapping from the Anatomical Therapeutic and Chemical classification to DrugBank, through the bio2rdf and dbpedia sparql endpoints.
}
\usage{
mapping_atc_db()
}
\details{
No argument needed. 
}
\value{
Return a data.table of size 2993 rows by 2  columns giving the whole Anatomical Therapeutic and Chemical classification to DrugBank mapping.
}
\references{
Alison Callahan, Jose Cruz-Toledo, Peter Ansell, Michel Dumontier: Bio2RDF Release 2: Improved Coverage, Interoperability and Provenance of Life Science Linked Data. ESWC 2013: 200-212. 

Lehmann, J. et al. (2015). Dbpedia - a large-scale, multilingual knowledge base
extracted from wikipedia. Semantic Web, 6, 167–195.
}
\author{
Y. Rivault
}
\note{
%%  ~~further notes~~
}

\seealso{
get_DID
}
\examples{
\dontrun{
  xx = mapping_atc_db()
  head(xx)
}
}
\keyword{ATC}% use one of  RShowDoc("KEYWORDS")
\keyword{DB}% __ONLY ONE__ keyword per line
