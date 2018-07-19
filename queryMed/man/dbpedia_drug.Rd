\name{dbpedia_drug}
\alias{dbpedia_drug}

\title{
dbpedia_drug
}
\description{
Retrieve drug information from dbpedia such as atc and drug bank identifier codes
}
\usage{
dbpedia_drug(lang="en",atc=NULL,db=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lang}{
Language for character results
}
  \item{atc}{
A vector of ATC codes for which to retrieve informations
}
  \item{db}{
A vector of DrugBank codes for which to retrieve informations
}
}
\value{
data table of drug terms and their annotation : drug, atc code,  drug bank identifier,  abstract,   comment, label. When querying the whole dataset, the data table is of size 8712 rows by 6 columns.
}
\references{
Lehmann, J. et al. (2015). Dbpedia - a large-scale, multilingual knowledge base extracted from wikipedia. Semantic Web, 6, 167–195.
}
\author{
Y. Rivault
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See Also as \code{\link[pkg:queryMed]{dbpedi_disease}}
}
\examples{
\dontrun{
drug_dbp <- dbpedia_drug(lang="en")
head(drug_dbp)
}
}
