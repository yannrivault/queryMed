\name{dbpedia_drug}
\alias{dbpedia_drug}

\title{
dbpedia_drug
}
\description{
Retrieve drug information from dbpedia such as atc and drug bank identifier codes
}
\usage{
dbpedia_drug(lang="en")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lang}{
Language for character results
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
\dontrun{
drug_dbp <- dbpedia_drug(lang="en")
head(drug_dbp)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
