\name{dbpedia_disease}
\alias{dbpedia_disease}

\title{
Retrieve disease information from dbpedia
}
\description{
Retrieve disease information from dbpedia such as ICD10-ICD9 matching, label, synonyms etc.
}
\usage{
dbpedia_disease(lang="en")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lang}{
  Character vector to specify the language of the results returned 
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
data table of disease terms and their annotation : disease name, icd10, icd9,   abstract, comment, label, synonyms. When querying the whole dataset, the data table is of size 5523 rows by 6 columns.
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
disease_dbp <- dbpedia_disease(lang="en")
head(disease_dbp)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
