\name{sparql}
\alias{sparql}
\title{
Querying sparql endpoints
}
\description{
Function that sends queries on sparql endpoints. It is an alternative to the SPARQL() from the SPARQL package, by allowing retrieving JSON.
}
\usage{
sparql(query = "", url = "", api_key = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{query}{
The SPARQL query.
}
  \item{url}{
url of the sparql endpoint.
}
  \item{api_key}{
An api key if needed.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Data table of results generated by the data returned from the SPARQL query
}
\references{
Willem Robert Van Hage et al. SPARQL: SPARQL client, 2013. R package version 1.16.
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
# Querying SPARQL endpoints :

# Retrieving some informations about drugs that are available through db-pedia:

  # Drugs and their labels:
query="SELECT DISTINCT * 
       WHERE {
        ?drug rdf:type dbo:Drug .
        ?drug rdfs:label ?label .
        }"
results <- sparql(query,url="https://dbpedia.org/sparql/")
results[1:2,]

  # Drugs and their drugbank Id:
query="SELECT DISTINCT * 
       WHERE {
        ?drug rdf:type dbo:Drug .
        ?drug dbp:drugbank ?db_Id .
        }"
results <- sparql(query,url="https://dbpedia.org/sparql/")
results

  # Drugs and their ATC Id:
query="SELECT DISTINCT ?drug concat(str(?prefix),str(?suffix)) as ?atc 
       WHERE {
        ?drug dbo:atcPrefix ?prefix .
        ?drug dbo:atcSuffix ?suffix .
        }"
results <- sparql(query,url="https://dbpedia.org/sparql/")
results

# Retrieving some informations about drugs that are available through bio2rdf:

query="SELECT DISTINCT *
       WHERE {
        ?drug <http://bio2rdf.org/kegg_vocabulary:formula> ?formula.
        }"
results <- sparql(query,url="http://bio2rdf.org/sparql")
results
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
