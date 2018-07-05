\name{bio2rdf_db}
\alias{bio2rdf_db}

\title{
Retrieve drug informations from bio2rdf
}
\description{
Retrieve drug informations from bio2rdf SPARQL endpoint such as label, description, category, etc.
}
\usage{
bio2rdf_db(lang="en",atc=NULL,db=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lang}{
  Character vector to specify the language of the results returned 
}
  \item{atc}{
A vector of atc codes for which to retrieve informations
}
  \item{db}{
A vector of db codes for which to retrieve informations
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
}
\references{
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
bio2rdf <- bio2rdf_db(lang="en")
head(bio2rdf)
}
}
