% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/uri2norm.R
\name{uri2norm}
\alias{uri2norm}
\title{Conversion of URI to variable}
\usage{
uri2norm(X)
}
\arguments{
\item{X}{One URI, a vector composed of URIs, a matrix of URIs or a dataframe of URIs.}
}
\value{
Character vector
}
\description{
Fonction that turn URI(s) (address type syntax) into computable variable
}
\examples{
 uri2norm("http://purl.bioontology.org/ontology/UATC/A05A")
 uri2norm(c("http://purl.bioontology.org/ontology/UATC/A05A",
 "http://purl.bioontology.org/ontology/UATC/A05A01"))
}
\author{
Y. Rivault
}
