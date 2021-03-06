\name{pddi_plot}
\alias{pddi_plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Potential drug-drug interaction graph
}
\description{
This function proposes a graph representation of potential drug-drug interaction as listed in DIKB data base.
}
\usage{
pddi_plot(drug="" , type="name", direction="object", nbinteractions= 1, source=NULL, contraindication= NULL, plot=TRUE, level=4, mypalette=NULL, weight=NULL)
}
\arguments{
  \item{drug}{
  character vector that specifies one drug name or one Drug Bank Identifier (https://www.drugbank.ca/) (DBI) or one Anatomical Therapeutic Chemical compound's code (http://www.who.int/classifications/atcddd/en/) (ATC)
}
  \item{type}{
 character vector that specifies whether drug is a drug name (default), DBI or ATC code.
}
  \item{direction}{
 character vector that specifies whether the queried drug is an object or an precipitant as defined by DIKB 
}
\item{nbinteractions}{
 numeric that states the minimun number of sources listing the interaction or the number of time the interactions have been listed by a source.
}
\item{source}{
 character vector that specifies the original source of the potential drug-drug interaction. It could be CredibleMeds, DDI-Corpus-2011, DDI-Corpus-2013, Drugbank, FrenchDB,  HEP,  HIV,   NDF-RT, NLM-Corpus, ONC-HighPriority, ONC-NonInteruptive, OSCAR,  PK-Corpus, World-Vista, Kegg (See https://github.com/dbmi-pitt/public-PDDI-analysis for more details). 
}
\item{contraindication}{
 If true filter on the contraindication listed in the Kegg data source.
}
\item{plot}{
 Logical whether or not to display the plot. If False the function return the data.frame used to build the plot.  
}
\item{level}{
 numeric ranging from 1 to 5 that indicates the ATC level to use to display the potential interacting drugs with the drug of interest.  
}
\item{mypalette}{
 character vector that specifies color of the nodes. If null the default is defined using brewer.pal and colorRampPalette from RBrewerColor and grDevices, respectively.
}
\item{weight}{
  numeric defining a threshold to filter out graph nodes and edges when the potential drug-drug interaction are listed less than  that threshold by different sources.
}
}
\details{
the graph is a star graph centered on the drug of interest. The color of the nodes depends on the ATC level specified in the function cal; grouping molecules by ATC classes. The thickness of the edges depends on the number of times the  potential drug-drug interaction is listed by different sources.
}
\value{
If the argument graph is set to TRUE a graph is plotted and the associated data.frame is returned. When the graph argument is set to FALSE anly the data.frame is returned.
}

\author{
N. Le Meur
}
\seealso{
See Also \code{\link[pkg:queryMed]{DIKB}} dataset and \code{\link{igraph}} package
}
\examples{
pddi_plot("CLOPIDOGREL", "name", level=4)
pddi_plot("CLOPIDOGREL", "name", level=5, nbinteractions=2)
}

\keyword{pddi}

