\name{mapping_cui}
\alias{mapping_cui}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Mapping medical nomenclatures to CUI
}
\description{
The function allows mapping from concept unique identifier (cui - UMLS) to medical nomenclatures available through Biopotal, and vice versa.

}
\usage{
Mapping(codes=NULL,ontologies="",api_key="")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{codes}{
Codes the user want to map. It can be cui's codes or any terminology codes available on Bioportal.
}
  \item{ontologies}{
A set of ontologies where to find mappings if codes are cui. Or a set of ontologies which codes belongs if they are not cui.
}
  \item{api_key}{
An API Key is required to access any API call on Bioportal and SIFR. To have one, you need to register at http://bioportal.bioontologies.org/, respectively to http://bioportal.lirmm.fr/ for SIFR.
}
}
%- maybe also 'usage' for other objects documented here.
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

}