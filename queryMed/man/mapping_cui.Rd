\name{mapping_cui}
\alias{mapping_cui}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
mapping_cui
}
\description{
Mapping from concept unique identifier (cui UMLS) to medical nomenclatures available through Biopotal, and vice versa.

}
\usage{
Mapping(codes=NULL,ontologies="",api_key="",progress=T)
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
 \item{progress}{
A boolean to display or not a progress bar.
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

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
