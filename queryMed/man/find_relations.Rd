\name{find_relations}
\alias{find_relations}

\title{
Find relation between medical terms and pairs of known biomedical relations listed in Knowledge database
}
\description{
Function to find intersection between medical terms (disease, drugs...) and pairs defining a biomedical relationship like interactions, contraindications and indications through database .
}
\usage{
find_relations(data.x,data_indices,data.y=NULL,data_elements.x=NULL,data_elements.y=NULL,target,target_elements,progress="none")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data.x}{
  Data frame with one column of identifier and one column of medical terms
  }
  \item{data_indices}{
  column names of data.x that contains identifiers.
  }
  \item{data_elements.x}{
  column names of data.x that contains medical term of interest.
  }
  \item{data.y}{
  Data frame ....
  }
  \item{data_elements.y}{
  column names of data.x that contains medical term of interest.
  }
  \item{target}{
  Knowledge database (e.g. DIKB, DID, NDF-RT, etc)
  }
  \item{target_elements}{
  Column names where to look for relations (in pairs).
  }
  \item{progress}{
  Whether or not to display progress bar. Default is set to "none"
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
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
# Do patients have drug interaction ?
data(drug_set)
data(DIKB)
interactions <- find_relations(data.x=drug_set,
                               data_indices = "patient",
                               data_elements.x = "drugs",
                               target=DIKB, target_elements = c("atc1","atc2"), 
                               progress="none")

interactions[[1]]
}

\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
