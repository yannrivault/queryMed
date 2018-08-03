\name{mapping_cui}
\alias{mapping_cui}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Mapping between medical nomenclatures using CUI
}
\description{
  The function allows mapping codes from a medical terminology to another, using the Concept Unique Identifier (CUI) from the Unified Medical Language System (UMLS). \emph{mapping_cui()} uses \emph{codes2cui()} function, which programmatically access to BioPortal API to search for a potential mapping. Thus it needs a BioPortal API key. To have one, you need to register at \href{http://bioportal.bioontology.org/}{BioPortal}.
}
\usage{
  mapping_cui(codes=NULL,ontologies_source="",ontologies_target="",api_key="")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{codes}{
    Codes from source ontologies (or nomenclatures) the user want to mapp to target ontologies (or nomenclatures).
  }
    \item{ontologies_source}{
    A set of ontologies where the codes come from. Can be void.
    }
    \item{ontologies_source}{
    A set of ontologies to search for mapping. If void, the mapping will be searched in all available ontologies in \href{http://bioportal.bioontology.org/}{BioPortal}.
    }
    \item{api_key}{
    An API Key is required to access any API call on BioPortal. To have one, you need to register at \href{http://bioportal.bioontology.org/}{BioPortal}.
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
    \dontrun{
    data(drug_set)
    drug_ATC_NDFRT <- mapping_cui(drug_set$ATC,
                              ontologies_source="ATC",
                              ontologies_target="NDFRT",
                              api_key="your_api_key")
    head(drug_ATC_NDFRT)
    }
    }