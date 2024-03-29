% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/allGenerics.R, R/setInfo.R
\name{oSR}
\alias{oSR}
\alias{oSR<-}
\alias{oSR,ISA-method}
\alias{oSR<-,ISA-method}
\title{Get and set oSR}
\usage{
oSR(x)

oSR(x) <- value

\S4method{oSR}{ISA}(x)

\S4method{oSR}{ISA}(x) <- value
}
\arguments{
\item{x}{An object of class \linkS4class{ISA}.}

\item{value}{A data.frame containing the ontology Source Reference
information. data.frame in which at least the following columns are present:
'Term Source Name', 'Term Source File', 'Term Source Version', 'Term Source Description'.}
}
\value{
A data.frame containing the ontology Source Reference information.

The updated object of class \linkS4class{ISA}.
}
\description{
Get and set the ontology Source Reference (oSR) data.frame in an object of
\linkS4class{ISA}.
}
\examples{
## Read example Atwell data set.
isaObject1 <- readISATab(path = file.path(system.file("extdata/Atwell",
                                          package = "isatabr")))

## Extract ontology Source Reference data.frame.
oSR(isaObject1)

}
