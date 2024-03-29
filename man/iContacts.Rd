% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/allGenerics.R, R/setInfo.R
\name{iContacts}
\alias{iContacts}
\alias{iContacts<-}
\alias{iContacts,ISA-method}
\alias{iContacts<-,ISA-method}
\title{Get and set iContacts.}
\usage{
iContacts(x)

iContacts(x) <- value

\S4method{iContacts}{ISA}(x)

\S4method{iContacts}{ISA}(x) <- value
}
\arguments{
\item{x}{An object of class \linkS4class{ISA}.}

\item{value}{A data.frame containing the investigation contacts
information. A data.frame in which at least the following columns are
present:
'Investigation Person Last Name', 'Investigation Person First Name', 'Investigation Person Mid Initials', 'Investigation Person Email', 'Investigation Person Phone', 'Investigation Person Fax', 'Investigation Person Address', 'Investigation Person Affiliation', 'Investigation Person Roles', 'Investigation Person Roles Term Accession Number', 'Investigation Person Roles Term Source REF'.}
}
\value{
A data.frame containing the investigation contacts information.

The updated object of class \linkS4class{ISA}.
}
\description{
Get and set the iContacts data.frame in an object of
\linkS4class{ISA}.
}
\examples{
## Read example Atwell data set.
isaObject1 <- readISATab(path = file.path(system.file("extdata/Atwell",
                                          package = "isatabr")))

## Extract iContacts data.frame.
iContacts(isaObject1)

}
