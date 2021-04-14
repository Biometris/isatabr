% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/allGenerics.R, R/setInfo.R
\name{sFiles}
\alias{sFiles}
\alias{sFiles<-}
\alias{sFiles,ISA-method}
\alias{sFiles<-,ISA-method}
\title{Get and set sFiles.}
\usage{
sFiles(x)

sFiles(x) <- value

\S4method{sFiles}{ISA}(x)

\S4method{sFiles}{ISA}(x) <- value
}
\arguments{
\item{x}{An object of the \code{\link{ISA-class}}.}

\item{value}{A list of data.frames containing the study files.
In each data.frame at least the following columns are present:
\verb{Source Name}, \verb{Term Source REF}, \verb{Sample Name}.}
}
\value{
A list of data.frames containing the study files.

The updated object of the \code{\link{ISA-class}}.
}
\description{
Get and set the list of sFiles data.frame in an object of
\code{\link{ISA-class}}.
}