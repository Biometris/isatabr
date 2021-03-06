% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validity.R
\name{validISAObject}
\alias{validISAObject}
\title{Check the validity of an object of class ISA.}
\usage{
validISAObject(object)
}
\arguments{
\item{object}{An object of class \linkS4class{ISA}.}
}
\value{
TRUE or an error message.
}
\description{
The \code{validISAObject} function checks whether an object of class
\linkS4class{ISA} is a valid object. An object of the \linkS4class{ISA} is
considered valid when:
\itemize{
\item There is only one investigation file, which name starts with \strong{i_}
and ends with \strong{.txt}, present in the folder containing the ISA-Tab files.
\item All sections in the investigation file contain at least the columns
specified for that specific section at
https://isa-specs.readthedocs.io/en/latest/isatab.html
\item The study and assay files contain at least the columns specified at
https://isa-specs.readthedocs.io/en/latest/isatab.html
}
}
\examples{
## Read example Atwell data set.
isaObject1 <- readISATab(path = file.path(system.file("extdata/Atwell",
                                          package = "isatabr")))

## Check validity of the object.
validISAObject(isaObject1)

}
\seealso{
\linkS4class{ISA}
}
