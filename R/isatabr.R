#' @title
#' isatabr package overview.
#'
#' @description
#' The isatabr package is an implementation of the ISA-Tab format for R. It
#' builds an R object from ISA-Tab format files.
#'
#' @details
#' ISA is a metadata framework to manage an increasingly diverse set of life
#' science, environmental and biomedical experiments that employ one or a
#' combination of technologies. Built around the \strong{Investigation} (the
#' project context), \strong{Study} (a unit of research) and \strong{Assay}
#' (analytical measurements) concepts, ISA helps you to provide rich
#' descriptions of experimental metadata (i.e. sample characteristics,
#' technology and measurement types, sample-to-data relationships) so that the
#' resulting data and discoveries are reproducible and reusable.
#'
#' The ISA Abstract Model has been implemented in two format specifications,
#' ISA-Tab and ISA-JSON. In this package the former, ISA-Tab, is being used.
#' ISA-Tab files are tab separated values files stored with a
#' \strong{\emph{.txt}} extension.
#'
#' @references
#' \itemize{
#'   \item \href{https://isa-tools.org/}{ISA framework}
#'   \item \href{https://isa-specs.readthedocs.io/en/stable/}{ISA Model and
#'   Serialization Specifications}
#'   \item \href{https://www.isacommons.org}{ISA users community}
#' }
#'
#' @import methods
#' @importFrom utils count.fields read.table write.table
#'
#' @docType package
#' @name isatabr-package
NULL
