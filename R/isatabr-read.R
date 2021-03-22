#' @title
#' Read an ISA-Tab dataset into an R object.
#'
#' @description
#' Reads an ISA-Tab dataset, given as a zip file or as a set of files in a
#' specific folder, and builds an object of the \code{\link{ISA-class}}.
#'
#' @param path A character vector with the name of the directory in which the
#'             ISA-Tab files are located (if the parameter zipfile is not
#'             provided or if it is equal to NULL), or the name of the
#'             directory where the zip archive containing the ISA-Tab files is
#'             located (if the parameter zipfile is not NULL). The default
#'             value is the current working directory.
#' @param zipfile A character vector with the name of the zip archive containing
#'                ISA-Ttab files themselves (without a directory name in the zip
#'                archive). The default value is NULL (specifying that the ISA-
#'                Tab files have not been archived in one zipped file).
#' @param verbose A logical vector indicating to show messages for the different
#'                steps, if TRUE, or not to show them, if FALSE (the default
#'                value).
#'
#' @return An object of the \linkS4class{ISA} class.
#'
#' @author
#' Maikel Verouden
#'
#' @examples
#' \dontrun{
#' ## Example for mass spectrometry dataset
#' temp <- tempdir()
#' datafiles <- c(file.path(system.file("cdf/KO", package = "faahKO"),
#'                          grep(pattern = "CDF",
#'                               x = dir(system.file("cdf/KO", package = "faahKO")),
#'                               ignore.case = TRUE,
#'                               value = TRUE)),
#'                file.path(system.file("cdf/WT", package = "faahKO"),
#'                          grep(pattern = "CDF",
#'                               x = dir(system.file("cdf/WT", package = "faahKO")),
#'                               value = TRUE)))
#' file.copy(datafiles, temp, recursive = TRUE)
#' isafiles <- file.path(system.file(package = "faahKO"),
#'                       grep(pattern = "txt",
#'                            x = dir(system.file(package = "faahKO")),
#'                            value = TRUE))
#' file.copy(isafiles, temp, recursive = TRUE)
#' isaObject1 <- readISATab(path = temp)
#'
#' ## Example of readISATab for a mass spectrometry experiment
#' isazip <- "faahKO-metadata.zip"
#' isaObject2 <- readISATab(path = file.path(system.file("extdata",
#'                                                       package = "Risa")),
#'                          zipfile = isazip,
#'                          verbose = TRUE)
#' }
#'
#' @export
readISATab <- function(path = getwd(),
                       zipfile = NULL,
                       verbose = FALSE) {
  checkCharacter(path)
  ## Run file.path to assure trailing / are removed/added when needed.
  path <- file.path(path)
  if (!file.exists(path)) {
    stop(path, " is not an existing folder on this system!")
  }
  if (!is.null(zipfile)) {
    checkCharacter(zipfile)
    readZippedISATabFiles(path = path, zipfile = zipfile, verbose = verbose)
  } else {
    readISATabFiles(path = path, verbose = verbose)
  }
}

### This function only works if the zip file does not contain a directory name
### (but the ISA-Tab files themselves)
readZippedISATabFiles <- function(path = getwd(),
                                  zipfile,
                                  verbose = FALSE) {
  tmpdir <- normalizePath(tempdir())
  if (verbose) {
    message("Unzipping file in temporary directory: ", tmpdir)
  }
  unzippedISATabFiles <-
    utils::unzip(zipfile = normalizePath(file.path(path, zipfile)),
                 exdir = tmpdir)
  if (verbose) {
    message("Unzipped files: ", paste(gsub(pattern = file.path(tmpdir, "/"),
                                           replacement = "",
                                           x = unzippedISATabFiles),
                                      collapse = ", "))
  }
  isaobj <- readISATabFiles(path = tmpdir, verbose = verbose)
  return(isaobj)
}

readISATabFiles <- function(path = getwd(),
                            verbose = FALSE) {
  if (verbose) {
    message("Converting ISA-Tab dataset at ",
            normalizePath(path),
            " into an R object...")
  }
  isaobject <- new(Class = "ISA", path = normalizePath(path))
  if (verbose) {
    message("... done.")
  }
  return(isaobject)
}
