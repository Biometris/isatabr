#' @title
#' Check the validity of an object of class ISA.
#'
#' @description
#' The \code{validISAObject} function checks whether an object of class
#' \linkS4class{ISA} is a valid object (in this case TRUE is returned, otherwise
#' an error message will appear). An object of the \code{\link{ISA-class}} is
#' valid when:
#' \itemize{
#'  \item There is only one investigation file, which name starts with
#'        \strong{\emph{i_}} and ends with \strong{\emph{.txt}}, present in the
#'        folder containing the ISA-Tab files.
#' }
#'
#' @param object An object of class \linkS4class{ISA}.
#'
#' @return TRUE or an error message.
#'
#' @seealso \linkS4class{ISA}
#'
#' @examples
#' ## Example BII data set.
#' isaObject1 <- readISATab(path = file.path(system.file("extdata/BII-I-1",
#'                                           package = "isatabr")))
#' validISAObject(isaObject1)
#'
#' @export
validISAObject <- function(object) {
  ## Check that path point to an existing folder.
  path <- object["path"]
  if (!file.exists(path)) {
    stop(path, " is not an existing folder on this system!")
  } else {
    path <- normalizePath(path)
  }
  ## Check number of investigation files - should be 1.
  iFileName <- object[ISASyntax$iFileName]
  noIFilenames <- length(iFileName)
  if (noIFilenames == 0) {
    stop("Did not find any investigation file at folder ", path)
  } else if (noIFilenames > 1) {
    stop("Found too many possible investigation files: ",
         paste(object["Investigation Filename"], collapse = ", "))
  } else { # noIFilenames == 1
    return(TRUE)
  }
  ## Check structure of investigation file name.
  if (!grepl(pattern = paste0("^",
                              ISASyntax$iPrefix,
                              ".*[a-zA-Z0-9_-]",
                              "(\\.txt)$"),
             x = iFileName,
             perl = TRUE)) {
    stop(paste0("The investigation file: \"",
                iFileName,
                "\" for the \"",
                ISASyntax$iFileName,
                "\" slot does not match the requirements (start with ",
                "\"i_\" and end with \".txt\")."))
  }
  ## Check that investigation file is found in path.
  if (!iFileName %in% list.files(path)) {
    stop(paste0("The \"",
                ISASyntax$iFileName,
                "\": \"",
                iFileName,
                "\" is not present in the folder: ",
                path))
  }
}
setValidity(Class = "ISA",
            method = validISAObject)
