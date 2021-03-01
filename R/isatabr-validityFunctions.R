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
#' @seealso
#' \linkS4class{ISA}, \code{\link{ISA-class}},
#' \code{\link{initialize,ISA-method}}
#'
#' @examples
#' \dontrun{
#'
#' a <- new("ISA")
#' print(a)
#' a <- initialize(a,
#'                 path = "~/Desktop/ISA-Tab_example/WUR/")
#' validISAObject(a)
#' print(a)
#' }
#'
#' @export
validISAObject <- function(object) {
  noIFilenames <- length(object[ISASyntax$iFileName])
  if (noIFilenames == 0) {
    stop("Did not find any investigation file at folder ", object["path"])
  } else if (noIFilenames > 1) {
    stop("Found too many possible investigation files: ",
         paste(object["Investigation Filename"], collapse = ", "))
  } else {# noIFilenames == 1
    return(TRUE)
  }
}
setValidity(Class = "ISA", method = validISAObject)
