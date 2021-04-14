### Constructor methods ----
#' @title
#' Constructor method of ISA-class.
#'
#' @description
#' When creating a new object of class \linkS4class{ISA} via:
#' \code{object <- new(Class = "ISA", path)}, the function
#' \code{initialize(.Object, path)} is called to initialize and create the
#' actual object. The \code{initialize-method} is seldomly used as a function
#' itself.
#'
#' @param .Object character, name of the object of class \linkS4class{ISA} to
#'                be initialized
#' @param path length-one character vector containing the path to the ISA-Tab
#'             files of the dataset.
#'
#' @rdname ISA-class
#' @aliases ISA-initialize
#' @importFrom utils read.table read.delim
#' @export
setMethod(
  f = "initialize",
  signature = "ISA",
  definition = function(.Object, path) {
    ## Assignment of the "path" slot
    .Object[ISASyntax$path] <- path
    ## Read ISA-Tab files in specified path
    filenames <- list.files(path)
    ## START INVESTIGATION FILE ----
    ## Locate Investigation Filename(s)
    iFileName <- grep(pattern = paste0("^",
                                       ISASyntax$iPrefix,
                                       ".*[a-zA-Z0-9_-]",
                                       "(\\.txt)$"),
                      x = filenames,
                      value = TRUE,
                      perl = TRUE)
    ## Check the number of Investigation Files in the specified path
    if (length(iFileName) == 0) {
      stop("Did not find any investigation file in folder ",
           normalizePath(path))
    } else if (length(iFileName) > 1) {
      stop("Found too many possible investigation files: ",
           paste(iFileName, collapse = ", "))
    } else {# noIFileNames == 1
      ## Assignment of the "Investigation File Name" slot
      .Object[ISASyntax$iFileName] <- iFileName
    }
    ## Read in investigation file
    iNoCols <- max(count.fields(file = file.path(path, iFileName),
                                sep = "\t",
                                quote = "\"",
                                blank.lines.skip = TRUE,
                                comment.char = "#"),
                   na.rm = TRUE)
    iFile <- read.table(file = file.path(path, iFileName),
                        sep = "\t",
                        fill = TRUE,
                        na.strings = "",
                        comment.char = "#",
                        blank.lines.skip = TRUE ,
                        stringsAsFactors = FALSE,
                        col.names = paste0("V", seq_len(iNoCols)))
    iRownames <- iFile[, 1]
    ## row numbers where sections in the investigation file start
    OSRStart <- which(iRownames == ISASyntax$oSR)
    IStart <- which(iRownames == ISASyntax$invest)
    IPubsStart <- which(iRownames == ISASyntax$iPubs)
    ICStart <- which(iRownames == ISASyntax$iContacts)
    SStart <- which(iRownames == ISASyntax$study)
    SDDStart <- which(iRownames == ISASyntax$sDD)
    SPubsStart <- which(iRownames == ISASyntax$sPubs)
    SFStart <- which(iRownames == ISASyntax$sFacts)
    SAStart <- which(iRownames == ISASyntax$sAssays)
    SPrStart <- which(iRownames == ISASyntax$sProts)
    SCStart <- which(iRownames == ISASyntax$sContacts)
    iFileRowEnd <- length(iRownames)
    ## Ontology Source Reference section
    ## Create an ONTOLOGY SOURCE REFERENCE data.frame
    .Object[ISASyntax$oSR] <- createISASlotDataFrame(
      file = iFile,
      startRow = OSRStart,
      endRow = IStart)
    ## Investigation section
    ## Create an INVESTIGATION data.frame
    .Object[ISASyntax$invest] <- createISASlotDataFrame(
      file = iFile,
      startRow = IStart,
      endRow = IPubsStart)
    ## Create an INVESTIGATION PUBLICATIONS data.frame
    .Object[ISASyntax$iPubs] <- createISASlotDataFrame(
      file = iFile,
      startRow = IPubsStart,
      endRow = ICStart)
    ## Create an INVESTIGATION CONTACTS data.frame
    .Object[ISASyntax$iContacts] <- createISASlotDataFrame(
      file = iFile,
      startRow = ICStart,
      endRow = SStart[1])
    ## Study section
    ## Create a STUDY list of data.frames (one per study)
    for (i in seq_along(SStart)) {
      .Object[ISASyntax$study][[i]] <- createISASlotDataFrame(
        file = iFile,
        startRow = SStart[i],
        endRow = SDDStart[i])
      names(.Object[ISASyntax$study])[i] <-
        .Object[ISASyntax$study][[i]][[ISASyntax$sidentifier]]
    }
    ## Create a STUDY DESIGN DESCRIPTORS list of data.frames (one per study)
    for (i in seq_along(SStart)) {
      .Object[ISASyntax$sDD][[i]] <- createISASlotDataFrame(
        file = iFile,
        startRow = SDDStart[i],
        endRow = SPubsStart[i])
      names(.Object[ISASyntax$sDD])[i] <- names(.Object[ISASyntax$study])[i]
    }
    ## Create a STUDY PUBLICATIONS list of data.frames (one per study)
    for (i in seq_along(SStart)) {
      .Object[ISASyntax$sPubs][[i]] <- createISASlotDataFrame(
        file = iFile,
        startRow = SPubsStart[i],
        endRow = SFStart[i])
      names(.Object[ISASyntax$sPubs])[i] <- names(.Object[ISASyntax$study])[i]
    }
    ## Create a STUDY FACTORS list of data.frames (one per study)
    for (i in seq_along(SStart)) {
      .Object[ISASyntax$sFacts][[i]] <- createISASlotDataFrame(
        file = iFile,
        startRow = SFStart[i],
        endRow = SAStart[i])
      names(.Object[ISASyntax$sFacts])[i] <- names(.Object[ISASyntax$study])[i]
    }
    ## Create a STUDY ASSAYS list of data.frames (one per study)
    for (i in seq_along(SStart)) {
      .Object[ISASyntax$sAssays][[i]] <- createISASlotDataFrame(
        file = iFile,
        startRow = SAStart[i],
        endRow = SPrStart[i])
      names(.Object[ISASyntax$sAssays])[i] <- names(.Object[ISASyntax$study])[i]
    }
    ## Create a STUDY PROTOCOLS list of data.frames (one per study)
    for (i in seq_along(SStart)) {
      .Object[ISASyntax$sProts][[i]] <- createISASlotDataFrame(
        file = iFile,
        startRow = SPrStart[i],
        endRow = SCStart[i])
      names(.Object[ISASyntax$sProts])[i] <- names(.Object[ISASyntax$study])[i]
    }
    ## Create a STUDY CONTACTS list of data.frames (one per study)
    for (i in seq_along(SStart)) {
      if (i < length(SStart)) {
        .Object[ISASyntax$sContacts][[i]] <- createISASlotDataFrame(
          file = iFile,
          startRow = SCStart[i],
          endRow = SStart[i + 1])
        names(.Object[ISASyntax$sContacts])[i] <-
          names(.Object[ISASyntax$study])[i]
      } else {# i == length(SStart)
        .Object[ISASyntax$sContacts][[i]] <- createISASlotDataFrame(
          file = iFile,
          startRow = SCStart[i],
          endRow = (iFileRowEnd + 1))
        names(.Object[ISASyntax$sContacts])[i] <-
          names(.Object[ISASyntax$study])[i]
      }
    }
    ## END INVESTIGATION FILE ----

    ## START STUDY FILES ----
    sFileName <- getStudyInfo(isaObject = .Object)
    for (i in seq_along(sFileName)) {
      sFilePath <- file.path(.Object["path"], sFileName[i])
      tempdf <- read.delim(file = sFilePath,
                           header = TRUE,
                           sep = "\t",
                           stringsAsFactors = FALSE)
      columnNames <- as.character(read.table(file = sFilePath,
                                             header = FALSE,
                                             sep = "\t",
                                             nrows = 1,
                                             stringsAsFactors = FALSE))
      colnames(tempdf) <- columnNames
      .Object[ISASyntax$sFiles][[i]] <- unique(tempdf)
      names(.Object[ISASyntax$sFiles])[i] <- sFileName[i]
    }
    ## END STUDY FILES ----

    ## START ASSAY FILES ----
    aFileName <- getAssayFileNames(isaObject = .Object)
    for (i in seq_along(aFileName)) {
      aFilePath <- file.path(.Object["path"], aFileName[i])
      tempdf <- read.delim(file = aFilePath,
                           header = TRUE,
                           sep = "\t",
                           stringsAsFactors = FALSE)
      columnNames <- as.character(read.table(file = aFilePath,
                                             header = FALSE,
                                             sep = "\t",
                                             nrows = 1,
                                             stringsAsFactors = FALSE))
      colnames(tempdf) <- columnNames
      .Object[ISASyntax$aFiles][[i]] <- tempdf
      names(.Object[ISASyntax$aFiles])[i] <- aFileName[i]
    }
    ## END ASSAY FILES ----
    return(.Object)
  }
)

### Path standard generics.

#' Get and set path
#'
#' Get and set the file path for an object of the \code{\link{ISA-class}}.
#'
#' @param x An object of the \code{\link{ISA-class}}.
#'
#' @return The path to the folder for the object of the \code{\link{ISA-class}}.
#' @rdname path
#' @export
setGeneric("path", function(x) standardGeneric("path"))

#' @param x An object of the \code{\link{ISA-class}}.
#' @param value A length-one character vector indicating the path to an
#' accessible directory on the system.
#'
#' @return The updated object of the \code{\link{ISA-class}}.
#' @rdname path
#' @export
setGeneric("path<-", function(x, value) standardGeneric("path<-"))

### iFileName standard generics.

#' Get and set iFileName
#'
#' Get and set the file name for the investigation file in an object of
#' \code{\link{ISA-class}}.
#'
#' @param x An object of the \code{\link{ISA-class}}.
#'
#' @return The file name of the investigation file.
#' @rdname iFileName
#' @export
setGeneric("iFileName", function(x) standardGeneric("iFileName"))

#' @param x An object of the \code{\link{ISA-class}}.
#' @param value A length-one character vector indicating the name of the
#' investigation file, a string starting with "i_" and ending in ".txt"
#'
#' @return The updated object of the \code{\link{ISA-class}}.
#' @rdname iFileName
#' @export
setGeneric("iFileName<-", function(x, value) standardGeneric("iFileName<-"))

### oSR standard generics.

#' Get and set oSR
#'
#' Get and set the ontology Source Reference (oSR) data.frame in an object of
#' \code{\link{ISA-class}}.
#'
#' @param x An object of the \code{\link{ISA-class}}.
#'
#' @return A data.frame containing the ontology Source Reference information.
#' @rdname oSR
#' @export
setGeneric("oSR", function(x) standardGeneric("oSR"))

#' @param x An object of the \code{\link{ISA-class}}.
#' @param value A data.frame containing the ontology Source Reference
#' information. data.frame in which at least the following columns are present:
#' `r paste0("``", oSRCols, "``", collapse = ", ")`.
#'
#' @return The updated object of the \code{\link{ISA-class}}.
#' @rdname oSR
#' @export
setGeneric("oSR<-", function(x, value) standardGeneric("oSR<-"))

### invest standard generics.

#' Get and set invest.
#'
#' Get and set the investigation data.frame in an object of
#' \code{\link{ISA-class}}.
#'
#' @param x An object of the \code{\link{ISA-class}}.
#'
#' @return A data.frame containing the investigation information.
#' @rdname invest
#' @export
setGeneric("invest", function(x) standardGeneric("invest"))

#' @param x An object of the \code{\link{ISA-class}}.
#' @param value A data.frame containing the investigation information. A
#' data.frame in which at least the following columns are present:
#' `r paste0("``", investCols, "``", collapse = ", ")`.
#'
#' @return The updated object of the \code{\link{ISA-class}}.
#' @rdname invest
#' @export
setGeneric("invest<-", function(x, value) standardGeneric("invest<-"))

### iPubs standard generics.

#' Get and set iPubs.
#'
#' Get and set the iPubs data.frame in an object of
#' \code{\link{ISA-class}}.
#'
#' @param x An object of the \code{\link{ISA-class}}.
#'
#' @return A data.frame containing the investigation publications information.
#' @rdname iPubs
#' @export
setGeneric("iPubs", function(x) standardGeneric("iPubs"))

#' @param x An object of the \code{\link{ISA-class}}.
#' @param value A data.frame containing the investigation publications
#' information. A data.frame in which at least the following columns are
#' present:
#' `r paste0("``", iPubsCols, "``", collapse = ", ")`.
#'
#' @return The updated object of the \code{\link{ISA-class}}.
#' @rdname iPubs
#' @export
setGeneric("iPubs<-", function(x, value) standardGeneric("iPubs<-"))

### iContacts standard generics.

#' Get and set iContacts.
#'
#' Get and set the iContacts data.frame in an object of
#' \code{\link{ISA-class}}.
#'
#' @param x An object of the \code{\link{ISA-class}}.
#'
#' @return A data.frame containing the investigation contacts information.
#' @rdname iContacts
#' @export
setGeneric("iContacts", function(x) standardGeneric("iContacts"))

#' @param x An object of the \code{\link{ISA-class}}.
#' @param value A data.frame containing the investigation contacts
#' information. A data.frame in which at least the following columns are
#' present:
#' `r paste0("``", iContactsCols, "``", collapse = ", ")`.
#'
#' @return The updated object of the \code{\link{ISA-class}}.
#' @rdname iContacts
#' @export
setGeneric("iContacts<-", function(x, value) standardGeneric("iContacts<-"))


### study standard generics.

#' Get and set study.
#'
#' Get and set the list of study data.frame in an object of
#' \code{\link{ISA-class}}.
#'
#' @param x An object of the \code{\link{ISA-class}}.
#'
#' @return A list of data.frames containing the study information.
#' @rdname study
#' @export
setGeneric("study", function(x) standardGeneric("study"))

#' @param x An object of the \code{\link{ISA-class}}.
#' @param value A list of data.frames containing the study information.
#' In each data.frame at least the following columns are present:
#' `r paste0("``", studyCols, "``", collapse = ", ")`.
#'
#' @return The updated object of the \code{\link{ISA-class}}.
#' @rdname study
#' @export
setGeneric("study<-", function(x, value) standardGeneric("study<-"))


### sDD standard generics.

#' Get and set sDD.
#'
#' Get and set the list of sDD data.frame in an object of
#' \code{\link{ISA-class}}.
#'
#' @param x An object of the \code{\link{ISA-class}}.
#'
#' @return A list of data.frames containing the study design descriptors.
#' @rdname sDD
#' @export
setGeneric("sDD", function(x) standardGeneric("sDD"))

#' @param x An object of the \code{\link{ISA-class}}.
#' @param value A list of data.frames containing the study design descriptors.
#' In each data.frame at least the following columns are present:
#' `r paste0("``", sDDCols, "``", collapse = ", ")`.
#'
#' @return The updated object of the \code{\link{ISA-class}}.
#' @rdname sDD
#' @export
setGeneric("sDD<-", function(x, value) standardGeneric("sDD<-"))











### Extract slots of an ISA-class object ----
#' @title Extract and replace methods for S4 ISA-class object
#'
#' @param x S4 object of class \linkS4class{ISA}.
#' @param i character, name of a slot to extract or replace.
#'
#' @details
#' \code{x[i]} defines a method to extract the information in the slot of an S4
#' object of class \\linkS4class{ISA} by using a character vector specifying the
#' name of the slot. Via \code{x[i] <- value} the value(s) in a slot of an S4
#' class object of class \linkS4class{ISA} can be replaced by means of
#' specifying the name of the slot as a character vector. Below in the slot
#' descriptions the character vectors to access specific slots are given.
#'
#' @docType methods
#' @rdname ISA-class
#' @aliases extractandreplace-methods
setMethod(
  f = "[",
  signature = c(x = "ISA", i = "character"),
  definition = function(x, i) {
    if (i == ISASyntax$path) return(x@path)
    if (i == ISASyntax$iFileName) return(x@iFileName)
    if (i == ISASyntax$oSR) return(x@oSR)
    if (i == ISASyntax$invest) return(x@invest)
    if (i == ISASyntax$iPubs) return(x@iPubs)
    if (i == ISASyntax$iContacts) return(x@iContacts)
    if (i == ISASyntax$study) return(x@study)
    if (i == ISASyntax$sDD) return(x@sDD)
    if (i == ISASyntax$sPubs) return(x@sPubs)
    if (i == ISASyntax$sFacts) return(x@sFacts)
    if (i == ISASyntax$sAssays) return(x@sAssays)
    if (i == ISASyntax$sProts) return(x@sProts)
    if (i == ISASyntax$sContacts) return(x@sContacts)
    if (i == ISASyntax$sFiles) return(x@sFiles)
    if (i == ISASyntax$aFiles) return(x@aFiles)
  }
)

### Replace slots of an ISA-class object ----
#' @param value ANY, replacement value for the slotname specified by \code{i}.
#'
#' @docType methods
#' @rdname ISA-class
#' @aliases extractandreplace-methods
setReplaceMethod(
  f = "[",
  signature = c(x = "ISA", i = "character", value = "ANY"),
  definition = function(x, i, value) {
    if (i == ISASyntax$path) {
      if (!inherits(value, "character")) {
        stop(paste0("The \"",
                    ISASyntax$path,
                    "\" slot of the ISA-class object can only be specified as ",
                    "a length-one vector of class \"character\"."))
      }
      if (!file.exists(value)) {
        stop(value, " is not an existing folder on this system!")
      } else {
        x@path <- normalizePath(value)
      }
    }
    if (i == ISASyntax$iFileName) {
      if (!inherits(value, "character")) {
        stop(paste0("The \"",
                    ISASyntax$iFileName,
                    "\" slot of the ISA-class object can only be specified as ",
                    "a length-one vector of class \"character\"."))
      }
      if (length(value) != 1) {
        stop(paste0("Please provide only a length-one vector of class ",
                    "\"character\" for the \"",
                    ISASyntax$iFileName,
                    "\" slot of the ISA-class object."))
      }
      if (!grepl(pattern = paste0("^",
                                  ISASyntax$iPrefix,
                                  ".*[a-zA-Z0-9_-]",
                                  "(\\.txt)$"),
                 x = value,
                 perl = TRUE)) {
        stop(paste0("The provided replacement value: \"",
                    value,
                    "\" for the \"",
                    ISASyntax$iFileName,
                    "\" slot does not match the requirements (start with ",
                    "\"i_\" and end with \".txt\")."))
      }
      if (!value %in% list.files(x[ISASyntax$path])) {
        stop(paste0("The \"",
                    ISASyntax$iFileName,
                    "\": \"",
                    value,
                    "\" is not present in the folder: ",
                    x[ISASyntax$path]))
      } else {
        x@iFileName <- value
      }
    }
    if (i == ISASyntax$oSR) {
      if (!inherits(value, "data.frame")) {
        stop(paste0("The \"",
                    ISASyntax$oSR,
                    "\" slot of the ISA-class object can only be of class ",
                    "\"data.frame.\""))
      } else {
        x@oSR <- value
      }
    }
    if (i == ISASyntax$invest) {
      if (!inherits(value, "data.frame")) {
        stop(paste0("The \"",
                    ISASyntax$invest,
                    "\" slot of the ISA-class object can only be of class ",
                    "\"data.frame\"."))
      } else {
        x@invest <- value
      }
    }
    if (i == ISASyntax$iPubs) {
      if (!inherits(value, "data.frame")) {
        stop(paste0("The \"",
                    ISASyntax$iPubs,
                    "\" slot of the ISA-class object can only be of class ",
                    "data.frame."))
      } else {
        x@iPubs <- value
      }
    }
    if (i == ISASyntax$iContacts) {
      if (!inherits(value, "data.frame")) {
        stop(paste0("The \"",
                    ISASyntax$iContacts,
                    "\" slot of the ISA-class object can only be of class ",
                    "data.frame."))
      } else {
        x@iContacts <- value
      }
    }
    if (i == ISASyntax$study) {
      if (!inherits(value, "list")) {
        stop(paste0("The \"",
                    ISASyntax$study,
                    "\" slot of the ISA-class object can only be of class ",
                    "list."))
      } else {
        x@study <- value
      }
    }
    if (i == ISASyntax$sDD) {
      if (!inherits(value, "list")) {
        stop(paste0("The \"",
                    ISASyntax$sDD,
                    "\" slot of the ISA-class object can only be of class ",
                    "list."))
      } else {
        x@sDD <- value
      }
    }
    if (i == ISASyntax$sPubs) {
      if (!inherits(value, "list")) {
        stop(paste0("The \"",
                    ISASyntax$sPubs,
                    "\" slot of the ISA-class object can only be of class ",
                    "list."))
      } else {
        x@sPubs <- value
      }
    }
    if (i == ISASyntax$sFacts) {
      if (!inherits(value, "list")) {
        stop(paste0("The \"",
                    ISASyntax$sFacts,
                    "\" slot of the ISA-class object can only be of class ",
                    "list."))
      } else {
        x@sFacts <- value
      }
    }
    if (i == ISASyntax$sAssays) {
      if (!inherits(value, "list")) {
        stop(paste0("The \"",
                    ISASyntax$sAssays,
                    "\" slot of the ISA-class object can only be of class ",
                    "list."))
      } else {
        x@sAssays <- value
      }
    }
    if (i == ISASyntax$sProts) {
      if (!inherits(value, "list")) {
        stop(paste0("The \"",
                    ISASyntax$sProts,
                    "\" slot of the ISA-class object can only be of class ",
                    "list."))
      } else {
        x@sProts <- value
      }
    }
    if (i == ISASyntax$sContacts) {
      if (!inherits(value, "list")) {
        stop(paste0("The \"",
                    ISASyntax$sContacts,
                    "\" slot of the ISA-class object can only be of class ",
                    "list."))
      } else {
        x@sContacts <- value
      }
    }
    if (i == ISASyntax$sFiles) {
      if (!inherits(value, "list")) {
        stop(paste0("The \"",
                    ISASyntax$sFiles,
                    "\" slot of the ISA-class object can only be of class ",
                    "list."))
      } else {
        x@sFiles <- value
      }
    }
    if (i == ISASyntax$aFiles) {
      if (!inherits(value, "list")) {
        stop(paste0("The \"",
                    ISASyntax$aFiles,
                    "\" slot of the ISA-class object can only be of class ",
                    "list."))
      } else {
        x@aFiles <- value
      }
    }
    return(x)
  }
)

