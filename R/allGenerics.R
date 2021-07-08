#' Constructor method of ISA-class.
#'
#' When creating a new object of class \linkS4class{ISA} via:
#' \code{object <- new(Class = "ISA", path)}, the function
#' \code{initialize(.Object, path)} is called to initialize and create the
#' actual object. The \code{initialize-method} is seldomly used as a function
#' itself.
#'
#' @param .Object character, name of the object of class \linkS4class{ISA} to
#' be initialized
#' @param path length-one character vector containing the path to the ISA-Tab
#' files of the dataset.
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
    .Object@path <- path
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
      .Object@iFileName <- iFileName
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
                        blank.lines.skip = TRUE,
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
    .Object@oSR <- createISASlotDataFrame(
      file = iFile,
      startRow = OSRStart,
      endRow = IStart)
    ## Investigation section
    ## Create an INVESTIGATION data.frame
    .Object@invest <- createISASlotDataFrame(
      file = iFile,
      startRow = IStart,
      endRow = IPubsStart)
    ## Create an INVESTIGATION PUBLICATIONS data.frame
    .Object@iPubs <- createISASlotDataFrame(
      file = iFile,
      startRow = IPubsStart,
      endRow = ICStart)
    ## Create an INVESTIGATION CONTACTS data.frame
    .Object@iContacts <- createISASlotDataFrame(
      file = iFile,
      startRow = ICStart,
      endRow = SStart[1])
    ## Study section
    ## Create a STUDY list of data.frames (one per study)
    for (i in seq_along(SStart)) {
      .Object@study[[i]] <- createISASlotDataFrame(
        file = iFile,
        startRow = SStart[i],
        endRow = SDDStart[i])
      names(.Object@study)[i] <- .Object@study[[i]][[ISASyntax$sidentifier]]
    }
    ## Create a STUDY DESIGN DESCRIPTORS list of data.frames (one per study)
    for (i in seq_along(SStart)) {
      .Object@sDD[[i]] <- createISASlotDataFrame(
        file = iFile,
        startRow = SDDStart[i],
        endRow = SPubsStart[i])
      names(.Object@sDD)[i] <- names(.Object@study)[i]
    }
    ## Create a STUDY PUBLICATIONS list of data.frames (one per study)
    for (i in seq_along(SStart)) {
      .Object@sPubs[[i]] <- createISASlotDataFrame(
        file = iFile,
        startRow = SPubsStart[i],
        endRow = SFStart[i])
      names(.Object@sPubs)[i] <- names(.Object@study)[i]
    }
    ## Create a STUDY FACTORS list of data.frames (one per study)
    for (i in seq_along(SStart)) {
      .Object@sFacts[[i]] <- createISASlotDataFrame(
        file = iFile,
        startRow = SFStart[i],
        endRow = SAStart[i])
      names(.Object@sFacts)[i] <- names(.Object@study)[i]
    }
    ## Create a STUDY ASSAYS list of data.frames (one per study)
    for (i in seq_along(SStart)) {
      .Object@sAssays[[i]] <- createISASlotDataFrame(
        file = iFile,
        startRow = SAStart[i],
        endRow = SPrStart[i])
      names(.Object@sAssays)[i] <- names(.Object@study)[i]
    }
    ## Create a STUDY PROTOCOLS list of data.frames (one per study)
    for (i in seq_along(SStart)) {
      .Object@sProts[[i]] <- createISASlotDataFrame(
        file = iFile,
        startRow = SPrStart[i],
        endRow = SCStart[i])
      names(.Object@sProts)[i] <- names(.Object@study)[i]
    }
    ## Create a STUDY CONTACTS list of data.frames (one per study)
    for (i in seq_along(SStart)) {
      if (i < length(SStart)) {
        .Object@sContacts[[i]] <- createISASlotDataFrame(
          file = iFile,
          startRow = SCStart[i],
          endRow = SStart[i + 1])
        names(.Object@sContacts)[i] <- names(.Object@study)[i]
      } else {# i == length(SStart)
        .Object@sContacts[[i]] <- createISASlotDataFrame(
          file = iFile,
          startRow = SCStart[i],
          endRow = (iFileRowEnd + 1))
        names(.Object@sContacts)[i] <- names(.Object@study)[i]
      }
    }
    ## END INVESTIGATION FILE ----

    ## START STUDY FILES ----
    sFileNames <- getStudyFileNames(isaObject = .Object)
    for (i in seq_along(sFileNames)) {
      sFilePath <- file.path(.Object@path, sFileNames[i])
      tempdf <- read.delim(file = sFilePath,
                           header = TRUE,
                           sep = "\t",
                           blank.lines.skip = TRUE,
                           check.names = FALSE,
                           stringsAsFactors = FALSE)
      ## Remove empty rows.
      tempdf <- tempdf[apply(tempdf, 1, function(x) any(nzchar(x))), ]
      .Object@sFiles[[i]] <- unique(tempdf)
      names(.Object@sFiles)[i] <- sFileNames[i]
    }
    ## END STUDY FILES ----

    ## START ASSAY FILES ----
    aFileNames <- unlist(getAssayFileNames(isaObject = .Object))
    for (i in seq_along(aFileNames)) {
      aFilePath <- file.path(.Object@path, aFileNames[i])
      tempdf <- read.delim(file = aFilePath,
                           header = TRUE,
                           sep = "\t",
                           blank.lines.skip = TRUE,
                           check.names = FALSE,
                           stringsAsFactors = FALSE)
      ## Remove empty rows.
      tempdf <- tempdf[apply(tempdf, 1, function(x) any(nzchar(x))), ]
      colNames <- colnames(tempdf)
      ## Remove empty columns.
      tempdf <- tempdf[, colnames(tempdf) != "NA"]
      colnames(tempdf) <- colNames[!colNames == "NA"]
      .Object@aFiles[[i]] <- tempdf
      names(.Object@aFiles)[i] <- aFileNames[i]
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


### sPubs standard generics.

#' Get and set sPubs.
#'
#' Get and set the list of sPubs data.frame in an object of
#' \code{\link{ISA-class}}.
#'
#' @param x An object of the \code{\link{ISA-class}}.
#'
#' @return A list of data.frames containing the study publications.
#' @rdname sPubs
#' @export
setGeneric("sPubs", function(x) standardGeneric("sPubs"))

#' @param x An object of the \code{\link{ISA-class}}.
#' @param value A list of data.frames containing the study publications.
#' In each data.frame at least the following columns are present:
#' `r paste0("``", sPubsCols, "``", collapse = ", ")`.
#'
#' @return The updated object of the \code{\link{ISA-class}}.
#' @rdname sPubs
#' @export
setGeneric("sPubs<-", function(x, value) standardGeneric("sPubs<-"))


### sFacts standard generics.

#' Get and set sFacts.
#'
#' Get and set the list of sFacts data.frame in an object of
#' \code{\link{ISA-class}}.
#'
#' @param x An object of the \code{\link{ISA-class}}.
#'
#' @return A list of data.frames containing the study factors.
#' @rdname sFacts
#' @export
setGeneric("sFacts", function(x) standardGeneric("sFacts"))

#' @param x An object of the \code{\link{ISA-class}}.
#' @param value A list of data.frames containing the study factors.
#' In each data.frame at least the following columns are present:
#' `r paste0("``", sFactsCols, "``", collapse = ", ")`.
#'
#' @return The updated object of the \code{\link{ISA-class}}.
#' @rdname sFacts
#' @export
setGeneric("sFacts<-", function(x, value) standardGeneric("sFacts<-"))


### sAssays standard generics.

#' Get and set sAssays.
#'
#' Get and set the list of sAssays data.frame in an object of
#' \code{\link{ISA-class}}.
#'
#' @param x An object of the \code{\link{ISA-class}}.
#'
#' @return A list of data.frames containing the study assays.
#' @rdname sAssays
#' @export
setGeneric("sAssays", function(x) standardGeneric("sAssays"))

#' @param x An object of the \code{\link{ISA-class}}.
#' @param value A list of data.frames containing the study assays.
#' In each data.frame at least the following columns are present:
#' `r paste0("``", sAssaysCols, "``", collapse = ", ")`.
#'
#' @return The updated object of the \code{\link{ISA-class}}.
#' @rdname sAssays
#' @export
setGeneric("sAssays<-", function(x, value) standardGeneric("sAssays<-"))


### sProts standard generics.

#' Get and set sProts.
#'
#' Get and set the list of sProts data.frame in an object of
#' \code{\link{ISA-class}}.
#'
#' @param x An object of the \code{\link{ISA-class}}.
#'
#' @return A list of data.frames containing the study protocols.
#' @rdname sProts
#' @export
setGeneric("sProts", function(x) standardGeneric("sProts"))

#' @param x An object of the \code{\link{ISA-class}}.
#' @param value A list of data.frames containing the study protocols.
#' In each data.frame at least the following columns are present:
#' `r paste0("``", sProtsCols, "``", collapse = ", ")`.
#'
#' @return The updated object of the \code{\link{ISA-class}}.
#' @rdname sProts
#' @export
setGeneric("sProts<-", function(x, value) standardGeneric("sProts<-"))


### sContacts standard generics.

#' Get and set sContacts.
#'
#' Get and set the list of sContacts data.frame in an object of
#' \code{\link{ISA-class}}.
#'
#' @param x An object of the \code{\link{ISA-class}}.
#'
#' @return A list of data.frames containing the study contacts.
#' @rdname sContacts
#' @export
setGeneric("sContacts", function(x) standardGeneric("sContacts"))

#' @param x An object of the \code{\link{ISA-class}}.
#' @param value A list of data.frames containing the study contacts.
#' In each data.frame at least the following columns are present:
#' `r paste0("``", sContactsCols, "``", collapse = ", ")`.
#'
#' @return The updated object of the \code{\link{ISA-class}}.
#' @rdname sContacts
#' @export
setGeneric("sContacts<-", function(x, value) standardGeneric("sContacts<-"))



### sFiles standard generics.

#' Get and set sFiles.
#'
#' Get and set the list of sFiles data.frame in an object of
#' \code{\link{ISA-class}}.
#'
#' @param x An object of the \code{\link{ISA-class}}.
#'
#' @return A list of data.frames containing the study files.
#' @rdname sFiles
#' @export
setGeneric("sFiles", function(x) standardGeneric("sFiles"))

#' @param x An object of the \code{\link{ISA-class}}.
#' @param value A list of data.frames containing the study files.
#' In each data.frame at least the following columns are present:
#' `r paste0("``", sFilesCols, "``", collapse = ", ")`.
#'
#' @return The updated object of the \code{\link{ISA-class}}.
#' @rdname sFiles
#' @export
setGeneric("sFiles<-", function(x, value) standardGeneric("sFiles<-"))


### aFiles standard generics.

#' Get and set aFiles.
#'
#' Get and set the list of aFiles data.frame in an object of
#' \code{\link{ISA-class}}.
#'
#' @param x An object of the \code{\link{ISA-class}}.
#'
#' @return A list of data.frames containing the assay files.
#' @rdname aFiles
#' @export
setGeneric("aFiles", function(x) standardGeneric("aFiles"))

#' @param x An object of the \code{\link{ISA-class}}.
#' @param value A list of data.frames containing the assay files.
#' In each data.frame at least the following columns are present:
#' `r paste0("``", aFilesCols, "``", collapse = ", ")`.
#'
#' @return The updated object of the \code{\link{ISA-class}}.
#' @rdname aFiles
#' @export
setGeneric("aFiles<-", function(x, value) standardGeneric("aFiles<-"))

