### Get and set information for an ISA-class object

### Path.

#' @rdname path
setMethod("path", "ISA", function(x) x@path)

#' @rdname path
setMethod("path<-", "ISA", function(x, value) {
  ## nomalizePath is required to protect against system dependent paths,
  ## e.g. extra or missing /.
  x@path <- normalizePath(value)
  validISAObject(x)
  return(x)
})

### iFileName.

#' @rdname iFileName
setMethod("iFileName", "ISA", function(x) x@iFileName)

#' @rdname path
setMethod("iFileName<-", "ISA", function(x, value) {
  x@iFileName <- value
  validISAObject(x)
  return(x)
})

### oSR

#' @rdname oSR
setMethod("oSR", "ISA", function(x) x@oSR)

#' @rdname path
setMethod("oSR<-", "ISA", function(x, value) {
  x@oSR <- value
  validISAObject(x)
  return(x)
})

### invest

#' @rdname invest
setMethod("invest", "ISA", function(x) x@invest)

#' @rdname path
setMethod("invest<-", "ISA", function(x, value) {
  x@invest <- value
  validISAObject(x)
  return(x)
})


### iPubs

#' @rdname iPubs
setMethod("iPubs", "ISA", function(x) x@iPubs)

#' @rdname path
setMethod("iPubs<-", "ISA", function(x, value) {
  x@iPubs <- value
  validISAObject(x)
  return(x)
})


### iContacts

#' @rdname iContacts
setMethod("iContacts", "ISA", function(x) x@iContacts)

#' @rdname path
setMethod("iContacts<-", "ISA", function(x, value) {
  x@iContacts <- value
  validISAObject(x)
  return(x)
})



#' #' @title
#' #' Set the Investigation Identifier and Investigation File Name for an
#' #' ISA object.
#' #'
#' #' @description
#' #' Set for an object of the \code{\link{ISA-class}} the Investigation
#' #' Identifier and Investigation File Name.
#' #'
#' #' @param isaObject An object of the \code{\link{ISA-class}}.
#' #'
#' #' @return An object of the \code{\link{ISA-class}}.
#' #'
#' #' @family ISAinfo
#' #'
#' #' @export
#' getInvestigationInfo <- function(isaObject,
#'                                  ) {
#'   info <- isaObject[ISASyntax$iFileName]
#'   names(info) <- isaObject[ISASyntax$invest][[ISASyntax$iidentifier]]
#'   return(info)
#' }

#' #' @title
#' #' Retrieve the Study Identifier(s) and Study File Name(s) from an ISA object.
#' #'
#' #' @description
#' #' Retrieve from an object of the \code{\link{ISA-class}} the Study
#' #' Identifier(s) and Study File Name(s) as contained in the Investigation.
#' #' To directly access the Study Identifier(s) use the names() function, e.g.
#' #' names(getStudyInfo(isaObject)).
#' #'
#' #' @inheritParams getInvestigationInfo
#' #'
#' #' @return A named character vector containing the Study File Name(s) and the
#' #'         name(s) representing the Study Identifier(s).
#' #'
#' #' @author
#' #' Maikel Verouden
#' #'
#' #' @family ISAinfo
#' #'
#' #' @export
#' getStudyInfo <- function(isaObject) {
#'   sapply(X = isaObject[ISASyntax$study],
#'          FUN = function(x) {
#'            x[[ISASyntax$sFileName]]
#'          })
#' }
#'
#' #' @title
#' #' Retrieve the Assay File Name(s) per Study from an ISA object.
#' #'
#' #' @description
#' #' Retrieve from an object of the \code{\link{ISA-class}} the Assay File Name(s)
#' #' linked to the Study Identifier(s) per Study.
#' #'
#' #' @inheritParams getInvestigationInfo
#' #'
#' #' @return A named character vector of a list containing the Assay File Name(s)
#' #'         for each Study Identifier. The name of the character vector or names
#' #'         of the list elements represent(s) the Study Identifier(s).
#' #'
#' #' @author
#' #' Maikel Verouden
#' #'
#' #' @family ISAinfo
#' #'
#' #' @export
#' getAssayInfo <- function(isaObject) {
#'   lapply(X = isaObject[ISASyntax$sAssays],
#'          FUN = function(x) {
#'            x[[ISASyntax$aFileName]]
#'          })
#' }
#'
#' #' @title
#' #' Retrieve the Assay File Name(s) from an ISA object.
#' #'
#' #' @description
#' #' Retrieve from an object of the \code{\link{ISA-class}} the Assay File
#' #' Name(s).
#' #'
#' #' @inheritParams getInvestigationInfo
#' #'
#' #' @return An character vector containing the Assay File Name(s).
#' #'
#' #' @author
#' #' Maikel Verouden
#' #'
#' #' @family ISAinfo
#' #'
#' #' @export
#' getAssayFileNames <- function(isaObject) {
#'   unlist(lapply(X = names(getStudyInfo(isaObject)),
#'                 FUN = function(x) {getAssayInfo(isaObject)[[x]]}))
#' }
#'
#' #' @title
#' #' Retrieve the unique Assay Name(s) from the Assay Files in an ISA object.
#' #'
#' #' @description
#' #' Retrieve from an object of the \code{\link{ISA-class}} the Assay Name(s) per
#' #' Assay File.
#' #'
#' #' @inheritParams getInvestigationInfo
#' #'
#' #' @return A list of data frames containing the unique Assay Name(s) per Assay
#' #'         File.
#' #'
#' #' @author
#' #' Maikel Verouden
#' #'
#' #' @family ISAinfo
#' #'
#' #' @export
#' getAssayNames <- function(isaObject) {
#'   lapply(X = isaObject[ISASyntax$aFiles],
#'          FUN = function(i) {
#'            unique(i[grep(pattern = ISASyntax$assayName,
#'                          x = colnames(i))])
#'          })
#' }
#'
#' #' @title
#' #' Retrieve the Assay File(s) per Study from an ISA object.
#' #'
#' #' @description
#' #' Retrieve from an object of the \code{\link{ISA-class}} the Assay Files per
#' #' Study.
#' #'
#' #' @inheritParams getInvestigationInfo
#' #'
#' #' @return A list of Study Identifiers. Each element of the list, named by a
#' #'         Study Identifier, contains a list of data frames consisting of the
#' #'         Assay Files belonging to that specific study.
#' #'
#' #' @author
#' #' Maikel Verouden
#' #'
#' #' @family ISAinfo
#' #'
#' #' @export
#' getAssayFilesPerStudy <- function(isaObject) {
#'   aFilesPerStudy <-  lapply(X = getAssayInfo(isaObject),
#'                             FUN = function(x) {
#'                               lapply(X = x,
#'                                      FUN = function(x) {
#'                                        isaObject[ISASyntax$aFiles][[x]]})
#'                             })
#'   for (i in names(getStudyInfo(isaObject))) {
#'     names(aFilesPerStudy[[i]]) <- getAssayInfo(isaObject)[[i]]
#'   }
#'   return(aFilesPerStudy)
#' }
#'
#' #' @title
#' #' Retrieve the Study Assay Technology Types from  an ISA object.
#' #'
#' #' @description
#' #' Retrieve from an object of the \code{\link{ISA-class}} the Study Assay
#' #' Technology Type(s).
#' #'
#' #' @inheritParams getInvestigationInfo
#' #'
#' #' @return A character vector containing the Study Assay Technology Types.
#' #'
#' #' @author
#' #' Maikel Verouden
#' #'
#' #' @family ISAinfo
#' #'
#' #' @export
#' getAssayTechnologyTypes <- function(isaObject) {
#'   tempvec <- unlist(lapply(X = isaObject[ISASyntax$sAssays],
#'                            FUN = function(x) {
#'                              x[[ISASyntax$sAssayTechType]]
#'                            }))
#'   names(tempvec) <- NULL
#'   # ## Ordering of the Study Assay Technology Types by name
#'   # tempvec <- tempvec[order(tempvec)]
#'   return(tempvec)
#' }
#'
#' #' @title
#' #' Retrieve the Study Assay Technology Types per Study from an ISA object.
#' #'
#' #' @description
#' #' Retrieve from an object of the \code{\link{ISA-class}} the Study Assay
#' #' Technology Types per Study.
#' #'
#' #' @inheritParams getInvestigationInfo
#' #'
#' #' @return A list of Study Identifiers. Each element of the list contains a
#' #'         character vector specifying the Study Assay Technology Types
#' #'         belonging to that specific study (marked by its Study Identifier).
#' #'
#' #' @author
#' #' Maikel Verouden
#' #'
#' #' @family ISAinfo
#' #'
#' #' @export
#' getAssayTechnologyTypesPerStudy <- function(isaObject) {
#'   lapply(X = isaObject[ISASyntax$sAssays],
#'          FUN = function(x) {
#'            x[[ISASyntax$sAssayTechType]]
#'          })
#' }
#'
#' #' @title
#' #' Retrieve the unique Data File Names per Assay from an ISA object.
#' #'
#' #' @description
#' #' Retrieve from an object of the \code{\link{ISA-class}} the unique Data File
#' #' Names per Assay.
#' #'
#' #' @inheritParams getInvestigationInfo
#' #'
#' #' @return A list of data frames. Each element of the list, named by the Assay
#' #'         File name, contains the unique Data File Names as stored in the
#' #'         Assay File.
#' #'
#' #' @author
#' #' Maikel Verouden
#' #'
#' #' @family ISAinfo
#' #'
#' #' @export
#' getDataFileNamesPerAssay <- function(isaObject) {
#'   lapply(X = isaObject[ISASyntax$aFiles],
#'          FUN = function(df) {
#'            temp <- as.data.frame(df[, grep(pattern = ISASyntax$dataFile,
#'                                            x = colnames(df))])
#'            colnames(temp) <- colnames(df)[c(grep(pattern = ISASyntax$dataFile,
#'                                                  x = colnames(df)))]
#'            return(unique(temp))
#'          })
#' }
#'
#' #' @title
#' #' Retrieve the Study Assay Measurement Types from an ISA object.
#' #'
#' #' @description
#' #' Retrieve from an object of the \code{\link{ISA-class}} the Study Assay
#' #' Measurement Type(s).
#' #'
#' #' @inheritParams getInvestigationInfo
#' #'
#' #' @return A character vector containing the Study Assay Measurement Types.
#' #'
#' #' @author
#' #' Maikel Verouden
#' #'
#' #' @family ISAinfo
#' #'
#' #' @export
#' getAssayMeasurementTypes <- function(isaObject) {
#'   tempvec <- unlist(lapply(X = isaObject[ISASyntax$sAssays],
#'                            FUN = function(x) {
#'                              x[[ISASyntax$sAssayMeasType]]
#'                            }))
#'   names(tempvec) <- NULL
#'   # ## Ordering of the Study Assay Measurement Types by name
#'   # tempvec <- tempvec[order(tempvec)]
#'   return(tempvec)
#' }
#'
#' #' @title
#' #' Retrieve the Study Assay Measurement Types per Study from an ISA object.
#' #'
#' #' @description
#' #' Retrieve from an object of the \code{\link{ISA-class}} the Study Assay
#' #' Measurement Types per Study.
#' #'
#' #' @inheritParams getInvestigationInfo
#' #'
#' #' @return A list of Study Identifiers. Each element of the list, named by a
#' #'         Study Identifier, contains a character vector specifying the Study
#' #'         Assay Measurement Types belonging to a specific study (marked by its
#' #'         Study Identifier).
#' #'
#' #' @author
#' #' Maikel Verouden
#' #'
#' #' @family ISAinfo
#' #'
#' #' @export
#' getAssayMeasurementTypesPerStudy <- function(isaObject) {
#'   lapply(X = isaObject[ISASyntax$sAssays],
#'          FUN = function(x) {
#'            x[[ISASyntax$sAssayMeasType]]
#'          })
#' }
#'
#' #' @title
#' #' Retrieve the unique Sample Names per Study from an ISA object.
#' #'
#' #' @description
#' #' Retrieve from an object of the \code{\link{ISA-class}} the unique Sample
#' #' Names per Study.
#' #'
#' #' @inheritParams getInvestigationInfo
#' #'
#' #' @return A list of Study Identifiers. Each element of the list, named by the
#' #'         Study Identifier, contains a character vector specifying the unique
#' #'         Sample Names belonging to that specific study.
#' #'
#' #' @author
#' #' Maikel Verouden
#' #'
#' #' @family ISAinfo
#' #'
#' #' @export
#' getSamplesPerStudy <- function(isaObject) {
#'   temp <- lapply(X = isaObject[ISASyntax$sFiles],
#'                  FUN = function(df) {
#'                    unique(df[, grep(pattern = ISASyntax$sampleName,
#'                                     x = colnames(df))])
#'                  })
#'   names(temp) <- names(getStudyInfo(isaObject))
#'   return(temp)
#' }
#'
#' #' @title
#' #' Retrieve all Sample Names from an ISA object.
#' #'
#' #' @description
#' #' Retrieve from an object of the \code{\link{ISA-class}} all Sample Names.
#' #'
#' #' @inheritParams getInvestigationInfo
#' #'
#' #' @return A character vector containing all sample names.
#' #'
#' #' @author
#' #' Maikel Verouden
#' #'
#' #' @family ISAinfo
#' #'
#' #' @export
#' getSamples <- function(isaObject) {
#'   unique(unlist(getSamplesPerStudy(isaObject)))
#' }
#'
#' #' @title
#' #' Retrieve the unique Sample Names per Assay File Name from an ISA object.
#' #'
#' #' @description
#' #' Retrieve from an object of the \code{\link{ISA-class}} the unique Sample
#' #' Names used in the Assay Files.
#' #'
#' #' @inheritParams getInvestigationInfo
#' #'
#' #' @return A list of character vectors, where each list element, named by the
#' #' Assay File Name, contains the unique Sample Names used in that specific Assay
#' #' File.
#' #'
#' #' @author
#' #' Maikel Verouden
#' #'
#' #' @family ISAinfo
#' #'
#' #' @export
#' getSamplesPerAssayFileName <- function(isaObject) {
#'   lapply(X = isaObject[ISASyntax$aFiles],
#'          FUN = function(df) {
#'            unique(df[, grep(pattern = ISASyntax$sampleName,
#'                             x = colnames(df))])
#'          })
#' }
#'
#' #' @title
#' #' Retrieve the Assay File Names per Sample Name from an ISA object.
#' #'
#' #' @description
#' #' Retrieve from an object of the \code{\link{ISA-class}} the Assay File Names
#' #' in which a sample (specified by its Sample Name) has been used.
#' #'
#' #' @inheritParams getInvestigationInfo
#' #'
#' #' @return A list of character vectors, where each list element,(named by the
#' #' Sample Name, contains the Assay File Names in which that specific sample has
#' #' been used.
#' #'
#' #' @author
#' #' Maikel Verouden
#' #'
#' #' @family ISAinfo
#' #'
#' #' @export
#' getAssayFileNamesPerSample <- function(isaObject) {
#'   templist <- lapply(X = getSamples(isaObject), FUN = function(x) {
#'     tempvec <- vector(mode = "character", length = 0)
#'     for (i in getAssayFileNames(isaObject)) {
#'       if (x %in% isaObject[ISASyntax$aFiles][[i]][[ISASyntax$sampleName]]) {
#'         tempvec <- c(tempvec, i)
#'       }
#'     }
#'     return(tempvec)
#'   })
#'   names(templist) <- getSamples(isaObject)
#'   templist[lengths(templist) == 0] <- NA_character_
#'   if (all(is.na(templist))) {
#'     return(message(paste("None of the samples in the Study Files (as given in",
#'                          "the column Sample Name) have been used in any of the",
#'                          "Assay Files")))
#'   } else {
#'     return(templist)
#'   }
#' }
#'
#' #' @title
#' #' Retrieve Sample Names and Raw Data File Names per Assay File Name from an
#' #' ISA object.
#' #'
#' #' @description
#' #' Retrieve from an object of the \code{\link{ISA-class}} the Sample Names and
#' #' Raw Data File Names for each Assay File Name.
#' #'
#' #' @inheritParams getInvestigationInfo
#' #'
#' #' @return A list of data frames, where each list element, named by the Assay
#' #'         File Name, contains the Sample Names and Raw Data File Names.
#' #'
#' #' @author
#' #' Maikel Verouden
#' #'
#' #' @family ISAinfo
#' #'
#' #' @export
#' getSampleToRawDataFile <- function(isaObject) {
#'   lapply(X = isaObject[ISASyntax$aFiles], FUN = function(df) {
#'     tmpdf <-
#'       df[, c(ISASyntax$sampleName,
#'              ISASyntax$rawDataFile[ISASyntax$rawDataFile %in% colnames(df)])]
#'     tmpdf <- tmpdf[!duplicated(tmpdf), ]
#'     tmpdf <- tmpdf[order(tmpdf[[ISASyntax$sampleName]]), ]
#'   })
#' }
#'
#' #' @title
#' #' Retrieve Sample Names and Assay Names per Assay File from an ISA object.
#' #'
#' #' @description
#' #' Retrieve from an object of the \code{\link{ISA-class}} the Sample Names and
#' #' Assay Names for each Assay File.
#' #'
#' #' @inheritParams getInvestigationInfo
#' #'
#' #' @return A list of data frames, where each list element, named by the
#' #' Assay File Name, contains the Sample Names and Assay Names.
#' #'
#' #' @author
#' #' Maikel Verouden
#' #'
#' #' @family ISAinfo
#' #'
#' #' @export
#' getSampleToAssayName <- function(isaObject) {
#'   lapply(X = isaObject[ISASyntax$aFiles], FUN = function(df) {
#'     tmpdf <- df[ , c(ISASyntax$sampleName,
#'                      grep(pattern = ISASyntax$assayName,
#'                           x = colnames(df),
#'                           value = TRUE))]
#'     tmpdf <- tmpdf[!duplicated(tmpdf), ]
#'     tmpdf <- tmpdf[order(tmpdf[[ISASyntax$sampleName]]), ]
#'   })
#' }
#'
#' #' @title
#' #' Retrieve Raw Data File Names and Sample Names per Assay File Name from an
#' #' ISA object.
#' #'
#' #' @description
#' #' Retrieve from an object of the \code{\link{ISA-class}} the Raw Data File
#' #' Names and Sample Names for each Assay File, specified by its Assay File Name.
#' #'
#' #' @inheritParams getInvestigationInfo
#' #'
#' #' @return A list of data frames, where each list element, named by the Assay
#' #'         File Name, contains the Raw Data File Names and Sample Names of that
#' #'         specific Assay.
#' #'
#' #' @author
#' #' Maikel Verouden
#' #'
#' #' @family ISAinfo
#' #'
#' #' @export
#' getRawDataFileToSample <- function(isaObject) {
#'   lapply(X = isaObject[ISASyntax$aFiles], FUN = function(df) {
#'     tmpdf <-
#'       df[ , c(ISASyntax$rawDataFile[ISASyntax$rawDataFile %in% colnames(df)],
#'               ISASyntax$sampleName)]
#'     tmpdf <- tmpdf[!duplicated(tmpdf), ]
#'     tmpdf <- tmpdf[order(tmpdf[[1]], tmpdf[[ISASyntax$sampleName]]), ]
#'   })
#' }
#'
#' #' @title
#' #' Retrieve Assay Names and Sample Names per Assay File Name from an ISA object.
#' #'
#' #' @description
#' #' Retrieve from an object of the \code{\link{ISA-class}} the unique Assay Names
#' #' and Sample Names for each Assay File, specified by its Assay File Name.
#' #'
#' #' @inheritParams getInvestigationInfo
#' #'
#' #' @return A list of data frames, where each list element, named by the Assay
#' #'         File Name, contains the unique Assay Names and Sample Names of that
#' #'         specific Assay.
#' #'
#' #' @author
#' #' Maikel Verouden
#' #'
#' #' @family ISAinfo
#' #'
#' #' @export
#' getAssayNameToSample <- function(isaObject) {
#'   lapply(X = isaObject[ISASyntax$aFiles], FUN = function(df) {
#'     tmpdf <- df[, c(grep(pattern = ISASyntax$assayName,
#'                           x = colnames(df),
#'                           value = TRUE),
#'                      ISASyntax$sampleName)]
#'     tmpdf <- tmpdf[!duplicated(tmpdf), ]
#'     tmpdf <- tmpdf[order(tmpdf[[1]], tmpdf[[ISASyntax$sampleName]]), ]
#'   })
#' }
#'
#' #' @title
#' #' Retrieve Factor Values per Study File from an ISA object.
#' #'
#' #' @description
#' #' Retrieve from an object of the \code{\link{ISA-class}} the Factor Values for
#' #' each Study File.
#' #'
#' #' @inheritParams getInvestigationInfo
#' #'
#' #' @return A list of factor lists, where each list element, named by the Study
#' #'         Identifier, contains a list of  factors specifying the Factor Values
#' #'         used in a specific Study File linked to the Study Identifier.
#' #'
#' #' @author
#' #' Maikel Verouden
#' #'
#' #' @family ISAinfo
#' #'
#' #' @export
#' getFactors <- function(isaObject) {
#'   tmplist <- lapply(X = isaObject[ISASyntax$sFiles], FUN = function(df) {
#'     tmplist <- lapply(X = grep(pattern = ISASyntax$fctrValue,
#'                                x = colnames(df),
#'                                value = TRUE),
#'                       FUN = function(x) {
#'                         factor(df[[x]])
#'                       })
#'     names(tmplist) <- grep(pattern = ISASyntax$fctrValue,
#'                            x = colnames(df),
#'                            value = TRUE)
#'     return(tmplist)
#'   })
#'   names(tmplist) <- names(getStudyInfo(isaObject))
#'   return(tmplist)
#' }
#'
#' #' @title
#' #' Retrieve unique treatments per Study File from an ISA object.
#' #'
#' #' @description
#' #' Retrieve from an object of the \code{\link{ISA-class}} the Treatments, as
#' #' unique combinations of specific Factor Values, per Study File.
#' #'
#' #' @inheritParams getInvestigationInfo
#' #'
#' #' @return A list of data frames, where each list element, named by the Study
#' #'         Identifier, contains the unique Factor Value combinations of a
#' #'         specific Study File linked to the Study Identifier.
#' #'
#' #' @author
#' #' Maikel Verouden
#' #'
#' #' @family ISAinfo
#' #'
#' #' @export
#' getTreatments <- function(isaObject) {
#'   tmplist <- lapply(X = isaObject[ISASyntax$sFiles],
#'                     FUN = function(df) {
#'                       tempdf <- unique(df[, grep(pattern = ISASyntax$fctrValue,
#'                                        x = colnames(df))])
#'                       rownames(tempdf) <- NULL
#'                       return(tempdf)
#'                     })
#'   names(tmplist) <- names(getStudyInfo(isaObject))
#'   return(tmplist)
#' }
#'
#' #' @title
#' #' Retrieve Sample Names for each treatment per Study File from an ISA object.
#' #'
#' #' @description
#' #' Retrieve from an object of the \code{\link{ISA-class}} the Sample Names for
#' #' each treatment, as unique combination of specific Factor Values, per Study
#' #' File. The treaments can be retrieved by the \code{\link{getTreatments}}
#' #' function.
#' #'
#' #' @inheritParams getInvestigationInfo
#' #'
#' #' @return A list of lists, where each list element, named by the Study
#' #'         Identifier, contains a list of treatments. Each element in the list
#' #'         of treatments contains the Sample Names used in that specific Study
#' #'         using a specific treatment, i.e. combination of Factor Values.
#' #'
#' #' @author
#' #' Maikel Verouden
#' #'
#' #' @family ISAinfo
#' #'
#' #' @export
#' getTreatmentGroups <- function(isaObject) {
#'   treatments <- getTreatments(isaObject)
#'   studyFiles <- isaObject[ISASyntax$sFiles]
#'   samplesPerStudy <- getSamplesPerStudy(isaObject)
#'   groups <- list()
#'   for (j in seq_along(studyFiles)) {
#'     subgroups <- list()
#'     n <- nrow(treatments[[j]])
#'     for (i in seq_len(n)) {
#'       treatment <- data.frame(treatments[[j]][i, ])
#'       df <- data.frame(lapply(X = treatment,
#'                               FUN = function(x) {
#'                                 rep(x, each = length(samplesPerStudy[[j]]))
#'                               }))
#'       subgroups[[i]] <-
#'         samplesPerStudy[[j]][apply(X = studyFiles[[j]][names(treatments[[j]])] == df,
#'                                    MARGIN = 1,
#'                                    FUN = all)]
#'     }
#'     groups[[j]] <- subgroups
#'   }
#'   names(groups) <- names(getStudyInfo(isaObject))
#'   return(groups)
#' }
#'
#' #' @title
#' #' Retrieve Assay Tables from an ISA object.
#' #'
#' #' @description
#' #' Retrieve from an object of the \code{\link{ISA-class}} the Assay Tables.
#' #'
#' #' @inheritParams getInvestigationInfo
#' #'
#' #' @return A list of lists of objects of class \code{\link{assayTab}}, where
#' #'         each list element, named by the Study Identifier, contains a list
#' #'         of objects of class \code{\link{assayTab}}.
#' #'
#' #' @author
#' #' Maikel Verouden
#' #'
#' #' @family ISAinfo
#' #'
#' #' @export
#' getAssayTabs <- function(isaObject) {
#'   ## Get info from isaObject.
#'   studyInfo <- getStudyInfo(isaObject)
#'   assayInfo <- getAssayInfo(isaObject)
#'   assayTechTypes <- getAssayTechnologyTypesPerStudy(isaObject)
#'   assayMeasTypes <- getAssayMeasurementTypesPerStudy(isaObject)
#'   assayFiles <- getAssayFilesPerStudy(isaObject)
#'   assayTabs <- lapply(X = seq_along(assayInfo), FUN = function(i) {
#'     assayTabsStudy <- lapply(X = seq_along(assayInfo[[i]]), FUN = function(j) {
#'       ## Class is dependent of technology type.
#'       ## Returns empty character for 'non-existing' technology.
#'       assayTechName <-
#'         names(technologyTypes)[technologyTypes == assayTechTypes[[i]][j]]
#'       assayClass <- if (isTRUE(nzchar(assayTechName)))
#'         paste0(assayTechName, "AssayTab") else "assayTab"
#'       new(assayClass,
#'           path = isaObject[ISASyntax$path],
#'           sFilename = studyInfo[i],
#'           sIdentifier = names(studyInfo)[i],
#'           aFilename = assayInfo[[i]][j],
#'           aFile = assayFiles[[i]][[j]],
#'           aTechType = assayTechTypes[[i]][j],
#'           aMeasType = assayMeasTypes[[i]][j]
#'       )
#'     })
#'     names(assayTabsStudy) <- assayInfo[[i]]
#'     return(assayTabsStudy)
#'   })
#'   names(assayTabs) <- names(studyInfo)
#'   return(assayTabs)
#' }

