#' Process assay tab data
#'
#' Process data from assay tab files
#'
#' @param isaObject An object of the \code{\link{ISA-class}}.
#' @param aTabObject An object of the \code{\link{assayTab-class}}.
#' @param type A character string indicating which data files should be
#' processed, either "raw" for raw data files, or "derived" for derived data
#' files. The file names are taken from the corresponding column in the
#' \code{aTabObject}.
#'
#' @docType methods
#' @rdname processAssay-methods
#' @exportMethod processAssay
setGeneric("processAssay",
           function(isaObject,
                    aTabObject,
                    type = c("raw", "derived")) standardGeneric("processAssay"))


#' @rdname processAssay-methods
#' @aliases processAssay,ISA,assayTab-method
setMethod(f = "processAssay",
          signature = c(isaObject = "ISA", aTabObject = "assayTab", type = "character"),
          definition = function(isaObject, aTabObject, type) {
            type <- match.arg(type)
            assayDat <- slot(aTabObject, "aFile")
            ## Construct column name containing files.
            fileCol <- ISASyntax[[paste0(type, "DataFile")]]["base"]
            ## Get file names.
            datFiles <- file.path(normalizePath(slot(aTabObject, "path"),
                                                winslash = .Platform$file.sep),
                                  unique(assayDat[[fileCol]]))
            ## Check that files exist.
            missFiles <- datFiles[!file.exists(datFiles)]
            if (length(missFiles) > 0) {
              stop("The following files are not found:\n",
                   paste(missFiles, collapse = ", "))
            }
            ## Read file contents.
            assayContLst <- lapply(X = datFiles, FUN = function(datFile) {
              tempdf <- read.table(datFile,
                                   header = TRUE,
                                   row.names = 1,
                                   check.names = FALSE,
                                   blank.lines.skip = TRUE,
                                   stringsAsFactors = FALSE)
              ## Remove empty rows.
              tempdf <- tempdf[apply(tempdf, 1, function(x) all(nzchar(x))), ]
              ## Remove empty columns.
              tempdf <- tempdf[, nzchar(colnames(tempdf))]
              return(tempdf)
            })
            assayCont <- do.call(rbind, assayContLst)
            return(assayCont)
          }
)

#' Process assay tab data for mass spectrometry
#'
#' Process data from assay tab files with technology type mass spectrometry
#' (ms). Processing those files requires the xcms package to be installed.
#'
#' @param isaObject An object of the \code{\link{ISA-class}}.
#' @param aTabObject An object of the \code{\link{msAssayTab-class}}.
#'
#' @rdname processAssay-methods
#' @aliases processAssay,ISA,msAssayTab-method
setMethod(f = "processAssay",
          signature = c(isaObject = "ISA", aTabObject = "msAssayTab", type = "character"),
          definition = function(isaObject, aTabObject, type) {
            if (requireNamespace("xcms", quietly = TRUE)) {
              type <- match.arg(type)
              assayDat <- slot(aTabObject, "aFile")
              ## Construct column name containing files.
              fileCol <- ISASyntax[[paste0(type, "DataFile")]]["ms"]
              spectralDatFiles <-
                file.path(normalizePath(slot(aTabObject, "path"),
                                        winslash = .Platform$file.sep),
                          unique(assayDat[[fileCol]]))
              ## Check that files exist.
              missFiles <- spectralDatFiles[!file.exists(spectralDatFiles)]
              if (length(missFiles) > 0) {
                stop("The following files are not found:\n",
                     paste(missFiles, collapse = ", "))
              }
              ## Construct vector of factors in assay.
              isaFactors <- getFactors(isaObject = isaObject)
              assayFactors <- names(isaFactors[[slot(aTabObject, "sIdentifier")]])
              if (length(assayFactors) > 0) {
                ## Construct data.frame with assay factors only.
                sClass <- assayDat[, assayFactors, drop = FALSE]
                for (colName in colnames(sClass)) {
                  if (!is.factor(sClass[[colName]])) {
                    ## Convert to factor if not a factor already.
                    sClass[[colName]] <- as.factor(sClass[[colName]])
                  }
                }
                xset <- xcms::xcmsSet(files = spectralDatFiles,
                                      sclass = sClass)
              } else {
                xset = try(xcms::xcmsSet(files = spectralDatFiles,
                                         phenoData = assayDat))
              }
              return(xset)
            } else {
              stop("For reading mass spectrometry data the xcms package ",
                   "should be installed.\n")
            }
          }
)

#' Process assay tab data for DNA microarray
#'
#' Process data from assay tab files with technology type DNA microarray
#' (ms). Processing those files requires the Biobase and affy packages to be
#' installed.
#'
#' @param isaObject An object of the \code{\link{ISA-class}}.
#' @param aTabObject An object of the \code{\link{microarrayAssayTab-class}}.
#'
#' @rdname processAssay-methods
#' @aliases processAssay,ISA,msAssayTab-method
setMethod(f = "processAssay",
          signature = c(isaObject = "ISA", aTabObject = "microarrayAssayTab", type = "character"),
          definition = function(isaObject, aTabObject, type) {
            if (requireNamespace("affy", quietly = TRUE)) {
              type <- match.arg(type)
              assayDat <- slot(aTabObject, "aFile")
              ## Construct column name containing files.
              fileCol <- ISASyntax[[paste0(type, "DataFile")]]["microarray"]
              ## Get microarray files for assay.
              microarrayDatFiles <- unique(assayDat[[fileCol]])
              ## Check that files exist.
              missFiles <- microarrayDatFiles[!file.exists(microarrayDatFiles)]
              if (length(missFiles) > 0) {
                stop("The following files are not found:\n",
                     paste(missFiles, collapse = ", "))
              }
              ## Construct meta data.
              aTabMIAME <- constructMIAMEMetadata(isaObject = isaObject,
                                                  aTabObject = aTabObject)
              ## Get expression set.
              ## justRMA only reads files from working directory.
              ## So change to that and reset when exiting function.
              wd <- getwd()
              on.exit(setwd(wd), add = TRUE)
              setwd(slot(aTabObject, "path"))
              xset <- affy::justRMA(filenames = microarrayDatFiles,
                                    #phenoData = assayDat,
                                    description = aTabMIAME)
              return(xset)
            } else {
              stop("For reading DNA microarray data the affy package ",
                   "should be installed.\n")
            }
          }
)

#' Helper function for construction MIAME meta data
#'
#' Helper function for construction of MIAME meta data for DNA microarray
#' assay tabs objects. MIAME is a class in the Biobase package for storing
#' MicroArray Experiment Information.
#'
#' @return An object of class \code{MIAME}.
#'
#' @noRd
#' @keywords internal
#' @importFrom utils person
constructMIAMEMetadata <- function(isaObject,
                                   aTabObject) {
  if (requireNamespace("Biobase")) {
    if (is(aTabObject, "microarrayAssayTab")) {
      assayDat <- slot(aTabObject, "aFile")
      sIdentifier <- names(slot(aTabObject, "sFilename"))
      sInfo <- slot(isaObject, "study")[[sIdentifier]]
      sContacts <- isaObject[ISASyntax$sContacts][[sIdentifier]]
      ## Get corresponding author details.
      sCorr <- sContacts[sContacts[[ISASyntax$sPersonRoles]] == "corresponding author", ]
      sCorrPers <- as.character(person(given = paste(sCorr[[ISASyntax$sPersonFirst]],
                                                     sCorr[[ISASyntax$sPersonMid]]),
                                       family = sCorr[[ISASyntax$sPersonLast]]))
      aTabMIAME <- Biobase::MIAME(name = sIdentifier,
                                  lab = sCorr[[ISASyntax$sPersonAff]],
                                  contact = sCorrPers,
                                  title = sInfo[[ISASyntax$sTitle]],
                                  abstract = sInfo[[ISASyntax$sDescription]],
                                  samples = as.list(assayDat["Sample Name"])
      )
      return(aTabMIAME)
    } else {
      stop("MIAME meta data can only be constructed for assays of type ",
           "DNA microarray.\n")
    }
  }
  stop("For constructing MIAME meta data the Biobase package ",
       "should be installed.\n")
}

