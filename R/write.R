#' Write ISA-Tab object.
#'
#' Write ISA-Tab object to files. The investigation file, study files and assay
#' files are written to the folder specified in \code{path}.
#'
#' @inheritParams getInvestigationInfo
#'
#' @param path A character vector with the name of the directory to which the
#'             file(s) should be written. The default value is the current
#'             working directory.
#'
#' @importFrom utils write.table
#' @export
writeISAtab <- function(isaObject,
                        path = getwd()) {
  writeInvestigationFile(isaObject = isaObject, path = path)
  writeStudyFiles(isaObject = isaObject, path = path)
  writeAssayFiles(isaObject = isaObject, path = path)
}

#' Write investigation file.
#'
#' Write investigation file.
#'
#' @inheritParams writeISAtab
#'
#' @importFrom utils write.table
#' @export
writeInvestigationFile <- function(isaObject,
                                   path = getwd()) {
  iSections <- c("oSR", "invest", "iPubs", "iContacts")
  sSections <- c("study", "sDD", "sPubs", "sFacts", "sAssays",
                 "sProts", "sContacts")
  ## Construct full output file name.
  outFile <- file.path(path, isaObject[ISASyntax$iFileName])
  ## Create an empty output file.
  file.create(outFile)
  for (section in iSections) {
    ## Get section content.
    sectionContent <- t(isaObject[ISASyntax[[section]]])
    ## Add content to output file.
    writeSection(section = section,
                 sectionContent = sectionContent,
                 outFile = outFile)
  }
  studies <- names(getStudyInfo(isaObject))
  ## Study content in investigation file is added per study.
  for (study in studies) {
    for (section in sSections) {
      ## Get section content.
      sectionContent <- t(isaObject[ISASyntax[[section]]][[study]])
      ## Add content to output file.
      writeSection(section = section,
                   sectionContent = sectionContent,
                   outFile = outFile)
    }
  }
}

#' Write study files.
#'
#' Write study files.
#'
#' @inheritParams writeISAtab
#'
#' @param studyFilenames A character vector indicating the study files that
#'                       should be written. Default all study files in isaObject
#'                       are written.
#'
#' @importFrom utils write.table
#' @export
writeStudyFiles <- function(isaObject,
                            studyFilenames = getStudyInfo(isaObject),
                            path = getwd()){
  studyFiles <- isaObject[ISASyntax$sFiles]
  for (studyFilename in studyFilenames) {
    studyContent <- studyFiles[[studyFilename]]
    ## Construct full output file name.
    outFile <- file.path(path, studyFilename)
    ## Create an empty output file.
    file.create(outFile)
    ## Write output to file.
    write.table(studyContent,
                file = outFile,
                row.names = FALSE,
                col.names = TRUE,
                quote = TRUE,
                sep = "\t",
                na = "\"\"")
  }
}

#' Write assay files.
#'
#' Write assay files.
#'
#' @inheritParams writeISAtab
#'
#' @param assayFilenames A character vector indicating the study files that
#'                       should be written. Default all study files in isaObject
#'                       are written.
#'
#' @importFrom utils write.table
#' @export
writeAssayFiles <- function(isaObject,
                            assayFilenames = getAssayFileNames(isaObject),
                            path = getwd()){
  assayFiles <- isaObject[ISASyntax$aFiles]
  for (assayFilename in assayFilenames) {
    assayContent <- assayFiles[[assayFilename]]
    ## Construct full output file name.
    outFile <- file.path(path, assayFilename)
    ## Create an empty output file.
    file.create(outFile)
    ## Write output to file.
    write.table(assayContent,
                file = outFile,
                row.names = FALSE,
                col.names = TRUE,
                quote = TRUE,
                sep = "\t",
                na = "\"\"")
  }
}

#' Helper function for writing sections
#'
#' Helper function for adding a single section to an output file.
#'
#' @param section A length-one character vector indicating the section name
#'                matching a section name in ISASyntax.
#' @param sectionContent A data.frame with the content of the section that is to
#'                       be written.
#' @param outFile A length-one character vector containing the full path to the
#'                output file.
#'
#' @noRd
#' @keywords internal
writeSection <- function(section = "",
                         sectionContent,
                         outFile) {
  ## Write section header.
  cat(paste0(ISASyntax[[section]], "\n"),
      file = outFile,
      append = TRUE)
  ## Write section content line by line to allow for unquoted row names.
  for (i in seq_len(nrow(sectionContent))) {
    ## Write row names - unquoted.
    cat(paste0(rownames(sectionContent)[i], "\t"),
        file = outFile,
        append = TRUE)
    ## Write content - quoted.
    write.table(sectionContent[i, , drop = FALSE],
                file = outFile,
                row.names = FALSE,
                col.names = FALSE,
                quote = TRUE,
                sep = "\t",
                na = "\"\"",
                append = TRUE)
  }
}
