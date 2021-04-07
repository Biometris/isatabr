### start ISASyntax list ----
ISASyntax <- list(
  ## ISA-Tab File Path
  path = "path",
  ## ISA-Tab File Prefixes
  iPrefix = "i_",
  sPrefix = "s_",
  aPrefix = "a_",
  ## ISA-Tab File Name
  iFileName = "Investigation File Name",
  sFileName = "Study File Name",
  aFileName = "Study Assay File Name",
  ## ISA-Tab: Files
  sFiles = "Study Files",
  aFiles = "Assay Files",
  ## ISA-Tab: Investigation File Section Headings (in order of appearance)
  oSR       = "ONTOLOGY SOURCE REFERENCE",
  invest    = "INVESTIGATION",
  iPubs     = "INVESTIGATION PUBLICATIONS",
  iContacts = "INVESTIGATION CONTACTS",
  study     = "STUDY",
  sDD       = "STUDY DESIGN DESCRIPTORS",
  sPubs     = "STUDY PUBLICATIONS",
  sFacts    = "STUDY FACTORS",
  sAssays   = "STUDY ASSAYS",
  sProts    = "STUDY PROTOCOLS",
  sContacts = "STUDY CONTACTS",
  ## ISA-Tab: Data Frame Column Names
  iidentifier    = "Investigation Identifier",
  sidentifier    = "Study Identifier",
  assayName      = "Assay Name",
  sAssayTechType = "Study Assay Technology Type",
  sAssayMeasType = "Study Assay Measurement Type",
  rawDataFile    = c("Raw Data File",
                     "Array Data File",
                     "Raw Spectral Data File",
                     "Free Induction Decay Data File"),
  ## ISA-Tab: Data Frame Column Names grep pattern
  dataFile   = "Data File",
  sampleName = "Sample Name",
  fctrValue  = "Factor Value",
  ## Column names study info.
  sTitle = "Study Title",
  sDescription = "Study Description",
  sPersonLast = "Study Person Last Name",
  sPersonFirst = "Study Person First Name",
  sPersonMid = "Study Person Mid Initials",
  sPersonAff = "Study Person Affiliation",
  sPersonRoles = "Study Person Roles",
  ## Column names study publication info.
  sPubMedId = "Study PubMed ID",
  sPubDOI = "Study Publication DOI",
  sPubAuthList = "Study Publication Author List",
  sPubTitle = "Study Publication Title",
  sPubStatus = "Study Publication Status",
  sPubstatusTermAccNum = "Study Publication Status Term Accession Number",
  sPubStatusTermSrcREF = "Study Publication Status Term Source REF",
  arrayDataFile = "Array Data File",
  rawSpecDataFile = "Raw Spectral Data File"
)
### end ISASyntax list ----

### Required columns for oSR data.frame.
oSRCols <- c("Term Source Name",
             "Term Source File",
             "Term Source Version",
             "Term Source Description")

### start technologyTypes list ----
## Only include technology types that have an associated assayTab class.
technologyTypes <- list(
  # fc         = "flow cytometry",
  microarray = "DNA microarray",
  ms         = "mass spectrometry"
  # NMR        = "NMR spectroscopy",
  # seq        = "nucleotide sequencing"
)
### end technologyTypes list ----

### start helper functions ----

checkCharacter <- function(...) {
  args <- list(...)
  if (!all(sapply(args, is.character))) {
    stop("The provided arguments must be of class character.")
  }
}

createISASlotDataFrame <- function(file,
                                   startRow,
                                   endRow) {
  tempdf <- as.data.frame(t(file[(startRow + 1):(endRow - 1), ]),
                          stringsAsFactors = FALSE)
  colnames(tempdf) <- tempdf[1, ]
  tempdf <- tempdf[-c(1), ]
  rownames(tempdf) <- NULL
  if (any(rowSums(!is.na(tempdf)) == 0)) {
    tempdf <- tempdf[-c(which(rowSums(!is.na(tempdf)) == 0)), ]
  }
  return(tempdf)
}
### end helper functions
