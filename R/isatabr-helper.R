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
  ### CONTINUE HERE!! ###
  # study.title = "Study Title",
  # study.description = "Study Description",
  # study.person.last.name = "Study Person Last Name",
  # study.person.first.name = "Study Person First Name",
  # study.person.mid.initial = "Study Person Mid Initial",
  # study.person.affiliation = "Study Person Affiliation",
  # sample.name = "Sample Name",
  # raw.data.file = "Raw Data File",
  # free.induction.decay.data.file = "Free Induction Decay Data File",
  # array.data.file = "Array Data File",
  derived.array.data.file = "Derived Array Data File",
  # raw.spectral.data.file = "Raw Spectral Data File",
  hybridization.assay.name = "Hybridization Assay Name",
  factor.name = "Factor Name")
### end ISASyntax list ----

### start technologyTypes list ----
technologyTypes <- list(
  fc         = "flow cytometry",
  microarray = "DNA microarray",
  ms         = "mass spectrometry",
  NMR        = "NMR spectroscopy",
  seq        = "nucleotide sequencing")
### end technologyTypes list ----

### start helper functions ----

checkCharacter <- function(...) {
  args <- list(...)
  if (!all(sapply(args, is.character))) {
    stop("The provided arguments must be of class character.")
  }
}

createISASlotDataFrame <- function(file, startRow, endRow) {
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
