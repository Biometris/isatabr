### ISA Class ----
#' @title
#' S4 Class ISA, initialization, extract- and replace-methods
#'
#' @description
#' An S4 class to store information from an ISA-Tab dataset, including an
#' investigation file, one or more study files, and one or more assay files for
#' each study file.
#'
#' @slot path A length-one character vector containing the path to the ISA-Tab
#'            dataset. The character vector to extract or replace this slot is
#'            "path".
#' @slot iFileName A length-one character vector containing the investigation
#'                 filename (by definition starting with \strong{\emph{i_}} and
#'                 ending at \strong{\emph{.txt}}). The character vector to
#'                 extract or replace this slot is "Investigation File Name".
#' @slot oSR A data.frame containing the "ONTOLOGY SOURCE REFERENCE" section of
#'           the investigation file. The character vector to extract or replace
#'           this slot is "ONTOLOGY SOURCE REFERENCE".
#' @slot invest A data.frame containing the "INVESTIGATION" section of
#'              the investigation file. The character vector to extract or
#'              replace this slot is "INVESTIGATION".
#' @slot iPubs A data.frame containing the "INVESTIGATION PUBLICATIONS" section
#'             of the investigation file. The character vector to extract or
#'             replace this slot is "INVESTIGATION PUBLICATIONS".
#' @slot iContacts A data.frame containing the "INVESTIGATION CONTACTS" section
#'                 of the investigation file. The character vector to extract or
#'                 replace this slot is "INVESTIGATION CONTACTS".
#' @slot study A list of data.frames containing the "STUDY" sections of the
#'             investigation file. Each study has its own section and the study
#'             identifier is used to name each element in the list. The character
#'             vector to extract or replace this slot is "STUDY".
#' @slot sDD A list of data.frames containing the "STUDY DESIGN DESCRIPTORS"
#'           sections of the investigation file. Each study has its own section
#'           and the study identifier is used to name each element in the list.
#'           The character vector to extract or replace this slot is "STUDY
#'           DESIGN DESCRIPTORS".
#' @slot sPubs A list of data.frames containing the "STUDY PUBLICATIONS"
#'             sections of the investigation file. Each study has its own
#'             section and the study identifier is used to name each element in
#'             the list. The character vector to extract or replace this slot is
#'             "STUDY PUBLICATIONS".
#' @slot sFacts A list of data.frames containing the "STUDY FACTORS" sections of
#'              the investigation file. Each study has its own section and the
#'              study identifier is used to name each element in the list. The
#'              character vector to extract or replace this slot is "STUDY
#'              FACTORS".
#' @slot sAssays A list of data.frames containing the "STUDY ASSAYS" sections of
#'               the investigation file. Each study has its own section and the
#'               study identifier is used to name each element in the list. The
#'               character vector to extract or replace this slot is "STUDY
#'               ASSAYS".
#' @slot sProts A list of data.frames containing the "STUDY PROTOCOLS" sections
#'              of the investigation file. Each study has its own section and
#'              the study identifier is used to name each element in the list.
#'              The character vector to extract or replace this slot is "STUDY
#'              PROTOCOLS".
#' @slot sContacts A list of data.frames containing the "STUDY CONTACTS"
#'                 sections of the investigation file. Each study has its own
#'                 section and the study identifier is used to name each element
#'                 in the list. The character vector to extract or replace this
#'                 slot is "STUDY CONTACTS".
#' @slot sFiles A list of data.frames containing the "Study Files", containing
#'              the contents of the Study Table files belonging to the studies
#'              in the investigation. Each study has one Study Table file. The
#'              character vector to extract or replace this slot is "Study
#'              Files".
#' @slot aFiles A list of data.frames containing the "Assay Files", containing
#'              the contents of the Assay Table files belonging to the studies
#'              in the investigation. Each study can have multiple Assay Table
#'              files. Each element of the list is named by the Assay File Name.
#'              The character vector to extract or replace this slot is "Assay
#'              Files".
#'
#' @author Maikel Verouden
#'
#' @keywords classes
#' @rdname ISA-class
#' @exportClass ISA
ISA <- setClass(Class = "ISA",
                slots = c(
                  path = "character",
                  iFileName = "character",
                  oSR = "data.frame",
                  invest = "data.frame",
                  iPubs = "data.frame",
                  iContacts = "data.frame",
                  study = "list",
                  sDD = "list",
                  sPubs = "list",
                  sFacts = "list",
                  sAssays = "list",
                  sProts = "list",
                  sContacts = "list",
                  sFiles = "list",
                  aFiles = "list"
                )
)

### assayTab Class ----
#' @title
#' S4 Class assayTab, initialization, extract- and replace-methods
#'
#' @description
#' An S4 class to store information from an assay files.
#'
#' @slot path A length-one character vector containing the path to the ISA-Tab
#'            dataset.
#' @slot sFileName A length-one character vector containing the study
#'                 filename (by definition starting with \strong{\emph{s_}} and
#'                 ending at \strong{\emph{.txt}}).
#' @slot sIndentifier A length-one character vector containing the study
#'                    identifier.
#' @slot aFileName A length-one character vector containing the assay
#'                 filename (by definition starting with \strong{\emph{a_}} and
#'                 ending at \strong{\emph{.txt}}).
#' @slot aFile A data.frame containing the contents of the Assay Table file.
#' @slot aTechType A length-one character vector containing the assay
#'                 technology type.
#' @slot aMeasType A length-one character vector containing the assay
#'                 measurement type.
#'
#' @keywords classes
#' @rdname assayTab-class
#' @exportClass assayTab
assayTab <- setClass(Class = "assayTab",
                     slots = c(
                       path = "character",
                       sFilename = "character",
                       sIdentifier = "character",
                       aFilename = "character",
                       aFile = "data.frame",
                       aTechType = "character",
                       aMeasType = "character"
                     ),
                     prototype = list(
                       path = NA_character_,
                       sFilename = NA_character_,
                       sIdentifier = NA_character_,
                       aFilename = NA_character_,
                       aFile = data.frame(),
                       aTechType = NA_character_,
                       aMeasType = NA_character_
                     )
)

### Derived assayTab Classes ----
## Special classes for different technology types to enable different methods
## for each technology type.

#' @keywords classes
#' @rdname assayTab-class
#' @exportClass msAssayTab
fcAssayTab <- setClass(Class = "fcAssayTab",
                       contains = "assayTab")

#' @keywords classes
#' @rdname assayTab-class
#' @exportClass msAssayTab
msAssayTab <- setClass(Class = "msAssayTab",
                       contains = "assayTab")

#' @keywords classes
#' @rdname assayTab-class
#' @exportClass microarrayAssayTab
microarrayAssayTab <- setClass(Class = "microarrayAssayTab",
                               contains = "assayTab")

#' @keywords classes
#' @rdname assayTab-class
#' @exportClass seqAssayTab
seqAssayTab <- setClass(Class = "seqAssayTab",
                        contains = "assayTab")

#' @keywords classes
#' @rdname assayTab-class
#' @exportClass NMRAssayTab
NMRAssayTab <- setClass(Class = "NMRAssayTab",
                        contains = "assayTab")

