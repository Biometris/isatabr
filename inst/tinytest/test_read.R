
## Get location to external data folder.
extdataPath <- file.path(system.file("extdata", package = "isatabr"))

## Check reading of files.
expect_silent(ISAfiles <- readISATab(path = file.path(extdataPath, "BII-I-1")))

## Check reading of files with extra / at end of path.
pathExt <- paste0(file.path(extdataPath, "BII-I-1"), "/")
expect_silent(ISAfiles2 <- readISATab(path = pathExt))

## Check reading from zip files
expect_silent(ISAzip <- readISATab(path = extdataPath, zipfile = "BII-I-1.zip"))
