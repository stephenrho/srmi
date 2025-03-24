

.geticd10codes <- function(){
  zip <- "https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Publications/ICD10CM/2025-Update/Code-desciptions-April-2025.zip"

  file <- "icd10cm-order-April-2025.txt"

  tmp <- tempfile()
  download.file(zip, tmp)
  con <- unz(tmp, file)
  icd10_april2025 <- read.fwf(file = con,
                    widths = c(5,1,7,1,1,1,60,1,100)) # widths from icd10OrderFiles.pdf
  unlink(tmp)

  icd10_april2025 <- icd10_april2025[, c(3,5,7,9)]

  colnames(icd10_april2025) <- c("code", "billable", "short_desc", "long_desc")

  icd10_april2025$code <- gsub(" ", "", icd10_april2025$code)

  save(icd10_april2025, file = "data/icd10_april2025.rda")
}
