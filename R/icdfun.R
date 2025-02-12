
# trying to emulate icd::children

#' Convert ICD9 codes from short to decimal or back
#'
#' @param x vector of ICD 9 codes
#' @param direction short or decimal
#'
#' @returns vector of converted codes (not checked for existence!)
#' @export
icd9_convert <- function(x, direction = c("short", "decimal")){
  direction <- match.arg(direction)

  s2d <- function(xx){
    stopifnot(!grepl("[^0-9VE]", xx))
    lead0 <- gsub("[1-9].*", "", xx)
    if (grepl("^[VE]", xx)){
      nlead0 <- 0
    } else{
      stopifnot(lead0 %in% c("", "0", "00"))
      nlead0 <- nchar(lead0)
    }

    if (nchar(xx) > 3) {
      if (!grepl("^E", xx)){
        xx <- gsub('^(.{3})(.*)$', '\\1\\.\\2', xx)
      } else{
        if (nchar(xx) > 4) { # E codes are E[3digits].x
          xx <- gsub('^(.{4})(.*)$', '\\1\\.\\2', xx)
        }
      }

      }
    substr(xx, start = nlead0+1, stop=nchar(xx))
  }

  d2s <- function(xx){
    stopifnot(!grepl("[^0-9\\.VE]", xx))
    leadd <- gsub("\\..*", "", xx)
    n <- if (grepl("^E", xx)) 4 else 3
    paste0(rep("0", n - nchar(leadd)), gsub("\\.", "", xx))
  }

  fun <- if (direction == "short") d2s else s2d

  sapply(x, fun)
}

# icd9_convert(c("18899"), direction = "de")
# icd9_convert(c("18.899", "1a12"), direction = "sh")


#' Convert ICD10 codes from short to decimal or back
#'
#' @param x vector of ICD 10 codes
#' @param direction short or decimal
#'
#' @returns vector of converted codes (not checked for existence!)
#' @export
icd10_convert <- function(x, direction = c("short", "decimal")){
  direction <- match.arg(direction)

  s2d <- function(xx){
    stopifnot(!grepl("[^A-Z0-9]", xx))
    if (nchar(xx) > 3) {
      xx <- gsub('^(.{3})(.*)$', '\\1\\.\\2', xx)
    }
    xx
  }

  d2s <- function(xx){
    stopifnot(!grepl("[^A-Z0-9\\.]", xx))
    gsub("\\.", "", xx)
  }

  fun <- if (direction == "short") d2s else s2d

  sapply(x, fun)
}

# icd10_convert(c("A1899", "B123"), direction = "de")
# icd10_convert(c("A18.899", "B112"), direction = "sh")

#' Find children (sub-codes) of ICD9/10 codes using `icd.data`
#'
#' @description
#' If providing decimal codes please include decimal even if there are no
#' following digits (e.g. "E11.", "188.").
#'
#' @param x vector of ICD9 or 10 diagnosis codes (will guess based on starting character)
#' @param version 9 or 10. Guessed if missing
#' @param format of the returned codes (short or decimal)
#' @param unlist return a list of codes, with a slot for each code in x, or not
#'
#' @returns a list or vector of subcodes
#' @export
children <- function(x, version, format = c("short", "decimal"), unlist=FALSE){
  format <- match.arg(format)

  hasDec <- grepl("\\.", x)

  if (missing(version)){
    startCHAR <- grepl("^[A-Z]", x)
    startNum <- grepl("^[0-9]|^[VE]", x) # num or V/E

    stopifnot(all(startCHAR) | all(!startCHAR))
    stopifnot(all(startNum) | all(!startNum))
    stopifnot(all(hasDec) | all(!hasDec))

    if (all(startCHAR)){
      version <- 10
    } else{
      version <- 9
    }
    if (any(grepl("^V|^E", x))) warning("guessing ICD 10 but one or more codes start with V or E which are in ICD 9")
  } else{
    version <- as.numeric(version)
    stopifnot(version %in% 9:10)
  }

  if (version == 10){
    ml <- icd.data::icd10cm2016
    conv <- icd10_convert
  } else if (version == 9){
    ml <- icd.data::icd9cm_hierarchy
    conv <- icd9_convert
  }

  formin <- ifelse(all(hasDec), "decimal", "short")
  if (formin == "decimal"){
    codes <- conv(x, "short")
  } else{
    codes <- x
  }

  findchildren <- function(xx){
    grep(paste0("^", xx), ml$code, value = TRUE)
  }

  out <- lapply(codes, findchildren)
  names(out) <- x

  if (format == "decimal"){
    out <- lapply(out, conv, direction = "decimal")
  }

  if (unlist){
    unlist(out, use.names = FALSE)
  } else{
    out
  }
}

# children(c("E10.", "E11.1"), format = "de")
# children(c("188.", "46."), format = "de")
# children(c("188.", "46."), format = "de", unlist = T)
