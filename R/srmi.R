
#' Make a table of ICD/CPT/etc codes from lists
#'
#' @param ... lists of codes used to identify different diagnoses, procedures, etc.
#' Different coding systems (ICD9, ICD10, CPT, ...) would usually get separate lists.
#' @param sep codes are pasted together into a single cell separated by this string (default = ", ")
#'
#' @returns data.frame with one row per diagnosis/procedure (variable column) and a column for each list provided (coding system).
#' @export
codes_tab <- function(..., sep = ", "){
  if(!is.character(sep)) stop("sep should be character")
  codes <- list(...)
  if (rlang::is_empty(codes)) stop("provide at least one list")

  titles <- names(codes)
  if (is.null(titles)){
    titles <- paste0("list", seq(length(codes)))
    message("suggest providing some meaningful names for lists as arguments. Columns will be named list[1:n] by default.")
  }

  mystack <- function(x) stack(lapply(x, function(xx) paste(xx, collapse = sep)))
  mymerge <- function(x, y) merge(x, y, by="ind", all=TRUE)

  tab <- Reduce(mymerge, lapply(codes, mystack))

  colnames(tab)[-1] <- titles
  colnames(tab)[1] <- "variable"
  return(tab)
}

# codes = list(A = list(a = c('123','456','789'), b = c('1011', '1213')),
#              B = list(a = c("A12", "B13"), c = "123"),
#              C = list(b = "2343", d = c('1', '2'))
#              )
#
# codes_tab(icd = codes$A, cpt = codes$C, ndc = codes$B, sep = " | ")
# codes_tab(codes$A)

# todo: re-list results of codes_tab




## NIS ------

#' Get NIS file specifications
#'
#' @description
#' Uses stata load files from hcup-us.ahrq.gov to get specs for loading NIS files
#'
#' @param year of interest
#' @param file "Core", "Hospital", or "Severity"

#' @return data.frame with 4 columns ("type", "varname", "start", "end") for use in reading ASC files
#' @export
get_specs <- function(year, file = c("Core", "Hospital", "Severity")){
  file <- match.arg(file)

  start_str <- "*** Read data elements from the ASCII file ***"
  end_str <- "***  Assign labels to the data elements ***"
  types <- c("int" = "i", "byte" = "d", "double" = "d", "long" = "d", "str" = "c")

  suff <- if (year %in% 2019:2020) "_V2" else ""
  url <- paste0("https://hcup-us.ahrq.gov/db/nation/nis/tools/pgms/StataLoad_NIS_", year, "_", file, suff, ".Do")

  lines <- readLines(url)
  lines <- lines[(which(lines == start_str) + 1):(which(lines == end_str) - 3)]

  lines <- gsub("infix|\\/|\\-", "", lines)
  lines <- trimws(lines)

  lines <- strsplit(lines, split = " +")

  specs <- as.data.frame(do.call(rbind, lines))

  if (ncol(specs) != 4){
    stop("Something went wrong in getting specifications. Please check stata load program")
  }

  colnames(specs) <- c("type", "varname", "start", "end")
  specs$start = as.numeric(specs$start)
  specs$end = as.numeric(specs$end)
  #specs$type = types[specs$type]

  specs
}

#' Load NIS files into R
#'
#' @param year of interest
#' @param file "Core", "Hospital", or "Severity"
#' @param nis_path path to data files. If not given, assume they are in a subfolder titled paste0("NIS_", year)
#'
#' @return data.frame containing NIS data
#' @export
load_nis <- function(year, file = c("Core", "Hospital", "Severity"),
                     nis_path){
  # https://gist.github.com/markdanese/e53dcbfbb0c00f109e6bd65712d07cfa
  types <- c("int" = "i", "byte" = "d", "double" = "d", "long" = "d", "str" = "c")
  missing_values <- as.character(quote(c(-99, -88, -66, -99.9999999, -88.8888888, -66.6666666, -9, -8, -6, -5, -9999, -8888, -6666, -999999999, -888888888, -666666666,-999, -888, -666)))

  if (missing(nis_path)){
    nis_path <- fs::dir_ls(glob=paste0("*NIS_", year),
                           recurse = TRUE, type = "directory")
    nis_path <- paste0(nis_path, "/")
  }

  file <- match.arg(file)
  specs <- get_specs(year = year, file = file)
  specs$varname <- tolower(specs$varname)
  specs$type <- types[specs$type]
  specs$width <- with(specs, end - start + 1)

  fn <- paste0(nis_path, "NIS_", year, "_", file, ".ASC")

  d <- readr::read_fwf(file = fn,
                       col_positions = readr::fwf_widths(specs$width),
                       col_types = paste0(specs$type, collapse = ""),
                       trim_ws = TRUE,
                       na = missing_values)

  colnames(d) <- specs$varname
  as.data.frame(d)
}

## openFDA ----

query_fda <- function(dname, route){
  dname <- gsub(" ", "%", dname)
  dname <- sprintf('"%s"', dname) # enclose in quotes

  if (!missing(route) & !is.null(route)){
    dname <- sprintf('%s+AND+route:"%s"', dname, route)
  }

  url <- sprintf('https://api.fda.gov/drug/ndc.json?search=generic_name:%s&limit=100', dname)
  res <- httr::GET(url)$content |>
    rawToChar() |>
    jsonlite::fromJSON()

  if ("error" %in% names(res) | !"results" %in% names(res)){
    NULL
  } else{
    res$results
  }
}

get_fda <- function(druglist, route=NULL){
  lapply(druglist, \(x) lapply(x, query_fda, route=route))
}

get_ndc <- function(fda){
  lapply(fda, \(x) sapply(x, \(xx){
    if ("packaging" %in% names(xx)){
      sapply(xx$packaging, \(xxx) xxx$package_ndc) |>
        unlist() |>
        unique()
    } else{
      NULL
    }
  })
  )
}

get_rxcui <- function(fda){
  lapply(fda, \(x) sapply(x, \(xx){
    if ("openfda" %in% names(xx)){
      xx$openfda$rxcui |>
        unlist() |>
        unique()
    } else {
      NULL
    }
  })
  )
}
