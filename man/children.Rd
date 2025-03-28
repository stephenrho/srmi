% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/icdfun.R
\name{children}
\alias{children}
\title{Find children (sub-codes) of ICD9/10 codes using `icd.data`}
\usage{
children(x, version, format = c("short", "decimal"), unlist = FALSE)
}
\arguments{
\item{x}{vector of ICD9 or 10 diagnosis codes (will guess based on starting character)}

\item{version}{9 or 10. Guessed if missing}

\item{format}{of the returned codes (short or decimal)}

\item{unlist}{return a list of codes, with a slot for each code in x, or not}
}
\value{
a list or vector of subcodes
}
\description{
If providing decimal codes please include decimal even if there are no
following digits (e.g. "E11.", "188.").
}
