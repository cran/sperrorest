\name{summary.sperrorestpoolederror}
\alias{print.sperrorest}
\alias{print.sperroresterror}
\alias{print.sperrorestimportance}
\alias{print.sperrorestpoolederror}
\alias{summary.sperrorest}
\alias{summary.sperrorestpoolederror}
\title{Summary and print methods for sperrorest results}
\usage{
  \method{summary}{sperrorestpoolederror} (object,
    level = 0, na.rm = TRUE, ...)

  \method{summary}{sperrorest} (object, ...)

  \method{print}{sperrorestimportance} (x, ...)

  \method{print}{sperroresterror} (x, ...)

  \method{print}{sperrorestpoolederror} (x, ...)

  \method{print}{sperrorest} (x, ...)
}
\arguments{
  \item{object}{a \code{\link{sperrorest}} object}

  \item{...}{additional arguments for
  \code{\link{summary.sperroresterror}} or
  \code{\link{summary.sperrorestimportance}}}

  \item{x}{Depending on method, a \code{\link{sperrorest}},
  \code{\link{sperroresterror}} or
  \code{\link{sperrorestimportance}} object}

  \item{level}{Level at which errors are summarized: 0:
  overall; 1: repetition; 2: fold}

  \item{na.rm}{Remove \code{NA} values? See
  \code{\link{mean}} etc.}
}
\description{
  Summary methods provide varying level of detail while
  print methods provide full details.
}
\seealso{
  \code{\link{sperrorest}}, \code{\link{sperroresterror}},
  \code{\link{sperrorestimportance}},
  \code{\link{summary.sperroresterror}},
  \code{\link{summary.sperrorestimportance}}
}

