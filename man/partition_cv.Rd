% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sperrorest_resampling.R
\name{partition_cv}
\alias{partition_cv}
\title{Partition the data for a (non-spatial) cross-validation}
\usage{
partition_cv(
  data,
  coords = c("x", "y"),
  nfold = 10,
  repetition = 1,
  seed1 = NULL,
  return_factor = FALSE
)
}
\arguments{
\item{data}{\code{data.frame} containing at least the columns specified by
\code{coords}}

\item{coords}{(ignored by \code{partition_cv})}

\item{nfold}{number of partitions (folds) in \code{nfold}-fold cross-validation
partitioning}

\item{repetition}{numeric vector: cross-validation repetitions to be
generated. Note that this is not the number of repetitions, but the indices
of these repetitions. E.g., use \code{repetition = c(1:100)} to obtain (the
'first') 100 repetitions, and \code{repetition = c(101:200)} to obtain a
different set of 100 repetitions.}

\item{seed1}{\code{seed1+i} is the random seed that will be used by \link{set.seed} in
repetition \code{i} (\code{i} in \code{repetition}) to initialize the random number
generator before sampling from the data set.}

\item{return_factor}{if \code{FALSE} (default), return a \link{represampling} object;
if \code{TRUE} (used internally by other {sperrorest} functions), return a
\code{list} containing factor vectors (see Value)}
}
\value{
If \code{return_factor = FALSE} (the default), a \link{represampling} object.
Specifically, this is a (named) list of \code{length(repetition)} \code{resampling}
objects. Each of these \link{resampling} objects is a list of length \code{nfold}
corresponding to the folds. Each fold is represented by a list of
containing the components \code{train} and \code{test}, specifying the indices of
training and test samples (row indices for \code{data}). If \code{return_factor = TRUE} (mainly used internally), a (named) list of length
\code{length(repetition)}. Each component of this list is a vector of length
\code{nrow(data)} of type \code{factor}, specifying for each sample the fold to which
it belongs. The factor levels are \code{factor(1:nfold)}.
}
\description{
\code{partition_cv} creates a \link{represampling} object for
\code{length(repetition)}-repeated \code{nfold}-fold cross-validation.
}
\details{
This function does not actually perform a cross-validation or
partition the data set itself; it simply creates a data structure
containing the indices of training and test samples.
}
\examples{
data(ecuador)
## non-spatial cross-validation:
resamp <- partition_cv(ecuador, nfold = 5, repetition = 5)
# plot(resamp, ecuador)
# first repetition, second fold, test set indices:
idx <- resamp[["1"]][[2]]$test
# test sample used in this particular repetition and fold:
ecuador[idx, ]
}
\seealso{
\link{sperrorest}, \link{represampling}
}
