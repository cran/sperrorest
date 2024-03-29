% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sperrorest_resampling.R
\name{partition_disc}
\alias{partition_disc}
\alias{partition_loo}
\title{Leave-one-disc-out cross-validation and leave-one-out cross-validation}
\usage{
partition_disc(
  data,
  coords = c("x", "y"),
  radius,
  buffer = 0,
  ndisc = nrow(data),
  seed1 = NULL,
  return_train = TRUE,
  prob = NULL,
  replace = FALSE,
  repetition = 1
)

partition_loo(data, ndisc = nrow(data), replace = FALSE, ...)
}
\arguments{
\item{data}{\code{data.frame} containing at least the columns specified by
\code{coords}}

\item{coords}{vector of length 2 defining the variables in \code{data} that
contain the x and y coordinates of sample locations.}

\item{radius}{radius of test area discs; performs leave-one-out resampling if
radius <0.}

\item{buffer}{radius of additional 'neutral area' around test area discs that
is excluded from training and test sets; defaults to 0, i.e. all samples
are either in the test area or in the training area.}

\item{ndisc}{Number of discs to be randomly selected; each disc constitutes a
separate test set. Defaults to \code{nrow(data)}, i.e. one disc around each
sample.}

\item{seed1}{\code{seed1+i} is the random seed that will be used by \link{set.seed} in
repetition \code{i} (\code{i} in \code{repetition}) to initialize the random number
generator before sampling from the data set.}

\item{return_train}{If \code{FALSE}, returns only test sample; if \code{TRUE}, also the
training area.}

\item{prob}{optional argument to \link{sample}.}

\item{replace}{optional argument to \link{sample}: sampling with or without
replacement?}

\item{repetition}{see \code{partition_cv}; however, see Note below: \code{repetition}
should normally be \verb{= 1} in this function.}

\item{...}{arguments to be passed to \code{partition_disc}}
}
\value{
A \link{represampling} object. Contains \code{length(repetition)} \code{resampling}
objects. Each of these contains \code{ndisc} lists with indices of test and (if
\code{return_train = TRUE}) training sets.
}
\description{
\code{partition_disc} partitions the sample into training and tests
set by selecting circular test areas (possibly surrounded by an exclusion
buffer) and using the remaining samples as training samples
(leave-one-disc-out cross-validation). \code{partition_loo} creates training and
test sets for leave-one-out cross-validation with (optional) buffer.
}
\note{
Test area discs are centered at (random) samples, not at general
random locations. Test area discs may (and likely will) overlap independently
of the value of \code{replace}. \code{replace} only controls the replacement
of the center point of discs when drawing center points from the samples.

\code{radius < 0} does leave-one-out resampling with an optional buffer.
\code{radius = 0} is similar except that samples with identical coordinates
would fall within the test area disc.
}
\examples{
data(ecuador)
parti <- partition_disc(ecuador,
  radius = 200, buffer = 200,
  ndisc = 5, repetition = 1:2
)
# plot(parti,ecuador)
summary(parti)

# leave-one-out with buffer:
parti.loo <- partition_loo(ecuador, buffer = 200)
summary(parti)
}
\references{
Brenning, A. 2005. Spatial prediction models for landslide
hazards: review, comparison and evaluation. Natural Hazards and Earth System
Sciences, 5(6): 853-862.
}
\seealso{
\link{sperrorest}, \link{partition_cv}, \link{partition_kmeans}
}
