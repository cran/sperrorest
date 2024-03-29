#' @title Summarize error statistics obtained by {sperrorest}
#'
#' @description `summary.sperroresterror` calculates mean, standard deviation,
#'  median etc. of the calculated error measures at the specified level
#'  (overall, repetition, or fold). `summary.sperrorestreperror` does the same
#'  with the pooled error, at the overall or repetition level.
#'
#' @importFrom stats IQR kmeans mad median predict
#' rnorm runif sd terms weighted.mean
#' @importFrom graphics par plot points
#' @name summary.sperroresterror
#' @method summary sperroresterror
#'
#' @param object `sperroresterror` resp. `sperrorestcombinederror` error object
#'   calculated by [sperrorest]
#' @param level Level at which errors are summarized: 0: overall (i.e. across
#'   all repetitions); 1: repetition; 2: fold
#' @param pooled If `TRUE` (default), mean and standard deviation etc are
#'   calculated between fold-level error estimates. If `FALSE`, apply first a
#'   [weighted.mean] among folds before calculating mean, standard deviation etc
#'   among repetitions. See also Details.
#' @param na.rm Remove `NA` values? See [mean] etc.
#' @param ... additional arguments (currently ignored)
#'
#' @return Depending on the level of aggregation, a `list` or `data.frame` with
#'   mean, and at level 0 also standard deviation, median and IQR of the error
#'   measures.
#'
#' @details Let's use an example to explain the `error_rep` argument. E.g.,
#'   assume we are using 100-repeated 10-fold cross-validation. If `error_rep =
#'   TRUE` (default), the mean and standard deviation calculated when
#'   summarizing at `level = 0` are calculated across the error estimates
#'   obtained for each of the `100*10 = 1000` folds. If `error_rep = FALSE`,
#'   mean and standard deviation are calculated across the `100` repetitions,
#'   using the weighted average of the fold-level errors to calculate an error
#'   value for the entire sample. This will essentially not affect the mean
#'   value but of course the standard deviation of the error.
#'
#'   `error_rep = FALSE` is not recommended, it is mainly for testing purposes;
#'   when the test sets are small (as in leave-one-out cross-validation, in the
#'   extreme case), consider running [sperrorest] with `error_rep = TRUE` and
#'   examine only the `error_rep` component of its result.
#'
#' @seealso [sperrorest]
#'
#' @export
#' @importFrom dplyr bind_rows
summary.sperroresterror <- function(object, # nolint start
                                                 level = 0,
                                                 pooled = TRUE,
                                                 na.rm = TRUE,
                                                 ...) {
  err <- unclass(object)

  # Check if object really contains error estimates:
  is_err_object <- function(x) {
    res <- FALSE
    if (is.list(x)) {
      if (all(c("train", "test") %in% names(x))) {
        if (is.list(x$train)) {
          res <- TRUE
        }
      }
    }
    res
  }

  get_dist <- function(y) {
    ifelse(any(names(y) == "distance"), y$distance, -1)
  }

  get_err <- function(y) {
    if (!is_err_object(y)) return(NULL)
    res <- data.frame(
      train = y$train,
      test = y$test,
      distance = get_dist(y)
    )
    if (length(y$train) == 1) {
      colnames(res) <- c(
        paste0("train.", names(y$train)),
        paste0("test.", names(y$test)),
        "distance"
      )
    }
    res
  }

  if (level <= 2) {
    for (i in seq_along(err)) {
      err_rep <- list()
      cnt <- 0
      for (j in seq_along(err[[i]])) {
        tmp <- get_err(err[[i]][[j]])
        if (!is.null(tmp)) {
          cnt <- cnt + 1
          err_rep[[cnt]] <- tmp
        }
      }
      err[[i]] <- as.data.frame(dplyr::bind_rows(err_rep))
    }
  }

  # nolint end
  if (pooled) {
    if (level <= 1) {
      errdf <- err[[1]]
      if (length(err) > 1) {
        for (i in 2:length(err)) {
          if (nrow(err[[i]] > 0))
            errdf <- rbind(errdf, err[[i]])
        }
      }
      rownames(errdf) <- NULL
      err <- as.data.frame(errdf)
      rm(errdf)
    }
    if (level <= 0) {
      err <- data.frame(
        mean = apply(err, 2, function(y) {
          mean(unlist(y), na.rm = na.rm)
        }),
        sd = apply(err, 2, function(y) {
          sd(unlist(y), na.rm = na.rm)
        }),
        median = apply(err, 2, function(y) {
          median(unlist(y), na.rm = na.rm)
        }),
        IQR = apply(err, 2, function(y) IQR(unlist(y), na.rm = na.rm))
      )
    }
  } else {
    if (level <= 1) {
      ### w = summary.partition(resampling) ?????
      err <- lapply(err, function(x) {
        apply(x, 2, function(y) {
          weighted.mean(unlist(y), na.rm = na.rm)
        })
      })
      sel <- sapply(err, length) > 0
      err <- err[sel]
      err <- as.data.frame(dplyr::bind_rows(err))
    }
    if (level <= 0) {
      err <- data.frame(
        mean = sapply(err, mean),
        sd = sapply(err, sd),
        median = sapply(err, median),
        IQR = sapply(err, IQR)
      )
    }
  }
  err
}

#' @rdname summary.sperrorest
#' @inheritParams summary.sperroresterror
#' @name summary.sperrorestreperror
#' @method summary sperrorestreperror
#' @export
summary.sperrorestreperror <- function(object, # nolint
                                       level = 0,
                                       na.rm = TRUE, # nolint
                                       ...) {
  class(object) <- NULL
  object <- as.data.frame(object)
  if (level <= 0) {
    object <- data.frame(
      mean = sapply(object, mean, na.rm = na.rm),
      sd = sapply(object, sd, na.rm = na.rm),
      median = sapply(object, median, na.rm = na.rm),
      IQR = sapply(object, IQR, na.rm = na.rm)
    )
  }
  object
}

#' @title Summarize variable importance statistics obtained by {sperrorest}
#'
#' @description `summary.sperrorestimportance` calculated mean, standard
#'   deviation, median etc. of the calculated error measures at the specified
#'   level (overall, repetition, or fold).
#' @name summary.sperrorestimportance
#' @method summary sperrorestimportance
#'
#' @param object `sperrorestimportance` object calculated by [sperrorest] called
#'   with argument `importance = TRUE`
#' @inheritParams summary.sperroresterror
#' @param which optional character vector specifying selected variables for
#'   which the importances should be summarized
#'
#' @return a list or data.frame, depending on the `level` of aggregation
#'
#' @export
summary.sperrorestimportance <- function(object, # nolint start
                                         level = 0,
                                         na.rm = TRUE,
                                         which = NULL,
                                         ...) {
  # nolint end
  # Create array that will hold the variable importance results:
  nfolds <- sapply(object, length)
  if (any(nfolds != nfolds[1]) & !na.rm) {
    warning("Repetitions have a varying number of folds, therefore NAs will be produced; chaging 'na.rm' to TRUE.")
    na.rm <- TRUE
  }
  arrdim <- c(length(object), max(nfolds), dim(object[[1]][[1]]))
  arrdimnames <- list(
    names(object), # names of repetitions
    names(object[[which.max(nfolds)]]), # names of folds
    rownames(object[[1]][[1]]), # names of
    colnames(object[[1]][[1]])
  )
  arr <- array(NA, dim = arrdim, dimnames = arrdimnames)
  # Fill the array with the results:
  for (i in seq_along(object)) {
    if (nfolds[i] >= 1) {
      for (j in seq_along(object[[i]])) {
        if (is.data.frame(object[[i]][[j]])) {
          arr[i, j, , ] <- as.matrix(object[[i]][[j]])
        } else {
          warning("ignoring unexpected object in repetition ", i, ", fold ", j)
        }
      }
    }
  }
  # Summarize at the repetition level:
  if (level <= 1) {
    arr <- apply(arr, c(1, 3, 4), mean, na.rm = na.rm)
  }
  # Summarize at the overall level, i.e. across all repetitions:
  if (level <= 0) {
    if (is.null(which)) {
      arr <- data.frame(
        mean = apply(arr, c(2, 3), mean, na.rm = na.rm),
        sd = apply(arr, c(2, 3), sd, na.rm = na.rm),
        median = apply(arr, c(2, 3), median, na.rm = na.rm),
        IQR = apply(arr, c(2, 3), IQR, na.rm = na.rm)
      )
    }
    else {
      # summarize importance of selected features:
      arr <- arr[, which, ] # nocov start
      arr <- data.frame(
        mean = apply(arr, 2, mean, na.rm = na.rm),
        sd = apply(arr, 2, sd, na.rm = na.rm),
        median = apply(arr, 2, median, na.rm = na.rm),
        IQR = apply(arr, 2, IQR, na.rm = na.rm)
      ) # nocov end
    }
  }
  arr
}


#' @title Summary and print methods for sperrorest results
#'
#' @description Summary methods provide varying level of detail while print
#'   methods provide full details.
#' @name summary.sperrorest
#'
#' @method summary sperrorest
#'
#' @param object a [sperrorest] object
#' @param ... additional arguments for [summary.sperroresterror] or
#'   [summary.sperrorestimportance]
#' @param x Depending on method, a [sperrorest], `sperroresterror` or
#'   `sperrorestimportance` object
#'
#' @seealso [sperrorest], [summary.sperroresterror],
#'   [summary.sperrorestimportance]
#'
#' @export
summary.sperrorest <- function(object,
                               ...) {
  list(
    error_rep = summary(object$error_rep, ...),
    error_fold = summary(object$error_fold, ...),
    represampling = summary(object$represampling, ...),
    importance = summary(object$importance, ...),
    benchmark = summary(object$benchmark, ...),
    packageVersion = summary(object$package_version, ...)
  )
}

#' @rdname summary.sperrorest
#' @name print.sperrorestimportance
#' @method print sperrorestimportance
#' @export
print.sperrorestimportance <- function(x, ...) {
  print(unclass(summary(x, level = Inf, ...))) # nocov
}

#' @rdname summary.sperrorest
#' @name print.sperroresterror
#' @method print sperroresterror
#' @export
print.sperroresterror <- function(x, ...) {
  print(unclass(summary(x, level = Inf, ...))) # nocov
}

#' @rdname summary.sperrorest
#' @name print.sperrorestreperror
#' @method print sperrorestreperror
#' @export
print.sperrorestreperror <- function(x, ...) {
  print(unclass(summary(x,
    level = Inf, # nocov
    ...
  ))) # nocov
}

#' @rdname summary.sperrorest
#' @name print.sperrorest
#' @method print sperrorest
#' @export
print.sperrorest <- function(x, ...) {
  print(unclass(summary(x, level = Inf, ...))) # nocov
}

#' @rdname summary.sperrorest
#' @name print.sperrorestbenchmarks
#' @method print sperrorestbenchmarks
#' @export
print.sperrorestbenchmarks <- function(x, ...) {
  print(summary(x), ...) # nocov
}

#' @rdname summary.sperrorest
#' @name print.sperrorestpackageversion
#' @method print sperrorestpackageversion
#' @export
print.sperrorestpackageversion <- function(x, ...) {
  print(summary(x)) # nocov
}
