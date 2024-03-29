#' @title transfer_parallel_output
#' @description transfers output of parallel calls to runreps
#' @keywords internal
#' @export
transfer_parallel_output <- function(my_res = NULL,
                                     res = NULL,
                                     impo = NULL,
                                     pooled_error = NULL) {

  for (i in seq_along(my_res)) {
    if (i == 1) {
      pooled_error <- my_res[[i]]$pooled_error
      impo[[i]] <- my_res[[i]]$importance
      res[[i]] <- my_res[[i]]$error
    } else {
      pooled_error <- rbind(pooled_error, my_res[[i]]$pooled_error)
      impo[[i]] <- my_res[[i]]$importance
      res[[i]] <- my_res[[i]]$error
    }
  }

  return(list(
    pooled_error = pooled_error, impo = impo,
    res = res
  ))
}

#' @title remove_missing_levels
#' @description Accounts for missing factor levels present only in test data
#' but not in train data by setting values to NA
#'
#' @importFrom stringr str_split
#'
#' @param fit fitted model on training data
#'
#' @param test_data data to make predictions for
#'
#' @return data.frame with matching factor levels to fitted model
#'
#' @keywords internal
#'
#' @examples
#' foo <- data.frame(
#'   response = rnorm(3),
#'   predictor = as.factor(c("A", "B", "C"))
#' )
#' model <- lm(response ~ predictor, foo)
#' foo.new <- data.frame(predictor = as.factor(c("A", "B", "C", "D")))
#' predict(model, newdata = remove_missing_levels(
#'   fit = model,
#'   test_data = foo.new
#' ))
#' @export
remove_missing_levels <- function(fit,
                                  test_data) {

  # https://stackoverflow.com/a/39495480/4185785

  # drop empty factor levels in test data
  test_data <- as.data.frame(droplevels(test_data))

  # 'fit' object structure of 'lm' and 'glmmPQL' is different so we need to
  # account for it
  if (any(class(fit) == "glmmPQL")) {
    # Obtain factor predictors in the model and their levels
    factors <- (gsub(
      "[-^0-9]|as.factor|\\(|\\)", "",
      names(unlist(fit$contrasts))
    ))
    # do nothing if no factors are present
    if (length(factors) == 0) {
      return(test_data)
    }

    factor_levels <- unlist(lapply(fit$contrasts, function(x) names(unmatrix(x)))) # nolint

    factor_levels <- str_split(factor_levels, ":", simplify = TRUE)[, 1]

    model_factors <- as.data.frame(cbind(factors, factor_levels))
  } else {
    # Obtain factor predictors in the model and their levels
    factors <- (gsub(
      "[-^0-9]|as.factor|\\(|\\)", "",
      names(unlist(fit$xlevels))
    ))
    # do nothing if no factors are present
    if (length(factors) == 0) {
      return(test_data)
    }

    factor_levels <- unname(unlist(fit$xlevels))
    model_factors <- as.data.frame(cbind(factors, factor_levels))
  }

  # Select column names in test data that are factor predictors in
  # trained model
  predictors <- names(test_data[names(test_data) %in% factors])

  # For each factor predictor in your data, if the level is not in the model,
  # set the value to NA

  for (i in seq_along(predictors)) {
    found <- test_data[, predictors[i]] %in% model_factors[
      model_factors$factors == predictors[i],
    ]$factor_levels
    if (any(!found)) {
      # track which variable
      var <- predictors[i]
      # set to NA
      test_data[!found, predictors[i]] <- NA
      # drop empty factor levels in test data
      test_data <- droplevels(test_data)
      # issue warning to console
      message(sprintf(
        paste0(
          "\n'sperrorest()': Setting missing levels in",
          " '%s', only present in test data but missing ",
          "in train data, to 'NA'."
        ),
        var
      ))
    }
  }
  return(test_data) # nocov end
}

# copied from gdata::unmatrix to save a pkg dependency
unmatrix <- function(x,
                     byrow = FALSE) {
  rnames <- rownames(x)
  cnames <- colnames(x)
  if (is.null(rnames)) {
    rnames <- paste("r", seq_along(x), sep = "")
  }
  if (is.null(cnames)) {
    cnames <- paste("c", seq_along(x), sep = "")
  }
  nmat <- outer(rnames, cnames, paste, sep = ":")
  if (byrow) {
    vlist <- c(t(x))
    names(vlist) <- c(t(nmat))
  }
  else {
    vlist <- c(x)
    names(vlist) <- c(nmat)
  }
  return(vlist)
}
