## ---- echo=FALSE, cache=FALSE, results='hide'---------------------------------
library(knitr)
opts_chunk$set(
  cache = FALSE,
  eval = TRUE,
  fig.align = "center"
)
options(digits = 3)

## ---- message=FALSE-----------------------------------------------------------
library("sperrorest")

## -----------------------------------------------------------------------------
data("maipo", package = "sperrorest")

## -----------------------------------------------------------------------------
predictors <- colnames(maipo)[5:ncol(maipo)]
# Construct a formula:
fo <- as.formula(paste("croptype ~", paste(predictors, collapse = "+")))

## -----------------------------------------------------------------------------
library(MASS)
fit <- lda(fo, data = maipo)

## -----------------------------------------------------------------------------
pred <- predict(fit, newdata = maipo)$class
mean(pred != maipo$croptype)

## -----------------------------------------------------------------------------
table(pred = pred, obs = maipo$croptype)

## ---- message=FALSE-----------------------------------------------------------
library("rpart")

## -----------------------------------------------------------------------------
fit <- rpart(fo, data = maipo)

## optional: view the classiciation tree
# par(xpd = TRUE)
# plot(fit)
# text(fit, use.n = TRUE)

## -----------------------------------------------------------------------------
pred <- predict(fit, newdata = maipo, type = "class")
mean(pred != maipo$croptype)

## -----------------------------------------------------------------------------
table(pred = pred, obs = maipo$croptype)

## ---- message=FALSE-----------------------------------------------------------
library("ranger")

## -----------------------------------------------------------------------------
fit <- ranger(fo, data = maipo)
fit

## -----------------------------------------------------------------------------
pred <- predict(fit, data = maipo, type = "response")
mean(pred$predictions != maipo$croptype)

## -----------------------------------------------------------------------------
table(pred = pred$predictions, obs = maipo$croptype)

## -----------------------------------------------------------------------------
lda_predfun <- function(object, newdata, fac = NULL) {

  library(nnet)
  majority <- function(x) {
    levels(x)[which.is.max(table(x))]
  }

  majority_filter <- function(x, fac) {
    for (lev in levels(fac)) {
      x[fac == lev] <- majority(x[fac == lev])
    }
    x
  }

  pred <- predict(object, newdata = newdata)$class
  if (!is.null(fac)) pred <- majority_filter(pred, newdata[, fac])
  return(pred)
}

## -----------------------------------------------------------------------------
res_lda_nsp <- sperrorest(fo,
  data = maipo, coords = c("utmx", "utmy"),
  model_fun = lda,
  pred_fun = lda_predfun,
  pred_args = list(fac = "field"),
  smp_fun = partition_cv,
  smp_args = list(repetition = 1:10, nfold = 5),
  progress = FALSE
)

## -----------------------------------------------------------------------------
lapply(res_lda_nsp$error_rep, summary)

## ----fig.width=7, fig.asp=0.5-------------------------------------------------
resamp <- partition_factor_cv(maipo, nfold = 5, repetition = 1:1, fac = "field")
plot(resamp, maipo, coords = c("utmx", "utmy"))

## ----sperro-lda---------------------------------------------------------------
res_lda_sp <- sperrorest(fo,
  data = maipo, coords = c("utmx", "utmy"),
  model_fun = lda,
  pred_fun = lda_predfun,
  pred_args = list(fac = "field"),
  smp_fun = partition_factor_cv,
  smp_args = list(fac = "field", repetition = 1:10, nfold = 5),
  benchmark = TRUE, progress = FALSE
)
res_lda_sp$benchmark$runtime_performance

## -----------------------------------------------------------------------------
lapply(res_lda_sp$error_rep, summary)

## ----def-rf-predfun-----------------------------------------------------------
rf_predfun <- function(object, newdata, fac = NULL) {

  library(nnet)
  majority <- function(x) {
    levels(x)[which.is.max(table(x))]
  }

  majority_filter <- function(x, fac) {
    for (lev in levels(fac)) {
      x[fac == lev] <- majority(x[fac == lev])
    }
    x
  }

  pred <- predict(object, data = newdata)
  if (!is.null(fac)) pred <- majority_filter(pred$predictions, newdata[, fac])
  return(pred)
}

## ----sperro-rf----------------------------------------------------------------
res_rf_sp <- sperrorest(fo,
  data = maipo, coords = c("utmx", "utmy"),
  model_fun = ranger,
  pred_fun = rf_predfun,
  pred_args = list(fac = "field"),
  smp_fun = partition_factor_cv,
  smp_args = list(
    fac = "field",
    repetition = 1:10, nfold = 5
  ),
  benchmark = TRUE, progress = 2
)

## -----------------------------------------------------------------------------
lapply(res_rf_sp$error_rep, summary)

## -----------------------------------------------------------------------------
summary(res_rf_sp$error_rep$test_accuracy)

