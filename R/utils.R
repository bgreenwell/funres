#' @noRd
#' @keywords internal
unifend <- function(object, y = NULL, fill = FALSE, ...) {
  UseMethod("unifend")
}


#' @noRd
#' @keywords internal
#' @note
#' Support for generalized linear models (GLMs) fit via the core stats package.
unifend.glm <- function(object, y = NULL, ...) {

  # Extract needed components
  fam <- family(object)$family  # get family name as character string
  if (is.null(y)) {  # get response vector
    if (is.null(object$y)) {
      stop("No response vector could be found, please supply it ",
           "using the `y` argument.", call. = FALSE)
    } else {
      y <- object$y
    }
  }
  fv <- object$fitted.values  # get fitted values

  # Compute and return endpoints for function residual

  # Binomial -------------------------------------------------------------------
  if (fam == "binomial") {
    # FIXME: What about quasi-binomial?
    endpoints <- cbind(
      "lwr" = ifelse(y == 1, 1 - fv, 0),
      "upr" = ifelse(y == 1, 1, 1 - fv)
    )
  # Quasi-Poisson --------------------------------------------------------------
  } else if (fam == "quasipoisson") {
    rqpois <- function(n, lambda, phi) {
      stopifnot(phi >= 1)
      if (phi > 1) {
        lambda <- rgamma(n, shape = lambda / (phi - 1), rate = 1 / (phi - 1))
      }
      # Note: mu = lambda = the mean
      # rnbinom(n, mu = mu, size = mu / (phi - 1))
      rpois(n, lambda = lambda)
    }
    endpoints <- cbind(
      "lwr" = rqpois(y - 1, lambda = fv, phi = summary(object)$dispersion),
      "upr" = rqpois(y, lambda = fv, phi = summary(object)$dispersion)
    )
  # Poisson --------------------------------------------------------------------
  } else if (fam == "poisson") {
    endpoints <- cbind(
      "lwr" = ppois(y - 1, lambda = fv),
      "upr" = ppois(y, lambda = fv)
    )
  } else {
    stop("Unsupported family type.", call. = FALSE)
  }
  return(endpoints)

}


#' @noRd
#' @keywords internal
#' @note
#' Support for GLMs with negative binomial family fit via the
#' [MASS](https://cran.r-project.org/package=MASS) package.
unifend.negbin <- function(object, ...) {
  # Even though the object inherits from class "glm", it seems easier to just
  # keep this separate. Should work the same as `unifend.glm()` for the binomial
  # or quasi-binomial family.
  NULL
}


#' @noRd
#' @keywords internal
#' @note
#' Support for vector generalized linear models (VGLMs)  and vector generalized
#' additive models (VGAMs) fit via the
#' [vgam](https://cran.r-project.org/package=VGAM) package.
unifend.vglm <- function(object, ...) {
  # Seems to work for VGAMs to since they also apparently inherit from class "vglm"
  fam <- object@family@vfamily
  if ("VGAMordinal" %in% fam) {
    y <- apply(object@y, MARGIN = 1, FUN = function(j) which.max(j))
    # cumprobs <- cbind(0, t(apply(fitted(object), MARGIN = 1, FUN = cumsum)))
    cumprobs <- cbind(0, t(apply(object@fitted.values, MARGIN = 1, FUN = cumsum)))
    res <- cbind(
      "lwr" = cumprobs[cbind(seq_along(y), y)],     # P(Y < y_i)  (e.g., will be 0 if y_i = 1)
      "upr" = cumprobs[cbind(seq_along(y), y + 1)]  # P(Y <= y_i) (e.g., will be 1 if y_i = J)
    )
    return(res)
  } else {
    stop("Unsupported family type.", call. = FALSE)
  }
}


#' @noRd
#' @keywords internal
unifend.gam <- function(object, ...) {
  # This should just work since mgcv models also inherit from class glm
  unifend.glm(object, ...)
}


#' @noRd
#' @keywords internal
unifend.zeroinfl <- function(object, y = NULL, ...) {
  if (!requireNamespace("VGAM", quietly = TRUE)) {
    stop("Package \"VGAM\" is required for this function to work. ",
         "Please install it.", call. = FALSE)
  }
  if (!requireNamespace("pscl", quietly = TRUE)) {
    stop("Package \"pscl\" is required for this function to work. ",
         "Please install it.", call. = FALSE)
  }
  if (is.null(y)) {  # get response vector
    if (is.null(object$y)) {
      stop("No response vector could be found, please supply it ",
           "using the `y` argument.", call. = FALSE)
    } else {
      y <- object$y
    }
  }
  fv.count <- predict(object, type = "count")
  fv.binary <- predict(object, type = "zero")
  res <- cbind(
    "lwr" = VGAM::pzipois(y - 1, lambda = fv.count, pstr0 = fv.binary),
    "upr" = VGAM::pzipois(y, lambda = fv.count, pstr0 = fv.binary)
  )
}


#' @noRd
#' @keywords internal
expand <- function(endpoints, resolution = 101, flat = FALSE) {
  # Much faster than `apply()` + `seq()`
  z <- matrix(nrow = nrow(endpoints), ncol = resolution)
  for (i in seq_len(resolution)) {
    z[, i] <- endpoints[, 1L] + (endpoints[, 2L] - endpoints[, 1L]) /
      (resolution - 1) * (i - 1)
  }
  if (isTRUE(flat)) {
    z <- as.vector(z)
  }
  return(z)
}
