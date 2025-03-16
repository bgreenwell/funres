#' Function residuals
#'
#' Computes the function residual described in [TBD]().
#'
#' @param object A fitted model object.
#'
#' @param type Character string specifying the type of residual to compute.
#' Current options include:
#' * `"function"` - (the default) for a list of functional residuals;
#' * `"surrogate"` for a sample of surrogate residuals;
#' * `"probscale"` for probability-scale residuals.
#'
#' @param link.scale Logical indicating whether or not surrogate residuals
#' (`type = "surrogate"`) should be returned on the link scale (`TRUE`) vs. the
#' probability scale (`FALSE`). Default is `TRUE`.
#'
#' @param ... Additional optional arguments. Currently ignored.
#'
#' @return Either a list of functions (`type = "function"`) that also inherits
#' from class `"funres"` or a vector of residuals (`type = "surrogate"` or
#' `type = "probscale"`).
#'
#' @references
#' TODO: Reference paper when published in JASA.
#'
#' @examples
#' # Generate data from a logistic regression model with quadratic form
#' set.seed(1217)
#' n <- 1000
#' x <- rnorm(n)
#' z <- 1 - 2*x + 3*x^2 + rlogis(n)
#' y <- ifelse(z > 0, 1, 0)
#'
#' # Fit models with/without quadratic term
#' fit.wrong <- glm(y ~ x, family = binomial)  # wrong
#' fit.right <- glm(y ~ x + I(x^2), family = binomial)  # right
#'
#' # Generate functional residuals
#' fres.wrong <- fresiduals(fit.wrong)
#' plot(fres.wrong[[1]])  # plot first functional residual
#'
#' # Function-function plot
#' par(mfrow = c(1, 2))
#' ffplot(fres.wrong, type = "l")
#' ffplot(fit.wrong, type = "l")
#'
#' # Residual vs. predictor plot for each model based on surrogate method
#' par(mfrow = c(1, 2), las = 1)
#' lpars <- list(col = 2, lwd = 2)
#' col <- adjustcolor(1, alpha.f = 0.1)
#' palette("Okabe-Ito")
#' scatter.smooth(x, y = fresiduals(fit.wrong, type = "surrogate"),
#'                lpars = lpars, col = col, main = "Wrong model",
#'                xlab = "x", ylab = "Surrogate residual")
#' abline(h = 0, col = 3, lty = 2)
#' scatter.smooth(x, y = fresiduals(fit.right, type = "surrogate"),
#'                lpars = lpars, col = col, main = "Correct model",
#'                xlab = "x", ylab = "Surrogate residual")
#' abline(h = 0, col = 3, lty = 2)
#' @export
fresiduals <- function(object, type = c("function", "surrogate", "probscale"),
                       link.scale = TRUE, ...) {
  uend <- unifend(object)  # compute uniform endpoints for function residuals
  type <- match.arg(type)
  if (type == "function") {
    res <- apply(uend, MARGIN = 1, FUN = function(endpoints) {
      function(t) punif(t, min = endpoints[1L], max = endpoints[2L])
    })
    class(res) <- c("funres", class(res))
  } else if (type == "surrogate") {
    runifs <- apply(uend, MARGIN = 1, FUN = function(endpoints) {
      function(n) runif(n, min = endpoints[1L], max = endpoints[2L])
    })
    res <- sapply(runifs, FUN = function(sampler) sampler(1))
    if (isTRUE(link.scale)) {
      linkfun <- object$family$linkfun
      res <- linkfun(res)
    }
  } else {
    res <- apply(uend, MARGIN = 1, FUN = function(endpoints) {
      2*mean(endpoints) - 1
    })
  }
  return(res)
}
