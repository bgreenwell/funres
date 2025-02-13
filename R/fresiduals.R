#' Functional residuals
#'
#' Computes the function residual for XYZ as described in XYZ.
#'
#' @param object An object of class [vglm][VGAM::vglm()].
#'
#' @param ... Additional optional arguments. (Currently ignored.)
#'
#' @returns An `n`-by-2 matrix...
#'
#' @importFrom VGAM fitted
#'
#' @export
fresiduals <- function(object, ...) {
  UseMethod("fresiduals")
}


#' @rdname fresiduals
#'
#' @export
fresiduals.vglm <- function(object, ...) {

  ##############################################################################
  #
  # Question: What is the goal of this code chunk? From what I can tell, you're
  # trying to grab the class index associated with each row. If so, this does
  # not work when the response vector is a factor or character string because
  # the column names will be character string, so I made some modifications.
  #
  # FIXME: Grab class associated with max probability in each row? What if the
  # class names are not integers like in the simulation code?
  # y <- apply(model@y, MARGIN = 1, FUN = function(j) {
  #   # colnames(model@y)[which.max(j)]
  #   which.max(j)  # FIXME: Grab column idx instead?
  # })
  # The following step should not be needed if the above code is correct.
  # y_values <- y - min(y) + 1  # Make sure y values are 1, 2, 3, ..., J?
  #
  # Question: Grab fitted values/probabilites for each class, but append a
  # column of zeros to the beginning?
  #
  # probs <- cbind.data.frame(rep(0,nrow(model@y)),fitted(model))
  #
  # # Initialize a matrix to store cumulative probabilities
  # result <- matrix(NA, nrow = length(y_values), ncol = 2)
  #
  # # Loop through each observation
  # for (i in 1:length(y_values)) {
  #   # Calculate the cumulative sum of probabilities for the range before and including the current class
  #   result[i, ] <- c(sum(probs[i, 1:y_values[i]]), sum(probs[i, 1:(y_values[i]+1)]))
  # }
  ##############################################################################

  # Minimal, perhaps more robust version of the above code
  y <- apply(object@y, MARGIN = 1, FUN = function(j) which.max(j))
  cumprobs <- cbind(0, t(apply(fitted(object), MARGIN = 1, FUN = cumsum)))
  res <- cbind(
    cumprobs[cbind(seq_along(y), y)],     # P(Y < y_i)  (e.g., will be 0 if y_i = 1)
    cumprobs[cbind(seq_along(y), y + 1)]  # P(Y <= y_i) (e.g., will be 1 if y_i = J)
  )
  class(res) <- c("fresiduals", class(res))
  res
}
