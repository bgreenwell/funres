x <- y <- NULL

#' Functional residual density plot
#'
#' Generates a functional residual-vs-predictor plot, visualizing the density of
#' the functional residuals as a heatmap rendered on either the uniform or
#' normal scale.
#'
#' @param object An object of class [fresiduals].
#'
#' @param x Vector of predictor values.
#'
#' @param resolution Integer specifying the resolution of the plot. Default
#' is 101.
#'
#' @param scale Character string specifying which scale to use for plotting.
#' Default is `"uniform"`.
#'
#' @param geom Character string providing the type of [geom][ggplot2::Geom] to
#' plot. Current options include `"hex"` (the default) for a hexagonal heatmap
#' of 2D bin counts, `"kde"` for 2D kernel density estimation, or `"bin"` for a
#' heatmap of 2D bin counts.
#'
#' @param ... Additional optional arguments to be passed to the respective
#' [geom][ggplot2::Geom].
#'
#' @examples
#' # Generate data from a logistic regression model with quadratic form
#' set.seed(1217)
#' n <- 10000
#' x <- rnorm(n)
#' z <- 1 - 2*x + 3*x^2 + rlogis(n)
#' y <- ifelse(z > 0, 1, 0)
#'
#' # Fit models with/without quadratic term
#' fit.wrong <- glm(y ~ x, family = binomial)  # wrong
#' fit.right <- glm(y ~ x + I(x^2), family = binomial)  # right
#'
#' library(ggplot2)
#' library(patchwork)
#' theme_set(theme_bw())
#'
#' # Functional REsidual Density plot for each model
#' fredplot(fit.wrong, x = x) +
#'   scale_fill_distiller(palette = "Spectral") +
#'   ggtitle("Wrong model") +
#'   fredplot(fit.right, x = x) +
#'   scale_fill_distiller(palette = "Spectral") +
#'   ggtitle("Correct model")
#'
#' @export
fredplot <- function(
    object,
    x,
    resolution = 101,
    scale = c("uniform", "normal"),
    geom = c("hex", "kde", "bin"),
    ...
) {

  # Construct data frame for plotting
  uend <- unifend(object)  # uniform endpoints for functional residual
  y <- expand(uend, resolution = resolution, flat = TRUE)
  x <- rep(x, times = resolution)
  scale <- match.arg(scale)
  df <- if (scale == "uniform") {
    data.frame("x" = x, "y" = y)
  } else {
    # Deal with extreme cases of z = {0, 1}
    y[y == 0] <- 1e-6  # replace 0s with 0.000001
    y[y == 1] <- 1 - 1e-6  # replace 1s with 0.999999
    data.frame("x" = x, "y" = stats::qnorm(y))
  }

  # Construct and return plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y))
  geom <- match.arg(geom)
  if (geom == "hex") {
    p <- p + ggplot2::geom_hex(...) +
      ggplot2::scale_fill_distiller(palette = "Spectral")
  } else if (geom == "kde") {
    p <- p + ggplot2::geom_density_2d_filled(...) +
      ggplot2::scale_fill_brewer(palette = "Spectral")
  } else {
    p <- p + ggplot2::geom_bin2d(...) +
      ggplot2::scale_fill_distiller(palette = "Spectral")
  }
  return(p)

}
