x <- y <- NULL

#' Functional residual density plot
#'
#' Generates a functional residual-vs-predictor plot, visualizing the density of
#' the functional residuals as a heatmap rendered on either the uniform or
#' normal scale.
#'
#' @param object An object for which the extraction of
#' [functional residuals][fresiduals] is meangingful (e.g., a [glm][stats::glm])
#' object.
#'
#' @param x Vector of predictor values to use for the x-axis. Ideally, `x`
#' should represent a continuous variable. Categorical variables should be
#' coerced to integer. Using `jitter()` can also be useful. For instance,
#' something like `x = jitter(as.integer(df$x))` could be useful. If `x` is
#' categorical in nature, it's also recommended to turn off LOESS smoothing by
#' setting `smooth = FALSE`.
#'
#' @param resolution Integer specifying the resolution of the plot. Default
#' is 101.
#'
#' @param scale Character string specifying which scale to use for plotting.
#' Default is `"uniform"`.
#'
#' @param type Character string specifying the type of plot to construct.
#' Current options include `"hex"` (the default) for a hexagonal heatmap
#' of 2D bin counts, or `"kde"` for 2D kernel density estimation.
#'
#' @param n Integer specifying the number of grid points in each direction for
#' two-dimensional kernel density estimation; passed to [kde2d()][MASS::kde2d]
#' whenever `type = "kde"`. Default is 100.
#'
#' @param plot Logical indicating whether to return a data frame that can be
#' plotted manually (`FALSE`) or plot the results automatically (`TRUE`).
#' Default is `TRUE`. Setting to `FALSE` is useful for debugging or advanced
#' usage.
#'
#' @param color.palette A color palette function to be used to assign colors in
#' the plot.
#'
#' @param colorkey Logical indicating whether or not a legend should be drawn.
#' Default is `FALSE`.
#'
#' @param smooth Logical indiacting whether or not to include a LOESS smoother.
#' Default is `TRUE`,
#'
#' @param smooth.col Integer or character string specifying the line color to
#' use for the optional LOESS smoother; default is `"white"`.
#'
#' @param smooth.lwd Integer specifying the line width to use for the optional
#' LOESS smoother; default is 1.
#'
#' @param smooth.lty Integer or character string specifying the line type to
#' use for the optional LOESS smoother; default is 1 (equivalent to `"solid"`).
#'
#' @param xlab,ylab Character strings specifying the labels to use for the x-
#' and y-axes, respectively.
#'
#' @param ... Additional optional arguments passed to
#' [lattice::levelplot()][lattice::levelplot] (`type = "kde"`) or
#' [hexbin::hexbinplot()][hexbin::hexbinplot] (`type = "hex"`).
#'
#' @returns TBD.
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
#' bad <- glm(y ~ x, family = binomial)  # wrong
#' good <- glm(y ~ x + I(x^2), family = binomial)  # right
#'
#' # Functional REsidual Density plot for each model
#' gridExtra::grid.arrange(
#'   fredplot(bad, x = x, type = "hex", aspect = 1),
#'   fredplot(good, x = x, type = "hex", aspect = 1),
#'   nrow = 1
#' )
#' @export
fredplot <- function(
    object,
    x,
    resolution = 101,
    scale = c("uniform", "normal"),
    type = c("kde", "hex"),
    n = 100,
    plot = TRUE,
    color.palette = function(n) hcl.colors(n, "YlGnBu"),
    colorkey = FALSE,
    smooth = TRUE,
    smooth.col = "white",
    smooth.lwd = 1,
    smooth.lty = "solid",
    xlab = deparse1(substitute(x)),
    # xlab = "Predictor value",
    ylab = "Residual density",
    ...
) {

  # TODO: Handle the case where x is qualitative; currently, you hack something
  # by converting to numeric and, potentially, using jittering (e.g.,
  # something like `x = jitter(as.integer(df$x))`)
  if (!is.numeric(x)) {
    stop("Categorical predictors are not currently supported.", call. = FALSE)
  }

  # Construct data frame for plotting
  uend <- unifend(object)  # uniform endpoints for functional residual
  y <- expand(uend, resolution = resolution, flat = TRUE)
  xnew <- rep(x, times = resolution)
  scale <- match.arg(scale)
  df <- if (scale == "uniform") {
    data.frame("x" = xnew, "y" = y)
  } else {
    # FIXME: In reproducing the simulated ordinal examples, some of the values
    # in y appear to be larger that 1 by 2.220446e-16?
    y[y <= 0] <- 1e-6  # replace 0s with 0.000001
    y[y >= 1] <- 1 - 1e-6  # replace 1s with 0.999999
    data.frame("x" = xnew, "y" = stats::qnorm(y))
  }
  if (isFALSE(plot)) {
    return(df)
  }

  # Construct plot
  type <- match.arg(type)
  if (type == "hex") {
    if (!requireNamespace("hexbin", quietly = TRUE)) {
      stop("Package \"hexbin\" is required whenever `type = \"hex\"`. ",
           "Please install it.", call. = FALSE)
    }
    hexbin::hexbinplot(y ~ x, data = df, xlab = xlab, ylab = ylab, #main = "",
                       colramp = color.palette, colorkey = colorkey,,
                       panel = function(x, y, ...) {
                         hexbin::panel.hexbinplot(x, y, ...)
                         if (isTRUE(smooth)) {
                           lattice::panel.loess(x = df$x, y=df$y,
                                                col = smooth.col,
                                                lwd = smooth.lwd,
                                                lty = smooth.lty)
                         }
                       }, ...)
  } else {
    if (!requireNamespace("MASS", quietly = TRUE)) {
      stop("Package \"MASS\" is required whenever `type = \"kde\"`. ",
           "Please install it.", call. = FALSE)
    }
    if (!requireNamespace("lattice", quietly = TRUE)) {
      stop("Package \"lattice\" is required whenever `type = \"kde\"`. ",
           "Please install it.", call. = FALSE)
    }
    kde <- MASS::kde2d(df$x, df$y, n = n)
    kdedf <- data.frame(
      x = kde$x,
      y = rep(kde$y, each = length(kde$x)),
      z = as.vector(kde$z)
    )
    lattice::levelplot(z ~ x*y, data = kdedf, xlab = xlab, ylab = ylab,
                       col.regions = color.palette, colorkey = colorkey,
                       panel = function(x, y, ...) {
                         lattice::panel.levelplot(x, y, ...)
                         if (isTRUE(smooth)) {
                           lattice::panel.loess(x = df$x, y=df$y,
                                                col = smooth.col,
                                                lwd = smooth.lwd,
                                                lty = smooth.lty)
                         }
                       }, ...)
  }
}
