x <- y <- NULL

#' Functional residual-vs-predictor plots
#'
#' Generates a functional residual-vs-predictor plot, visualizing the density of
#' the functional residuals as a heatmap rendered on either the uniform or
#' normal scale.
#'
#' @param object An object of class [fresiduals].
#'
#' @param x Vector of predictor values.
#'
#' @param scale Character string specifying which scale to use for plotting.
#' Default is `"uniform"`.
#'
#' @param resolution Integer specifying the resolution of the plot. Default
#' is 101.
#'
#' @param ref Logical indicating whether or not to include a horizonal
#' reference line. Default is `TRUE`.
#'
#' @param smooth Logical indicating whether or not to include a LOESS smoother
#' on the plot. Default is `TRUE`.
#'
#' @param ref_linetype Character string or integer specifying the line type to
#' use for the reference line.
#'
#' @param ref_color Character string or integer specifying the color to
#' use for the reference line.
#'
#' @param smooth_linetype Character string or integer specifying the line type
#' to use for the smoother.
#'
#' @param smooth_color Character string or integer specifying the color to
#' use for the smoother.
#'
#' @param ... Additional optional arguments. (Currently ignored.)
#'
#' @export
frplot <- function(object, x, scale = c("uniform", "normal"), resolution = 101,
                   ref = TRUE, smooth = TRUE,
                   ref_linetype = "dashed", ref_color = "white",
                   smooth_linetype = "solid", smooth_color = "red", ...) {
  UseMethod("frplot")
}


#' @rdname frplot
#'
#' @export
frplot.fresiduals <- function(
    object,
    x,
    scale = c("uniform", "normal"),
    resolution = 101,
    ref = TRUE,
    smooth = TRUE,
    ref_linetype = "dashed",
    ref_color = "white",
    smooth_linetype = "solid",
    smooth_color = "red",
    ...
) {


  # An n-by-resolution matrix
  z <- matrix(nrow = length(x), ncol = resolution)
  for (i in seq_len(resolution)) {
    z[, i] <- object[, 1L] + (object[, 2L] - object[, 1L]) /
      (resolution - 1) * (i - 1)
  }
  z <- as.vector(z)

  # Replicate x to match the number of rows in numbers
  xres <- rep(x, times = resolution)
  # if (is.binary==TRUE){
  #   binary<-x
  #   average_values<-cbind.data.frame(binary,object) %>%
  #     arrange(binary)%>%
  #     group_by(binary)%>%
  #     summarise(mean_values=mean(object))%>%
  #     select(mean_values)
  #   y0<-qnorm(unlist(average_values[1,1]))
  #   y1<-qnorm(unlist(average_values[2,1]))
  #   xres<-xres+runif(length(xres),min=0,max=0.01)
  #   qnumbers <- cbind.data.frame(xres, qnumbers_v)
  #   numbers <- cbind.data.frame(xres, numbers_v)
  #
  #   p_norm<-ggplot(qnumbers, aes(xres,qnumbers_v)) +
  #     stat_density_2d(aes(fill = stat(level)), geom = 'polygon') +
  #     scale_fill_viridis_c(name = "density")+
  #     geom_hline(yintercept=0,linetype="dashed", color = "red")+
  #     labs(x = xlabs, y=" ")+
  #     xlim(xl,xp) + # Set x-axis limits
  #     labs(title = title)+
  #     theme(plot.title = element_text(size=12),axis.title=element_text(size=12))
  #   segment_df <- data.frame(x = 0, y = y0, xend = 1, yend = y1)
  #   p_norm<-p_norm+
  #     geom_segment(data=segment_df,aes(x = 0, y = y0, xend = 1, yend = y1),color="#3366CC",size = 1.5)
  #   return(p_norm)
  # }
  # Convert the matrix to a vector and then apply the normal quantile transformation

  # Construct data frame for plotting
  scale <- match.arg(scale)
  df <- if (scale == "uniform") {
    data.frame("x" = xres, "y" = z)
  } else {
    # Deal with extreme cases of z = {0, 1}
    z[z == 0] <- 1e-6  # replace 0s with 0.000001
    z[z == 1] <- z - 1e-6  # replace 1s with 0.999999
    data.frame("x" = xres, "y" = stats::qnorm(z))
  }

  # Construct and return plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
    # stat_density_2d(aes(fill = stat(level)), geom = "polygon") +
    ggplot2::stat_density2d_filled()
    # scale_fill_viridis_c(name = "density") +
  if (isTRUE(ref)) {
    p <- p + ggplot2::geom_hline(yintercept = 0.5, linetype = ref_linetype,
                                 color = ref_color)
  }
  if (isTRUE(smooth)) {
    p <- p + ggplot2::geom_smooth(method = "loess", se = FALSE,
                                  linetype = smooth_linetype, color = smooth_color)
  }
  return(p)

}
