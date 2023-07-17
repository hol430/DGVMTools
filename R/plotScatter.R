#' Make a scatter plot
#' 
#' This simple function makes (and returns it as an object, it doesn't print it) a simple scatter plot (using ggplot2). 
#' The data are two layers from either one or two Field objects.  Data points which appear in one Field but not the other are excluded  
#' 
#' @param x The DGVMTools::Field or Comparison object from which the data to be plotted should be taken.
#' @param layer.x The first layer to be plotted on the x axis.
#' @param layer.y The layers to be plotted on the y-axis. Defaults to all layers except layer.x.
#' @param cols A vector of colours which are used as the colours of the dots. Must have the same length as layer.y.
#' @param alpha Numeric between 0 and 1 specifing the transparency of the points.  Default is 1 (= fully opaque).
#' @param text.multiplier A number specifying an overall multiplier for the text on the plot.  
#' Make it bigger if the text is too small on large plots and vice-versa.
#' @param tolerance Numeric, passed to copyLayers. Defines how close the longitudes and latitudes of the gridcells in \code{x} and \code{y} (if different)
#' need to be to the coordinates in order to get a match.  Can be a single numeric (for the same tolerance for both lon and lat) or a vector of two numerics (for lon and lat separately).
#' Default is no rounding (value is NULL) and so is fine for most regular spaced grids.  However, setting this can be useful to force matching of 
#' coordinates with many decimal places which may have lost a small amount of precision and so don't match exactly.
#' @param size Marker size.
#' @param one_to_one_line Iff true, a 1:1 line will be shown on the plot.
#' @param legend.position Position of the legend, in the ggplot2 style.  Passed to the ggplot function \code{theme()}. Can be "none", "top", "bottom", "left" or "right" or two-element numeric vector
#' @param x.lim,y.lim Limits for the x and y axes (each a two-element numeric, optional)
#' 
#' @return A ggplot2 object
#' @export
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}  
plotScatter <- function(x,
                        layer.x,
                        layer.y = NULL,
                        alpha = 1,
                        text.multiplier,
                        tolerance = NULL,
                        cols = NULL,
                        size = 3,
                        title = NULL,
                        subtitle = NULL,
                        x.label,
                        y.label,
                        one_to_one_line = TRUE,
                        legend.position = "bottom",
                        x.lim = NULL,
                        y.lim = NULL) {
  
  # check
  if(layer.x %in% layer.y) stop("To make a meaningful scatter plot I need either two different layers, or a two different Fields, or both!)")

  if (!is.null(layer.y) && length(layer.y) == 0) stop("Cannot make a scatter plot without any y-axis layers")
  if (!is.null(layer.y) && !is.null(cols) && length(layer.y) != length(cols)) stop("Must provide the same number of colours as y-layers")

  # extract the data.tables
  if(is.Field(x) || is.Comparison(x)) x.dt <- copy(x@data)
  else if(is.data.table(x)) x.dt <- copy(x)
  else stop(paste("Cannot plot scatter from object x of type:", paste(class(x), collapse = " ")))

  layer.y <- santiseLayersForPlotting(c(x), layer.y)
  layer.y <- layer.y[layer.y != layer.x]
  to.plot <- x.dt[, append(getDimInfo(x.dt), c(layer.x, layer.y)), with = FALSE]
  
  
  # remove spaces and -" "from column names otherwise ggplot2 goes a bit flooby (although check out 'tidyevaluation' to maybe do this nicer)
  for(this.character in c(" ", "-")) {
    x.new <- gsub(x = layer.x, pattern = this.character, replacement = "_")
    y.new <- c()
    for (y in layer.y) {
      y.new <- c(y.new, gsub(x = y, pattern = this.character, replacement = "_"))
    }
  }

  setnames(to.plot, c(layer.x, layer.y), c(x.new, y.new))

  # Remove NAs from data.
  to.plot <- as.data.frame(stats::na.omit(to.plot))

  # Remove all columns except those required for plotting.
  to.plot <- to.plot[, c(x.new, y.new)]

  # Collapse y data columns into a single column.
  col.by <- "Layer"
  y_to <- "Value"
  to.plot <- pivot_longer(to.plot, cols = all_of(y.new), names_to = col.by, values_to = y_to)

  ### 8. MAKE A DESCRIPTIVE TITLE IF ONE HAS NOT BEEN SUPPLIED
  if(missing(title) || missing(subtitle) || is.null(title) || is.null(subtitle)) {
    titles <- makePlotTitle(x)
    if(missing(title)) title <- titles[["title"]]
    else if(is.null(title)) title <- waiver()
    if(missing(subtitle)) subtitle <- titles[["subtitle"]]
    else if(is.null(subtitle)) subtitle <- waiver()
  }

  # make the scatter plot
  scatter.plot <- to.plot %>% ggplot(aes(.data[[x.new]], .data[[y_to]], colour = .data[[col.by]]))
  scatter.plot <- scatter.plot + geom_point(size = size, alpha = alpha)

  # Set correct colours.
  if(!is.null(cols)) {
    scatter.plot <- scatter.plot + scale_color_manual(values = cols, labels = y.new, breaks = y.new, name = col.by)
  }

  # Plot 1:1 line.
  if (one_to_one_line) {
    scatter.plot <- scatter.plot + geom_abline(slope = 1, intercept = 0)
  }

  # Apply x/y axis limits.
  if(!is.null(x.lim)) {
    scatter.plot <- scatter.plot + xlim(x.lim)
  }
  if(!is.null(y.lim)) {
    scatter.plot <- scatter.plot + scale_y_continuous(limits = y.lim)
  }

  # labels depending on input type
  if(is.Field(x)) {
    if (missing(x.label) || is.null(x.label)) x.label <- stringToExpression(paste0(layer.x, " ", x@source@name,  " ", x@quant@name))
    if (missing(y.label) || is.null(y.label)) y.label <- stringToExpression(paste0(layer.y[[1]], " ", x@source@name,  " ", x@quant@name))
    scatter.plot <- scatter.plot + labs(y = stringToExpression(paste0(y.label, " (", standardiseUnitString(x@quant@units), ")")),
                                        x = stringToExpression(paste0(x.label, " (", standardiseUnitString(x@quant@units), ")")))
  }
  else scatter.plot <- scatter.plot + labs(y = layer.y, x = layer.x)

  # Apply bw theme for consistency with other dgvmtools plotting functions.
  scatter.plot <- scatter.plot + theme_bw()

  # labels and positioning
  scatter.plot <- scatter.plot + labs(title = title, subtitle = subtitle)
  
  if(!missing(text.multiplier)) scatter.plot <- scatter.plot + theme(text = element_text(size = theme_get()$text$size * text.multiplier))

  # Legend formatting.
  scatter.plot <- scatter.plot + theme(legend.title = element_blank())
  scatter.plot <- scatter.plot + theme(legend.position = legend.position, legend.key.size = unit(2, 'lines'))

  return(scatter.plot)
  
}