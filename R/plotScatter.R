#' Make a scatter plot
#' 
#' This simple function makes (and returns it as an object, it doesn't print it) a simple scatter plot (using ggplot2). 
#' The data are two layers from either one or two Field objects.  Data points which appear in one Field but not the other are excluded  
#' 
#' @param x The first DGVMTools::Field or Comparison object from which the data to be plotted should be taken.
#' @param y The second DGVMTools::Field or Comparison object from which the data to be plotted should be taken.
#' Default value is x.
#' @param layer.x The first layer to be plotted (taken from x)
#' @param layer.y The layers to be plotted on the y-axis. Defaults to all layers except layer.x.
#' @param alpha Numeric between 0 and 1 specifing the transparency of the points.  Default is 1 (= fully opaque).
#' @param text.multiplier A number specifying an overall multiplier for the text on the plot.  
#' Make it bigger if the text is too small on large plots and vice-versa.
#' @param tolerance Numeric, passed to copyLayers. Defines how close the longitudes and latitudes of the gridcells in \code{x} and \code{y} (if different)
#' need to be to the coordinates in order to get a match.  Can be a single numeric (for the same tolerance for both lon and lat) or a vector of two numerics (for lon and lat separately).
#' Default is no rounding (value is NULL) and so is fine for most regular spaced grids.  However, setting this can be useful to force matching of 
#' coordinates with many decimal places which may have lost a small amount of precision and so don't match exactly.
#' 
#' @return A ggplot2 object
#' @export
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}  
plotScatter <- function(x, layer.x, layer.y = NULL, alpha = 1, text.multiplier, tolerance = NULL) {
  # check
  if(layer.x %in% layer.y) stop("To make a meaningful scatter plot I need either two different layers, or a two different Fields, or both!)")

  if (!is.null(layer.y) && length(layer.y) == 0) stop("Cannot make a scatter plot without any y-axis layers")

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

  # make the scatter plot
  scatter.plot <- to.plot %>% ggplot(aes(.data[[x.new]], .data[[y_to]], colour = .data[[col.by]]))
  scatter.plot <- scatter.plot + geom_point(size=3, alpha = alpha)

  # labels depending on input type
  if(is.Field(x)) {
    if (missing(x.label) || is.null(x.label)) x.label <- stringToExpression(paste0(layer.x, " ", x@source@name,  " ", x@quant@name))
    if (missing(y.label) || is.null(y.label)) y.label <- stringToExpression(paste0(layer.y[[1]], " ", x@source@name,  " ", x@quant@name))
    units <- standardiseUnitString(x@quant@units)
    if (units != "1") {
      x.label <- stringToExpression(paste0(x.label, " (", units, ")"))
      y.label <- stringToExpression(paste0(y.label, " (", units, ")"))
    }
    scatter.plot <- scatter.plot + labs(y = stringToExpression(y.label),
                                        x = stringToExpression(x.label))
  }
  else scatter.plot <- scatter.plot + labs(y = layer.y, x = layer.x)

  # Apply bw theme for consistency with other dgvmtools plotting functions.
  scatter.plot <- scatter.plot + theme_bw()

  if(!is.null(text.multiplier)) scatter.plot <- scatter.plot + theme(text = element_text(size = theme_get()$text$size * text.multiplier))

  # Legend formatting.
  scatter.plot <- scatter.plot + theme(legend.title = element_blank())

  return(scatter.plot)
  
}