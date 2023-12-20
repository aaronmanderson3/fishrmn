lakefinder_water_clarity <- function(x)
	UseMethod("lakefinder_water_clarity")

#' @export
lakefinder_water_clarity.numeric <- function(x) {
	lakefinder_detail(id = x, type = "lake_survey")$waterClarity
}

#' @export
lakefinder_water_clarity.lakefinder_detail <- function(x) {
	x$waterClarity
}
