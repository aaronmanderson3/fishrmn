lakefinder_accesses <- function(x)
	UseMethod("lakefinder_accesses")

#' @export
lakefinder_accesses.numeric <- function(x) {
	lakefinder_detail(id = x, type = "lake_survey")$accesses
}

#' @export
lakefinder_accesses.lakefinder_detail <- function(x) {
	x$accesses
}
