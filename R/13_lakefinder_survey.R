lakefinder_survey <- function(x)
	UseMethod("lakefinder_survey")

#' @export
lakefinder_survey.numeric <- function(x) {
	lakefinder_detail(id = x, type = "lake_survey")$survey
}

#' @export
lakefinder_survey.lakefinder_detail <- function(x) {
	x$survey
}
