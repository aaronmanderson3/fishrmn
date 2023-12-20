#' Handle ellipses for JSON queries
#'
#' @description
#' Automatically transforms the ellipses arguments into a JSON query for the MN
#' DNR APIs.
#'
#' @details
#' Function will discard named arguments that are not allowed (determined by
#' `allowed_args`).  If a single unnamed argument is supplied with a single
#' `allowed_arg`, it will be automatically renamed.
#'
#' @param ... Arguments passed on from query wrapper functions
#' @param allowed_args Names of the arguments that are allowed (`NULL` to allow
#'   all arguments)
#' @param empty_allowed Can `...` be empty?
#'
#' @return `json_handle_dots()` returns a string that concatenates the names and values of the supplied
#' arguments.
#' @keywords internal
#'
#' @examples
#' # normal execution
#' fishrmn:::json_handle_dots(name = "Waconia", type = "lake")
#' fishrmn:::json_handle_dots(name = c("Waconia", "Hydes"), type = "lake")
#'
#' # discarded args
#' fishrmn:::json_handle_dots(name = c("Waconia", "Hydes"), type = "lake", allowed_args = "name")
#'
#' # error, empty but not allowed
#' try(fishrmn:::json_handle_dots(), TRUE)
json_handle_dots <- function(..., allowed_args = NULL, empty_allowed = FALSE) {

	# collect args into a list
	args <- rlang::list2(...)

	# return `NULL` if no arguments are supplied
	if(rlang::is_empty(args)) {

		if(empty_allowed) {

			rlang::warn(
				c("No arguments supplied",
					i = "Returning `NULL`"),
				args = args
			)

			return(NULL)

		} else {

			rlang::abort(
				c("No arguements supplied",
					i = "Supply arguments or set `empty_allowed = TRUE`")
			)

		}
	}

	# concatenate argument items into a vector
	args <- purrr::map(args, paste, collapse = ",")

	# if `args` contains a single unnamed item AND `allowed_args` contains a single item, rename the item
	if(any(is.null(names(args)))) {
		if(length(args) == 1 & length(allowed_args) == 1) {
			rlang::inform(
				c("Unnamed arguments",
					i = glue::glue("Renaming the argument to `allowed_args` ({allowed_args})")
				)
			)

			names(args) <- allowed_args
		} else {
			rlang::abort(
				c("Unnamed arguments",
					x = "All arguments must be named")
			)
		}
	}

	# drop non-allowed args
	if(!is.null(allowed_args)) {

		all_names <- names(args)

		if(!rlang::is_empty(setdiff(all_names, allowed_args))) {

			rlang::warn(
				c("Arguments include non-allowed args",
					"!" = paste0(
						"Arguments to be dropped: ",
						paste(collapse = ", ", setdiff(all_names, allowed_args))
					),
					i = paste0(
						"Allowed arguments: ",
						paste(collapse = ", ", allowed_args)
					)),
				args = args
			)
		}

		# remove `allowed_args` that don't exist in `all_names`
		args <- args[names(args) %in% allowed_args]
	}

	if(rlang::is_empty(args))
		NULL
	else
		purrr::imap_chr(args, ~ glue::glue("{.y}={.x}")) |>
		paste(collapse = "&")

}

#' Error-Tolerant JSON Reader
#'
#' @description
#'
#' [jsonlite::read_json()] with a [rlang::try_fetch()] wrapper
#'
#' @details
#'
#' If [jsonlite::read_json()] is not successful, [rlang::try_fetch()] contains
#' the error and a more informative error is returned.
#'
#' @param url URL to read
#'
#' @return Parsed JSON list or data.frame
#' @seealso [jsonlite::read_json()] for JSON reading/parsing,
#'   [rlang::try_fetch()] for error handling.
#' @keywords internal
#' @examples
#' fishrmn:::try_read_json("http://services.dnr.state.mn.us/api/lakefinder/by_id/v1/?id=10005900")
#' fishrmn:::try_read_json("http://services.dnr.state.mn.us/api/lakefinder/by_id/v1/?")
try_read_json <- function(url) {

	rlang::try_fetch(
		jsonlite::read_json(url, simplifyVector = T),
		error = function(e) {

			if(grepl("invalid char in json text",
							 e$message)) {

				rlang::abort(c("Failed to parse JSON",
											 x = "Invalid character in JSON text",
											 i = glue::glue("URL = {url}"),
											 i = "Was the request redirected?"),
										 url = url)

			} else {

				rlang::abort(c("Failed to parse JSON",
											 i = glue::glue("URL = {url}")),
										 url = url,
										 parent = e)

			}
		})
}
