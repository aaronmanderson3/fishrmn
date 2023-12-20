# http://services.dnr.state.mn.us/api/gazetteer/v1/usage.html


# type = county, lake, stprk, map100k, map24k, sna, nwr, wma, stream

# gazetteer(name = "Waconia", type = "lake")
# gazetteer(name = "Waconia", type = "map24k")
# gazetteer(name = "Waconia", type = "mcd")




#' MN DNR Gazetteer
#'
#' Wrapper to the [MN DNR Gazetteer
#' API](http://services.dnr.state.mn.us/api/gazetteer/v1/usage.html).
#'
#'
#'
#' @param ... Arguments to the JSON query.  \describe{
#'   \item{name}{
#'     Name of the object to retrieve
#'
#'     * line 1
#'     * line 2
#'   }
#'   \item{type}{
#'     Type of the object to retrieve.
#'     * line 1
#'     * line 2
#'   }
#' }
#'
#'   county, lake, stprk, map100k, map24k, sna, nwr, wma, stream
#'
#'
#'
#'
#' @return
#' @export
#'
#' @examples
gazetteer <- function(...) {

	# convert args into JSON query string
	args <- json_handle_dots(..., allowed_args = c("name", "type"))

	# create URL query
	url <- glue::glue("http://services.dnr.state.mn.us/api/gazetteer/v1?", args) |>
		URLencode()

	# get JSON
	rlang::inform(c(i = glue::glue("Fetching JSON - ", url)))
	json <- try_read_json(url)

	# check for valid response
	if(!json$status %in% c("OK", "SUCCESS"))
		rlang::abort(message = c("Failed to read JSON string",
											x = glue::glue("Status:  ", json$status),
											i = glue::glue("Message:  ", json$message)),
					url = url,
					json = json)

	# return results
	json$results
}

