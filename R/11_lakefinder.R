lakefinder <- function(query_by, ...) {

	# match `query_by`
	query_by <- match.arg(query_by, c("id", "name", "point"))

	# convert args into JSON query string
	args <- json_handle_dots(..., allowed_args = c("id", "name", "lat", "lon", "radius"))

	# create URL query
	url <- glue::glue("http://services.dnr.state.mn.us/api/lakefinder/by_{query_by}/v1/?{args}") |>
		URLencode()

	# get JSON
	message(paste0("Fetching JSON - ", url))
	json <- try_read_json(url)

	# check for valid response
	if(!json$status %in% c("OK", "SUCCESS"))
		abort(message = c("Failed to read JSON string",
											x = glue("Status:  ", json$status),
											i = glue("Message:  ", json$message)),
					url = url,
					json = json)


	# return results
	json$results |>
		clean_names()
}
