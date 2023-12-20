# #... can be any of (id, name, county)
lakefinder_json <- function(...) {

	# convert args into JSON query string
	args <- json_handle_dots(..., allowed_args = c("id", "name", "county"))


	# create URL query
	url <- glue("https://maps2.dnr.state.mn.us/cgi-bin/lakefinder_json.cgi?{args}") |>
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
		select(-any_of(c("pca_id")))
}


# type = "lake_survey"
lakefinder_detail <- function(...) {

	args <- json_handle_dots(..., allowed_args = c("id", "type"))


	# create URL query
	url <- glue("https://maps2.dnr.state.mn.us/cgi-bin/lakefinder/detail.cgi?{args}")

	# get JSON
	message(paste0("Fetching JSON - ", url))
	json <- read_json(url, simplifyVector = T)

	# check for valid response
	if(!json$status %in% c("OK", "SUCCESS"))
		abort(message = c("Failed to read JSON string",
											x = glue("Status:  ", json$status),
											i = glue("Message:  ", json$message)),
					url = url,
					json = json)

	# return results
	json$result %>% # use %>% to access `.` in the line below
		structure(class = c(class(.), "lakefinder_detail"))
}
