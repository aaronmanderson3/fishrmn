test_that("lakefinder - invalid arguments", {

	# invalid `query_by`
	expect_error(lakefinder("bad_query"))

	# multiple `query_by`
	expect_error(lakefinder(c("id", "name")))

	# no arguments
	expect_error(lakefinder("name"))

	suppressMessages({

		# non-existent name
		expect_error(lakefinder("name", name = "Wacoooovia"))

		# non-existent id
		expect_error(lakefinder("id", id = 12345678))

		# bad point
		expect_error(lakefinder("point", lon = -93, lat = 48, radius = 1))

		# mismatch of query_by and args
		expect_error(lakefinder("id", name = "Waconia"))
		expect_error(lakefinder("id", lon = -93.81398, lat = 44.85171, radius = 1000))
		expect_error(lakefinder("name", id = 10005900))
		expect_error(lakefinder("name", lon = -93.81398, lat = 44.85171, radius = 1000))
		expect_error(lakefinder("point", id = 10005900))
		expect_error(lakefinder("point", name = "Waconia"))

		# multiple names
		expect_error(lakefinder("name", name = c("Wright", "McLeod")))

		# extra args
		# this returns a valid response, but the actual list is questionable
		expect_message(lakefinder("id", id = 56009000, name = "Pulaski", lon = -93.81398, lat = 44.85171, radius = 1000))

	})
})

test_that("lakefinder - results", {

	suppressMessages({

		# by id
		expect_message(waconia <- lakefinder("id", id = 10005900),
									 "Fetching JSON")
		expect_message(rainy_kabetogama <- lakefinder("id", id = c(69069400, 69084500)),
									 "Fetching JSON")

		# by name
		expect_message(hydes <- lakefinder("name", name = "Hydes"),
									 "Fetching JSON")
		expect_message(long <- lakefinder("name", name = "Long"),
									 "Fetching JSON")

		# by point
		expect_message(buffalo_lakes <- lakefinder("point", lat = 45.17196258563692, lon = -93.87525783014095, radius = 3000),
									 "Fetching JSON")

	})

	# gather sample data
	rtns <- tibble::lst(waconia, rainy_kabetogama, hydes, long, buffalo_lakes)

	# test that all data is a data.frame
	rtns |>
		purrr::walk(expect_s3_class, "data.frame")

	# test for proper number of rows
	rtns |>
		purrr::map(nrow) |>
		purrr::walk2(c(1,2,1,134,6), expect_equal)

	# all data.frames should have pre-defined names
	rtns |>
		purrr::map(names) |>
		purrr::walk(expect_setequal, c("fish_species", "county_id", "resources", "special_fishing_regs", "border",
																	 "morphology","bbox", "apr_ids", "name", "id", "notes", "point", "county",
																	 "invasive_species", "mapid", "nearest_town", "pca_id"))

})
