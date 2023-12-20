#### lakefinder_json ####
test_that("lakefinder_json - invalid arguments", {

	# no arguments
	expect_error(lakefinder_json())

	suppressMessages({

		# non-existent name
		expect_error(lakefinder_json(name = "Wacoooovia"))

		# non-existent id
		expect_error(lakefinder_json(id = 12345678))

		# multiple names
		expect_error(lakefinder_json(name = c("Buffalo", "Pulaski")))

		# multiple counties
		expect_error(lakefinder_json(county = c(10, 86)))

	})
})

test_that("lakefinder_json - results", {

	suppressMessages({

		# by id
		expect_message(waconia <- lakefinder_json(id = 10005900),
									 "Fetching JSON")
		expect_message(rainy_kabetogama <- lakefinder_json(id = c(69069400, 69084500)),
									 "Fetching JSON")

		# by name
		expect_message(hydes <- lakefinder_json(name = "Hydes"),
									 "Fetching JSON")
		expect_message(long <- lakefinder_json(name = "Long"),
									 "Fetching JSON")

		# in county
		expect_message(carver <- lakefinder_json(county = 10),
									 "Fetching JSON")

		# by name in county
		expect_message(st_louis_silver <- lakefinder_json(name = "silver", county = 69),
									 "Fetching JSON")

	})

	# gather sample data
	rtns <- tibble::lst(waconia, rainy_kabetogama, hydes, long, carver, st_louis_silver)

	# test that all data is a data.frame
	rtns |>
		purrr::walk(expect_s3_class, "data.frame")

	# test for proper number of rows
	rtns |>
		purrr::map(nrow) |>
		purrr::walk2(c(1,2,1,134,86, 4), expect_equal)

	# all data.frames should have pre-defined names
	rtns |>
		purrr::map(names) |>
		purrr::walk(expect_setequal, c("id", "apr_ids", "border", "resources", "county", "point", "mapid", "county_id", "fishSpecies",
																	 "specialFishingRegs", "notes", "bbox", "invasiveSpecies", "nearest_town", "morphology", "name"))

})

#### lakefinder_detail ####
test_that("lakefinder_detail - invalid arguments", {

	# no arguments
	expect_error(lakefinder_detail())

	suppressMessages({

		# missing name
		expect_error(lakefinder_detail(type = "lake_survey"))

		# missing type
		expect_error(lakefinder_detail(id = 10005900))

		# non-existent id
		expect_error(lakefinder_detail(id = 12345678, type = "lake_survey"))

		# multiple ids
		expect_error(lakefinder_detail(id = c(69069400, 69084500), type = "lake_survey"))

	})
})

test_that("lakefinder_detail - results", {

	suppressMessages({

		expect_message(waconia <- lakefinder_detail(id = 10005900, type = "lake_survey"),
									 "Fetching JSON")
		expect_message(rainy <- lakefinder_detail(id = 69069400, type = "lake_survey"),
									 "Fetching JSON")
		expect_message(hydes <- lakefinder_detail(id = 10008800, type = "lake_survey"),
									 "Fetching JSON")
		expect_message(pulaski <- lakefinder_detail(id = 86005300, type = "lake_survey"),
									 "Fetching JSON")



	})

	# gather sample data
	rtns <- tibble::lst(waconia, rainy, hydes, pulaski)

	# test that all data is a list
	rtns |>
		purrr::walk(expect_s3_class, "list")

	# all lists should have pre-defined names
	rtns |>
		purrr::map(names) |>
		purrr::walk(expect_setequal, c("officeCode", "DOWNumber", "accesses", "sampledPlants", "shoreLengthMiles", "waterClarity",
																	 "averageWaterClarity", "meanDepthFeet", "littoralAcres", "maxDepthFeet", "surveys", "lakeName", "areaAcres"))

})
