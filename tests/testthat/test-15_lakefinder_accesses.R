test_that("lakefinder_accesses - invalid arguments", {

	# unsupported type
	expect_error(lakefinder_accesses())
	expect_error(lakefinder_accesses(NULL))
	expect_error(lakefinder_accesses("three"))

})

test_that("lakefinder_accesses - no results", {
	suppressMessages({
		expect_error(lakefinder_accesses(100059001))
	})
})

test_that("lakefinder_accesses - results", {

	suppressMessages({

		expect_message(waconia_numeric <- lakefinder_accesses(10005900),
									 "Fetching JSON")
		expect_message(minnewashta_numeric <- lakefinder_accesses(10000900),
									 "Fetching JSON")
		expect_message(pulaski_numeric <- lakefinder_accesses(86005300),
									 "Fetching JSON")

		expect_message(waconia_lakefinder_detail <- lakefinder_detail(id = 10005900, type = "lake_survey") |> lakefinder_accesses(),
									 "Fetching JSON")
		expect_message(minnewashta_lakefinder_detail <- lakefinder_detail(id = 10000900, type = "lake_survey") |> lakefinder_accesses(),
									 "Fetching JSON")
		expect_message(pulaski_lakefinder_detail <- lakefinder_detail(id = 86005300, type = "lake_survey") |> lakefinder_accesses(),
									 "Fetching JSON")

	})

	# gather sample data
	rtns <- tibble::lst(waconia_numeric,
											waconia_lakefinder_detail,
											minnewashta_numeric,
											minnewashta_lakefinder_detail,
											pulaski_numeric,
											pulaski_lakefinder_detail)

	# test that all data are data.frames
	rtns |>
		purrr::walk(expect_s3_class, "data.frame")

	# test for proper number of rows
	rtns |>
		purrr::map(nrow) |>
		purrr::walk(expect_gte, 1)

	# all lists should have pre-defined names
	rtns |>
		purrr::map(names) |>
		purrr::walk(expect_setequal, c("lakeAccessComments", "accessTypeId", "ownerTypeId", "publicUseAuthCode", "location"))

})
