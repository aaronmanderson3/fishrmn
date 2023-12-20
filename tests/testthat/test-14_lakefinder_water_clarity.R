test_that("lakefinder_water_clarity - invalid arguments", {

	# unsupported type
	expect_error(lakefinder_water_clarity())
	expect_error(lakefinder_water_clarity(NULL))
	expect_error(lakefinder_water_clarity("three"))

})

test_that("lakefinder_water_clarity - no results", {
	suppressMessages({
		expect_error(lakefinder_water_clarity(100059001))
	})
})

test_that("lakefinder_water_clarity - results", {

	suppressMessages({

		expect_message(waconia_numeric <- lakefinder_water_clarity(10005900),
									 "Fetching JSON")
		expect_message(minnewashta_numeric <- lakefinder_water_clarity(10000900),
									 "Fetching JSON")
		expect_message(pulaski_numeric <- lakefinder_water_clarity(86005300),
									 "Fetching JSON")

		expect_message(waconia_lakefinder_detail <- lakefinder_detail(id = 10005900, type = "lake_survey") |> lakefinder_water_clarity(),
									 "Fetching JSON")
		expect_message(minnewashta_lakefinder_detail <- lakefinder_detail(id = 10000900, type = "lake_survey") |> lakefinder_water_clarity(),
									 "Fetching JSON")
		expect_message(pulaski_lakefinder_detail <- lakefinder_detail(id = 86005300, type = "lake_survey") |> lakefinder_water_clarity(),
									 "Fetching JSON")

	})

	# gather sample data
	rtns <- tibble::lst(waconia_numeric,
											waconia_lakefinder_detail,
											minnewashta_numeric,
											minnewashta_lakefinder_detail,
											pulaski_numeric,
											pulaski_lakefinder_detail)

	# test for proper number of rows
	rtns |>
		purrr::map(nrow) |>
		purrr::walk(expect_gte, 1)

	# test for proper number of rows
	rtns |>
		purrr::map(ncol) |>
		purrr::walk(expect_equal, 2)

})
