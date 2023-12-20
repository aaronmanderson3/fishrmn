test_that("lakefinder_survey - invalid arguments", {

	# unsupported type
	expect_error(lakefinder_survey())
	expect_error(lakefinder_survey(NULL))
	expect_error(lakefinder_survey("three"))

})

test_that("lakefinder_survey - no results", {
	suppressMessages({
		expect_error(lakefinder_survey(100059001))
	})
})

test_that("lakefinder_survey - results", {

	suppressMessages({

		expect_message(waconia_numeric <- lakefinder_survey(10005900),
									 "Fetching JSON")
		expect_message(minnewashta_numeric <- lakefinder_survey(10000900),
									 "Fetching JSON")
		expect_message(pulaski_numeric <- lakefinder_survey(86005300),
									 "Fetching JSON")

		expect_message(waconia_lakefinder_detail <- lakefinder_detail(id = 10005900, type = "lake_survey") |> lakefinder_survey(),
									 "Fetching JSON")
		expect_message(minnewashta_lakefinder_detail <- lakefinder_detail(id = 10000900, type = "lake_survey") |> lakefinder_survey(),
									 "Fetching JSON")
		expect_message(pulaski_lakefinder_detail <- lakefinder_detail(id = 86005300, type = "lake_survey") |> lakefinder_survey(),
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
		purrr::walk(expect_setequal, c("lengths", "surveyDate", "surveyID", "surveySubType", "narrative",
																	 "headerInfo", "surveyType", "fishCatchSummaries"))

})
