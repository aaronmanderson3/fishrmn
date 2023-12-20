test_that("gazetteer - invalid arguments", {

	# no arguments
	expect_error(gazetteer())

	suppressMessages({

		# non-existent name
		expect_error(gazetteer(name = "Wacoooovia"))

		# non-existent lake, existent county
		expect_error(gazetteer(name = "Koochiching", type = "lake"))

		# non-existent county, existent lake
		expect_error(gazetteer(name = "Waconia", type = "county"))

		# invalid type
		expect_error(gazetteer(name = "Carver", type = "County"))

		# multiple names
		expect_error(gazetteer(name = c("Wright", "McLeod"), type = "county"))
	})
})

test_that("gazetteer - county", {

	suppressMessages({

		# single-item return
		expect_message(carver <- gazetteer(name = "Carver", type = "county"),
									 "Fetching JSON")

		# all-item return
		expect_message(all <- gazetteer(name = "", type = "county"),
									 "Fetching JSON")

		# single-item return, no type filter
		expect_message(fillmore <- gazetteer(name = "Fillmore"),
									 "Fetching JSON")

		# multiple-item return, partial name match
		expect_message(kanabec_kandiyohi <- gazetteer(name = "Kan", type = "county"),
									 "Fetching JSON")

	})

	# gather sample data
	rtns <- tibble::lst(carver, all, fillmore, kanabec_kandiyohi)

	# test that all data is a data.frame
	rtns|>
		purrr::walk(expect_s3_class, "data.frame")

	# test for at least one row
	rtns |>
		purrr::map(nrow) |>
		purrr::walk(expect_gt, 0)

	# all data.frames should have the same
	rtns |>
		purrr::map(names) |>
		purrr::walk(expect_setequal, c("county", "name", "type", "id", "bbox"))

})

test_that("gazetteer - lakes", {

	suppressMessages({

		# single-item return
		expect_message(waconia <- gazetteer(name = "Waconia", type = "lake"),
									 "Fetching JSON")

		# multi-item return
		expect_message(rainy <- gazetteer(name = "Rainy", type = "lake"),
									 "Fetching JSON")

		# single-item return, no type filter
		expect_message(hydes <- gazetteer(name = "Hydes"),
									 "Fetching JSON")

		# multi-item return, partial name match
		expect_message(longs <- gazetteer(name = "Long", type = "lake"),
									 "Fetching JSON")

	})

	# gather sample data
	rtns <- tibble::lst(waconia, rainy, hydes, longs)

	# test that all data is a data.frame
	rtns|>
		purrr::walk(expect_s3_class, "data.frame")

	# test for at least one row
	rtns |>
		purrr::map(nrow) |>
		purrr::walk(expect_gt, 0)

	# all data.frames should have pre-defined names
	rtns |>
		purrr::map(names) |>
		purrr::walk(expect_setequal, c("county", "name", "type", "id", "bbox"))

})
