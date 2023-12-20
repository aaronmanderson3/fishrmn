test_that("json_handle_dots - no arguments", {

	expect_error(json_handle_dots())

	expect_warning(x <- json_handle_dots(empty_allowed = TRUE))
	expect_null(x)

	expect_warning(x <- json_handle_dots(allowed_args = c("arg1", "arg2"),
																			 empty_allowed = TRUE))
	expect_null(x)

})

test_that("json_handle_dots - unnamed args", {

	# single unnamed arg with single allowed arg --> auto-rename
	expect_message(x <- json_handle_dots(1:3, allowed_args = "numbers"))
	expect_equal(x, "numbers=1,2,3")

	# multiple unnamed args
	expect_error(json_handle_dots(1:3, 4:6, allowed_args = "numbers"))

	# multiple allowed args
	expect_error(json_handle_dots(1:3, allowed_args = c("numbers", "digits")))

})

test_that("json_handle_dots - results", {

	# `allowed_args` not used
	json_handle_dots(a = 1:3, b = 5:9) |>
		expect_equal("a=1,2,3&b=5,6,7,8,9")

	# `names(args)` matches `allowed_args`
	json_handle_dots(a = 1:3, b = 5:9, c = c("alpha", "bravo", "charlie"),
									 allowed_args = c("a", "b", "c")) |>
		expect_equal("a=1,2,3&b=5,6,7,8,9&c=alpha,bravo,charlie")

	# `names(args)` doesn't match `allowed_args` (1 arg removed)
	expect_warning(
		x <- json_handle_dots(a = 1:3, b = 5:9, c = c("alpha", "bravo", "charlie"),
													allowed_args = c("a", "c"))
	)
	expect_equal(x, "a=1,2,3&c=alpha,bravo,charlie")

	# `names(args)` doesn't match `allowed_args` (2 arg removed)
	expect_warning(
		x <- json_handle_dots(a = 1:3, b = 5:9, c = c("alpha", "bravo", "charlie"),
													allowed_args = c("a"))
	)
	expect_equal(x, "a=1,2,3")

	# `names(args)` doesn't match `allowed_args` (all args removed)
	expect_warning(
		x <- json_handle_dots(a = 1:3, b = 5:9, c = c("alpha", "bravo", "charlie"),
													allowed_args = c("d"))
	)
	expect_equal(x, NULL)

	# `names(args)` doesn't match `allowed_args` (partial mismatch)
	expect_warning(
		x <- json_handle_dots(a = 1:3, b = 5:9, c = c("alpha", "bravo", "charlie"),
													allowed_args = c("c", "d"))
	)
	expect_equal(x, "c=alpha,bravo,charlie")

})

test_that("try_read_json", {

	expect_silent(try_read_json("http://services.dnr.state.mn.us/api/lakefinder/by_id/v1/?id=10005900"))
	expect_error(try_read_json("http://services.dnr.state.mn.us/api/lakefinder/by_id/v1/?"),
							 "Was the request redirected?")

})
