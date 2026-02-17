# Test scenario of getting no GPS data
test_that("STEP 1: API credentials can be set", {
  # Uses credentials in local .Renviron file
  # Does not check if credentials are correct
  clear_all_credentials()

  result <- set_credentials(
    api_key = api_key
  )

  expect_true(!is.null(result))
})

test_that("STEP 2: Session Data is Retrieved, but returns empty if no data found", {

  result <- get_session("2025-01-01",drills = FALSE)

  expect_equal(result, NULL)
})
