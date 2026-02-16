# Test scenario of getting no GPS data
test_that("STEP 1: API credentials can be set", {
  # Uses credentials in local .Renviron file
  # Does not check if credentials are correct
  clear_all_credentials()

  result <- set_credentials(
    api_key = Sys.getenv("api_key")
  )

  expect_true(!is.null(result))
})

test_that("STEP 2: Session Data is Retrieved, but returns empty if no data found", {
  clear_all_credentials()
  reset_credentials()

  result <- length(key_list(service="statsports_credentials"))

  expect_equal(result, 2)
})
