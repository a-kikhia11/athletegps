# Test scenario of using bad credentials to make a call
# Expect a 400 response

test_that("API credentials can be set", {
  # Uses credentials in local .Renviron file
  # Does not check if credentials are correct
  set_credentials(
    api_key = "A"
  )

  # Make a call with the bad credentials
  result <- expect_error(get_profiles(),regexp = "^Incorrect Request Headers or Body \\(415\\)")

})
