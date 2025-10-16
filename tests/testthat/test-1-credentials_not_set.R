# Test scenarios where functions are run before setting credentials
test_that("STEP 1: get_profiles without creds FAILS", {
  clear_all_credentials()
  expect_error(get_profiles(), regexp = "^STATSports API credentials not set.", fixed=FALSE)
})

test_that("STEP 2: get_metrics without creds FAILS", {
  clear_all_credentials()
  expect_error(get_metrics(), regexp = "^STATSports API credentials not set.", fixed=FALSE)
})

test_that("STEP 3: get_session without creds FAILS", {
  clear_all_credentials()
  expect_error(get_session(), regexp = "^STATSports API credentials not set.", fixed=FALSE)
})

test_that("STEP 4: get_raw without creds FAILS", {
  clear_all_credentials()
  expect_error(get_raw(), regexp = "^STATSports API credentials not set.", fixed=FALSE)
})

test_that("STEP 5: get_imu_only without creds FAILS", {
  clear_all_credentials()
  expect_error(get_imu_only(), regexp = "^STATSports API credentials not set.", fixed=FALSE)
})
