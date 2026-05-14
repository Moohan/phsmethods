# A helper function to generate a 'real' CHI number i.e. one which passes
# `chi_check`, given the first 6 chars i.e. the DoB
gen_real_chi <- function(first_6) {
  for (i in 1111:9999) {
    chi <- chi_pad(as.character(first_6 * 10000 + i))

    if (chi_check(chi) == "Valid CHI") {
      return(chi)
    }
  }
}

test_that("age_from_chi handles NA values in vectorised ref_date", {
  # CHIs: 01/01/1933, 01/01/1940, 01/01/1962
  chis <- c("0101336489", "0101405073", "0101625707")

  # ref_date vector with NAs
  ref_dates <- as.Date(c("2000-01-01", NA, "2020-01-01")) # NA should default to Sys.Date()

  # Expected ages:
  # 1933-01-01 at 2000-01-01 -> 67
  # 1940-01-01 at Sys.Date() -> depends on today's date (e.g., 85 if Sys.Date() is 2025-05-15)
  # 1962-01-01 at 2020-01-01 -> 58

  # Calculate expected age for the NA case based on Sys.Date()
  expected_age_na <- age_calculate(as.Date("1940-01-01"), Sys.Date())

  expect_equal(
    age_from_chi(chis, ref_date = ref_dates),
    c(67, expected_age_na, 58)
  )

  # Test with all NA ref_dates
  all_na_ref_dates <- as.Date(c(NA, NA, NA))
  expected_ages_all_na <- age_calculate(
    as.Date(c("1933-01-01", "1940-01-01", "1962-01-01")),
    Sys.Date()
  )

  expect_equal(
    age_from_chi(chis, ref_date = all_na_ref_dates),
    expected_ages_all_na
  )
})

test_that("age_from_chi handles NA values in vectorised min_age", {
  # CHIs: 01/01/1933, 01/01/1940, 01/01/1962
  chis <- c("0101336489", "0101405073", "0101625707")
  ref_date <- as.Date("2024-01-01") # Fixed reference date

  # min_age vector with NAs
  min_ages <- c(10, NA, 70) # NA should default to 0

  expect_message(
    expect_equal(
      age_from_chi(
        chis,
        ref_date = ref_date,
        min_age = min_ages,
        max_age = 120
      ),
      c(91, 84, NA_real_)
    )
  )

  # Test with all NA min_ages (should all default to 0)
  all_na_min_ages <- as.integer(c(NA, NA, NA))
  expect_equal(
    age_from_chi(
      chis,
      ref_date = ref_date,
      min_age = all_na_min_ages,
      max_age = 120
    ),
    c(91, 84, 62)
  )
})

test_that("age_from_chi handles NA values in vectorised max_age", {
  # CHIs: 01/01/1933, 01/01/1940, 01/01/1962
  chis <- c("0101336489", "0101405073", "0101625707")
  ref_date <- as.Date("2024-01-01") # Fixed reference date

  # max_age vector with NAs
  max_ages <- c(100, NA, 60) # NA should default to age from 1900-01-01 relative to ref_date

  expect_message(
    expect_equal(
      age_from_chi(chis, ref_date = ref_date, min_age = 0, max_age = max_ages), # Use min_age 0 to isolate max_age effect
      c(91, 84, NA_real_)
    )
  )

  # Test with all NA max_ages (should all default to age from 1900-01-01)
  all_na_max_ages <- as.integer(c(NA, NA, NA))
  expect_equal(
    age_from_chi(
      chis,
      ref_date = ref_date,
      min_age = 0,
      max_age = all_na_max_ages
    ),
    c(91, 84, 62) # All ages are <= 124
  )
})

test_that("age_from_chi handles mixed NA values in vectorised inputs", {
  # CHIs: 01/01/1933, 01/01/1940, 01/01/1962
  chis <- c("0101336489", "0101405073", "0101625707")

  # Mixed NA inputs
  ref_dates <- as.Date(c("2000-01-01", NA, "2020-01-01")) # NA defaults to Sys.Date()
  min_ages <- c(10, 0, NA) # NA defaults to 0
  max_ages <- c(100, NA, 60) # NA defaults to age from 1900-01-01 relative to ref_date

  # Calculate default max_age for the second element (ref_date is Sys.Date())
  default_max_age_2nd <- age_calculate(as.Date("1900-01-01"), Sys.Date())

  expected_age_2nd <- age_calculate(as.Date("1940-01-01"), Sys.Date())

  expect_equal(
    age_from_chi(
      chis,
      ref_date = ref_dates,
      min_age = min_ages,
      max_age = max_ages
    ),
    c(67, expected_age_2nd, 58)
  )
})

test_that("min_age validation works correctly", {
  expect_error(
    age_from_chi(
      "0101336489",
      ref_date = as.Date("2025-01-01"),
      min_age = 0:10
    ),
    regexp = "must be size 1"
  )

  expect_error(
    age_from_chi(
      c(
        "0101336489",
        gen_real_chi(150790),
        gen_real_chi(150190)
      ),
      min_age = c(20, 50)
    ),
    regexp = "must be size 3"
  )
})

test_that("max_age validation works correctly", {
  expect_error(
    age_from_chi(
      "0101336489",
      max_age = 1:10
    ),
    regexp = "must be size 1"
  )

  expect_error(
    age_from_chi(
      c(
        "0101336489",
        gen_real_chi(150790),
        gen_real_chi(150190)
      ),
      max_age = 1:10
    ),
    regexp = "must be size 3"
  )
})

test_that("ref_date validation w correctly", {
  expect_error(
    age_from_chi(
      "0101336489",
      ref_date = seq.Date(
        as.Date("1990-01-01"),
        as.Date("2000-01-01"),
        by = "year"
      )
    ),
    regexp = "must be size 1"
  )

  expect_error(
    age_from_chi(
      c(
        "0101336489",
        gen_real_chi(150790),
        gen_real_chi(150190)
      ),
      ref_date = seq.Date(
        as.Date("1990-01-01"),
        as.Date("2000-01-01"),
        by = "year"
      )
    ),
    regexp = "must be size 3"
  )
})

test_that("Checking types works", {
  expect_error(
    age_from_chi(gen_real_chi(150790), min_age = "12"),
    "`min_age` must be a "
  )

  expect_error(
    age_from_chi(gen_real_chi(150790), max_age = "50"),
    "`max_age` must be a "
  )
})

test_that("Context-aware messaging suggests min_age/max_age when called from age_from_chi", {
  # Test that when age_from_chi calls dob_from_chi, it suggests min_age/max_age
  expect_message(
    age_from_chi(gen_real_chi(010101)),
    regexp = "Try different values for.*min_age.*max_age"
  )

  # Test that the base message is still correct
  expect_message(
    age_from_chi(gen_real_chi(010101)),
    regexp = "1 CHI number produced an ambiguous date"
  )

  # Test with multiple CHI numbers
  expect_message(
    age_from_chi(c(gen_real_chi(010101), gen_real_chi(010110))),
    regexp = "2 CHI numbers produced ambiguous dates"
  )

  expect_message(
    age_from_chi(c(gen_real_chi(010101), gen_real_chi(010110))),
    regexp = "Try different values for.*min_age.*max_age"
  )
})

test_that("NA value handling works correctly", {
  # Test ref_date with NA values - need matching vector lengths
  expected_age_na <- age_calculate(as.Date("1933-01-01"), Sys.Date())

  expect_equal(
    age_from_chi(
      c("0101336489", "0101336489"),
      ref_date = c(as.Date("2023-01-01"), as.Date(NA)),
      min_age = 0,
      max_age = 150
    ),
    c(90, expected_age_na) # Should use today's date for NA ref_date
  )

  # Test min_age with NA values
  expect_equal(
    age_from_chi(
      "0101336489",
      ref_date = as.Date("2023-01-01"),
      min_age = NA_integer_, # Should default to 0
      max_age = 150
    ),
    90
  )

  # Test max_age with NA values (should use age from 1900-01-01)
  result_na_max <- age_from_chi(
    "0101336489",
    ref_date = as.Date("2023-01-01"),
    min_age = 0,
    max_age = NA_integer_
  )
  expect_true(is.numeric(result_na_max))
  expect_false(is.na(result_na_max))
})

test_that("Vector length validation works correctly", {
  # Test when ref_date length doesn't match chi_number length
  expect_error(
    age_from_chi(
      c("0101336489", "0101405073"),
      ref_date = c(
        as.Date("2023-01-01"),
        as.Date("2023-01-02"),
        as.Date("2023-01-03")
      )
    ),
    "must be size 2.*not 3"
  )

  # Test when max_age length doesn't match chi_number length
  expect_error(
    age_from_chi(
      c("0101336489", "0101405073"),
      max_age = c(100, 110, 120)
    ),
    "must be size 2.*not 3"
  )

  # Test when min_age length doesn't match chi_number length
  expect_error(
    age_from_chi(
      c("0101336489", "0101405073"),
      min_age = c(0, 5, 10)
    ),
    "must be size 2.*not 3"
  )

  # Test single chi with multiple ref_dates (should error)
  expect_error(
    age_from_chi(
      "0101336489",
      ref_date = c(as.Date("2023-01-01"), as.Date("2023-01-02"))
    ),
    "must be size 1.*not 2"
  )
})
