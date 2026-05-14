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

test_that("Returns correct age - no options except fixed reference date", {
  # Some standard CHIs
  expect_equal(
    age_from_chi(
      c(
        "0101336489",
        "0101405073",
        "0101625707"
      ),
      ref_date = as.Date("2023-11-01")
    ),
    c(90, 83, 61)
  )

  # Leap years
  expect_equal(
    age_from_chi(
      c(
        gen_real_chi(290228),
        gen_real_chi(290236),
        gen_real_chi(290296)
      ),
      ref_date = as.Date("2023-03-01")
    ),
    c(95, 87, 27)
  )

  # Century leap year (hard to test as 1900 is a long time ago!)
  expect_equal(
    age_from_chi(gen_real_chi(290200), ref_date = as.Date("2023-03-01")),
    23
  )
})

test_that("Returns correct age - fixed age and reference date supplied", {
  # Some standard CHIs
  # Fixed min age e.g. All patients are younger than X
  expect_equal(
    age_from_chi(
      c(
        "0101336489",
        "0101405073",
        "0101625707"
      ),
      min_age = 1,
      max_age = 101,
      ref_date = as.Date("2023-11-01")
    ),
    c(90, 83, 61)
  )
})

test_that("Returns correct age - unusual fixed age with fixed reference date", {
  # Some standard CHIs
  expect_message(
    expect_equal(
      age_from_chi(
        c(
          "0101336489",
          "0101405073",
          "0101625707"
        ),
        max_age = 72,
        ref_date = as.Date("2023-11-01")
      ),
      c(NA_real_, NA_real_, 61)
    ),
    "2 CHI numbers produced ambiguous dates"
  )
})

test_that("Returns NA when DoB is ambiguous so can't return age", {
  # Default is min_age as 0. max_age is NULL and will be set to the age from 1900-01-01.
  expect_message(
    expect_equal(
      age_from_chi(gen_real_chi(010101)),
      NA_integer_
    ),
    regexp = "1 CHI number produced an ambiguous date"
  )

  expect_message(
    expect_equal(
      age_from_chi(c(
        gen_real_chi(010101),
        gen_real_chi(010110),
        gen_real_chi(010120)
      )),
      c(NA_real_, NA_real_, NA_real_)
    ),
    regexp = "3 CHI numbers produced ambiguous dates"
  )
})

test_that("Can supply different reference dates per CHI", {
  # Some standard CHIs / dates
  # Reference date per CHI, e.g. Date of discharge
  expect_equal(
    age_from_chi(
      c(
        "0101336489",
        "0101405073",
        "0101625707"
      ),
      ref_date = as.Date(c(
        "1950-01-01",
        "2000-01-01",
        "2020-01-01"
      ))
    ),
    c(17, 60, 58)
  )
})

test_that("age_from_chi errors properly", {
  expect_error(
    age_from_chi(1010101129),
    regexp = "`chi_number` must be a <character> vector, not a <numeric> vector\\.$"
  )

  expect_error(
    age_from_chi("0101625707", ref_date = "01-01-2020"),
    regexp = "`ref_date` must be a <Date> or <POSIXct> vector, not a <character> vector\\.$"
  )

  expect_error(
    age_from_chi("0101625707", min_age = -2),
    regexp = "`min_age` must be a positive integer\\.$"
  )

  expect_error(
    age_from_chi("0101625707", min_age = 20, max_age = 10),
    regexp = "`max_age`, must always be greater than or equal to `min_age`\\.$"
  )
})

test_that("age_from_chi gives messages when returning NA", {
  # Invalid CHI numbers
  expect_message(age_from_chi("1234567890"), regexp = "1 CHI number is invalid")

  expect_message(
    age_from_chi(rep("1234567890", 99999)),
    regexp = "99,999 CHI numbers are invalid"
  )
})

test_that("age_from_chi returns correct age when chi_check = FALSE", {
  ref_date <- as.Date("2024-01-01")

  # Test with a valid CHI
  expect_equal(
    age_from_chi(gen_real_chi(010185), ref_date = ref_date, chi_check = FALSE),
    39
  )

  # Test with an invalid CHI (should return NA as the date part is invalid)
  expect_message(
    expect_equal(
      age_from_chi("1234567890", ref_date = ref_date, chi_check = FALSE),
      NA_real_ # age_calculate returns numeric NA
    )
  )

  # Test with a mix of valid and invalid CHIs
  mixed_chis <- c(gen_real_chi(010185), "1234567890", gen_real_chi(150790))
  expect_message(
    expect_equal(
      age_from_chi(mixed_chis, ref_date = ref_date, chi_check = FALSE),
      c(39, NA_real_, 33)
    )
  )

  expect_message(
    expect_equal(
      age_from_chi(
        mixed_chis,
        ref_date = ref_date,
        max_age = 35,
        chi_check = FALSE
      ),
      c(NA_real_, NA_real_, 33)
    )
  )
})

test_that("Edge case: negative min_age", {
  expect_error(
    age_from_chi("0101336489", min_age = -1),
    "must be a positive integer"
  )
})

test_that("Edge case: max_age less than min_age", {
  expect_error(
    age_from_chi("0101336489", min_age = 50, max_age = 30),
    "must always be greater than or equal"
  )
})
