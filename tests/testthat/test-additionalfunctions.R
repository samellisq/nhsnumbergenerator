context("Testing the additional functions")
library(nhsnumbergenerator)


test_that("does nine_digitify extend strings nine digits when approriate",{
  expect_equal(nchar(nine_digitify("1")), 9)
  expect_equal(nchar(nine_digitify("000000000")), 9)
})

test_that("nine_digitify crashes when given a string of more than 9 digits",{
  expect_error(nine_digitify("0000000000"))
})

test_that("sample_from_ranges produces a sigle numeric output",{
  expect_equal(length(sample_from_ranges(list(c(400000000, 499999999),c(600000000, 708800001)))), 1)
  expect_false(is.na(sample_from_ranges(list(c(400000000, 499999999),c(600000000, 708800001)))))
})

test_that("sample_from_ranges fails when given incorrect input",{
  expect_error(sample_from_ranges(list(c(100,200),c(400,300))))
  expect_error(sample_from_ranges(list(c(100,200),c(300,400.2))))
  expect_error(sample_from_ranges(list(c("100","200"),c("300","400"))))
})
