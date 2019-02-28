context("Testing the main (generate_nhs_number) function")
library(nhsnumbergenerator)

test_that("the output is of the correct length",{
  expect_equal(length(generate_nhs_number(n=1)),1)
  expect_equal(length(generate_nhs_number(n=10)),10)
  expect_equal(length(generate_nhs_number(n=25)),25)
})

test_that("the output is a valid nhs number",{
  expect_true(is_valid_nhs_number(generate_nhs_number(n = 1, determ = FALSE)))
  expect_true(is_valid_nhs_number(generate_nhs_number(n = 1, determ = TRUE)))
  expect_true(is_valid_nhs_number(remove_separators(generate_nhs_number(n = 1, determ = FALSE, separators = " "))))
  expect_true(is_valid_nhs_number(remove_separators(generate_nhs_number(n = 1, determ = TRUE, separators = " "))))
})

test_that("output is as expected",{
  expect_equal(generate_nhs_number(n =1, determ = TRUE), "4000000004")
  expect_equal(generate_nhs_number(n =1, determ = TRUE, nhsnumber_value_range = c(100000000,200000000)),"1000000001")

  randomnhsnum= as.numeric(generate_nhs_number(n=1))
  expect_true( ( (randomnhsnum>=4000000004 & randomnhsnum<= 4999999994) | (randomnhsnum>=6000000006 & randomnhsnum <=7088000016) ) )

  randomnhsnum= as.numeric(generate_nhs_number(n=1,nhsnumber_value_range = c(100000000,200000000)))
  expect_true((randomnhsnum>= as.integer(1000000001) & randomnhsnum<= as.integer(2000000002)))
})



test_that("is_valid_nhs_number fails  when given incorrect input",{
  expect_true(is_valid_nhs_number("0000000000"))
  expect_false(is_valid_nhs_number("0000000001", show_warnings = FALSE ))
  expect_false(is_valid_nhs_number("000000r000", show_warnings = FALSE))
  expect_false(is_valid_nhs_number("000000000", show_warnings = FALSE))
  expect_false(is_valid_nhs_number("00000000000", show_warnings = FALSE))
  expect_false(is_valid_nhs_number("0000000060", show_warnings = FALSE))

})
