context("Testing the helper functions")
library(nhsnumbergenerator)

test_that("checkdigit_weights data frame has the correct number of rows",{
  expect_equal(nrow(checkdigit_weights), 9)
})

test_that("calculate_check_digit returns an integer",{
  expect_true(calculate_check_digit(generate_nhs_number_test(10))%%1==0)
  expect_true(calculate_check_digit(generate_nhs_number_test(9))%%1==0)
})


test_that("calculate_check_digit calcualtes the correct check number",{
  expect_true(calculate_check_digit("6888011836")==6) #example nhs numbers taken from I Stenson's Python code
  expect_true(calculate_check_digit("4113269963")==3) #example nhs numbers taken from I Stenson's Python code
  expect_true(calculate_check_digit("4066638961")==1) #example nhs numbers taken from I Stenson's Python code
  expect_true(calculate_check_digit("0000000000")==0) #example nhs numbers taken from I Stenson's Python code

  expect_false(calculate_check_digit("6888011836")==1) #example nhs numbers taken from I Stenson's Python code
  expect_false(calculate_check_digit("4113269963")==1) #example nhs numbers taken from I Stenson's Python code
  expect_false(calculate_check_digit("4066638961")==0) #example nhs numbers taken from I Stenson's Python code
  expect_false(calculate_check_digit("0000000000")==1) #example nhs numbers taken from I Stenson's Python code
})

test_that("calcualte_check_digit fails when given the wrong input",{
  expect_error(supressWarnings(calculate_check_digit("a")))
  expect_error(supressWarnings(calculate_check_digit("a000000000")))
  expect_error(supressWarnings(calculate_check_digit("00000000")))
})

test_that("deterministic_nhs_number functions and produces an output of the correct length", {
  expect_equal(length(deterministic_nhs_number(10)), 10)
  expect_equal(length(deterministic_nhs_number(10, nhsnums.range = c(100000000,200000000))), 10)
  expect_equal(length(deterministic_nhs_number(10, nhsnums.range = list(c(100000000,200000000),c(300000000,400000000)))), 10)
})

test_that("deterministic_nhs_number fails when given incorrect input",{
  expect_error(deterministic_nhs_number(10, nhsnums.range = c(200000000,100000000)))
  expect_error(deterministic_nhs_number(10, nhsnums.range = c(100000000,100000000)))
  expect_error(deterministic_nhs_number(20, nhsnums.range = c(100000000,100000010)))
})

test_that("random_nhs_number functions and produces an output of the correct length", {
  expect_equal(length(random_nhs_number(10)), 10)
  expect_equal(length(random_nhs_number(10, nhsnums.range = c(100000000,200000000))), 10)
  expect_equal(length(random_nhs_number(10, nhsnums.range = list(c(100000000,200000000),c(300000000,400000000)))), 10)
})

test_that("random_nhs_number fails when given incorrect input",{
  expect_error(random_nhs_number(10, nhsnums.range = c(200000000,100000000)))
  expect_error(random_nhs_number(10, nhsnums.range = c(100000000,100000000)))
  expect_error(random_nhs_number(20, nhsnums.range = c(100000000,100000010)))
})

test_that("add_seperators adds separators",{
  expect_true(stringr::str_detect(add_separators("4000000004"), " "))
  expect_true(stringr::str_detect(add_separators("4000000004",separator = "-"), "-"))
})

test_that("add_seperators fails when given incorrect input",{
  expect_error(add_separators("400000000"))
  expect_error(add_separators("40000000040"))
})

test_that("remove_seperators removes separators",{
  expect_false(stringr::str_detect(remove_separators("400 000 0004"), " "))
  expect_false(stringr::str_detect(remove_separators("400-000-0004"), "-"))
})

test_that("remove_seperators gives a warning when output is of incorrect length", {
  expect_warning(remove_separators("400-400-400", suppress_warnings = FALSE))
})
