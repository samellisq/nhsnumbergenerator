#usethis::use_package("dplyr")



#' Generate a sequence of NHS numbers
#'
#' This function generates 10-digit NHS numbers, which are the unique patient identifier used by the National
#'Health Service. \cr Note that, at some point point in the past, NHS numbers were 9-digits long- these cannot be gerenated
#'with this function
#'
#'@param n Integer. The number of NHS numbers to generate
#'@param determ Logical.\cr
#' If \code{TRUE} returns a predictable sequence of 10-digit NHS numbers starting at the lower bound of the specified range. See
#'  - \code{\link{deterministic_nhs_number}} for a detailed description.\cr
#' If \code{FALSE} generates NHS numbers at random within the the specified range-
#'see \code{\link{random_nhs_number}} for more details. \cr Default is \code{FALSE}.
#'@param seperators Character. Add the symbol (in " "'s) that is wanted as the seperator. Seperators are added
#' e.g. seperators = "-" : XXX-XXX-XXXX
#' @param nhsnumber_value_range Numeric range in which the sequence NHS numbers is derived.\cr
#' Input as a vector detailing the limits of the range (inclusive) e.g. \code{c(lower.bound, upper.bound)}. For multiple non-consecutive ranges
#' input a list of vectors. \cr
#' Ranges must not be more than 9 digits long. \cr
#' When \code{range} is \code{NULL} (default) the sequence is constructed from the ranges currently issued in England, Wales and the Isle of Man.
#' That is 400000000-499999999 and 600000000-708800001.
#'
#'@return Generates a vector of \code{n} 10-digit NHS numbers
#'
#'@seealso \code{\link{random_nhs_number}}\cr
#'\code{\link{deterministic_nhs_number}}
#'
#'@export

generate_nhs_number= function(n, determ = FALSE, seperators = NA, nhsnumber_value_range = NULL){

  if(determ == FALSE){
    NHSnumbers = random_nhs_number(n, nhsnums.range = nhsnumber_value_range)
  }

  if(determ == TRUE){
    NHSnumbers = deterministic_nhs_number(n, nhsnums.range = nhsnumber_value_range)
  }

  if(!is.na(seperators)){
    for(i in 1:length(NHSnumbers)){NHSnumbers[i] = add_separators(NHSnumbers[i], separator = seperators)}
  }





  return(NHSnumbers)
}



#' Check an NHS number is valid
#'
#' This function validates potential NHS numbers- checking if they fit the requriments of an NHS number.\cr
#' \code{is_valid_nhsnumber} validates several features of the ptoetial NHS number (input as string) to
#' check that it is valid. THe function checks:\cr
#' 1. That the input value is a character\cr
#' 2. That the input string is 10 characters long.\cr
#' 3. The input string contains only numbers (0-9).\cr
#' 4. The check digit (calcualted from the first 9 digits) is not 10 (incidating an invalid NHS number) \cr
#' 5. The 10th check digit is correct (matches that calcualted independtly via the published algorithin).\cr
#' \cr
#' If all these requirements are met the funtion return \code{TRUE}, if any fail it returns \code{FALSE}
#'
#'
#' @param nhsnumber potential nhs number to be validated. Input as string.
#' @param show_warnings If \code{TRUE} (default) warnings explaning why the NHS number is printed. If \code{FALSE}
#'  these warnings are not displayed.
#' @return Logical TRUE/FALSE. IF FALSE a warning will be displayed indicating the reason for the faliure.
#'
#' @examples
#' is_valid_nhs_number("0000000000") # TRUE
#' is_valid_nhs_number("0000000001") # FALSE
#' is_valid_nhs_number("0000r00000") # FALSE
#' is_valid_nhs_number("000000000") # FALSE
#'
#'
#' @export

is_valid_nhs_number = function(nhsnumber, show_warnings = TRUE){

  split.number=stringr::str_split(string = nhsnumber,pattern = "")[[1]]

  if(!is.character(nhsnumber)|nchar(nhsnumber) != 10|sum(stringr::str_detect(split.number,"[0-9]"))!=nchar(nhsnumber)){
    if(show_warnings == TRUE){warning("Input must be a string of length 10")}
    return(FALSE)
  } else {

    checkdigit = calculate_check_digit(nhsnumber)

    if(checkdigit==10){
      if(show_warnings == TRUE){warning("Invalid NHS number. Check digit is 10")}
      return(FALSE)
    }

    lastdigit= as.numeric(stringr::str_split(string = nhsnumber,pattern = "")[[1]][10])

    if(checkdigit == lastdigit){
      return(TRUE)
    } else{
      if(show_warnings == TRUE){warning("Invalid NHS number. Check digit is incorrect")}
      return(FALSE)
    }

  }

}


#' Randomly generate and NHS number test example version
#'
#' This randomly generates a random sequence of numbers of length \code{parameter}
#'
#' This a test version a simply makes an \code{parameter} digit random number. It is used in testing so should be retained
#'
#' @param parameter A number.
#' @return A random sequence of digits of length paramter
#' @examples
#' generate_nhs_number(8)
#' generate_nhs_number(2)
#'

generate_nhs_number_test= function(parameter){
  alpha = sample(1:9,parameter,replace=TRUE)
  beta = as.numeric(paste(alpha, collapse=""))
  #gamma = dplyr::tibble(beta)
  return(beta)
}

