
#' Data frame of weights given to each digit position when calcualting NHS number check digits
#'
#' NHS numbers are 9 digits followed by a check digit. The check digit is calculated by an algorithim
#' that associates each digit by its \code{digit.position} -from left to right- with a \code{weight}. The digit positions
#' and their associated weights are shown in this data frame.
#'
#' @format A data frame with 9 rows and 2 variables
#' \describe{
#' \item{digit.position}{indexed position from left to right in an NHS number}
#' \item{weight}{the weight assinged to that value in the check digit alogoritm}
#' }
#'
#' @source https://www.datadictionary.nhs.uk/data_dictionary/attributes/n/nhs/nhs_number_de.asp


checkdigit_weights = data.frame(digit.position=seq(from=1, to = 9, by =1),
                                weight=seq(from=10, to = 2, by =-1)
)


#' Calculate NHS Number check digit
#'
#' Given the first 9 or 10 digits of a 10-digit NHS number, calculates what the check digit should be.
#'
#' This procedure is calcualted by the method described in
#'  https://www.datadictionary.nhs.uk/data_dictionary/attributes/n/nhs/nhs_number_de.asp \cr
#'  \cr
#'  A summary of this method is:\cr
#'  1. Multiply each of the first nine digits by a weighting factor\cr
#'  2. Add the results of each multiplication together.\cr
#'  3. Divide the total by 11 and establish the remainder.\cr
#'  4. Subtract the remainder from 11 to give the check digit.\cr
#'  NB. Check digits of 11 are converted to 0.
#'
#'
#'
#' @param nhsnumber A 9 or 10 digit NHS Number. Parsed as a string.\cr If a 10 digit NHS number is inputed only the first 9 digits
#' are used to calcualte the check digit. If the number is valid the integer produced will match the 10th digit of the numbers
#'
#' @return The check digit as an integer. \cr \cr If the check digit is less than 10 the supplied \code{nhsnumber} is valid.
#'  A check digit of 10 means the supplied \code{nhsnumber} is invalid
#'
#'@examples
#'  calculate_check_digit("000000000") # Returns 0
#'  calculate_check_digit("411326996") #Returns 3
#'
#'
#' @source https://www.datadictionary.nhs.uk/data_dictionary/attributes/n/nhs/nhs_number_de.asp
#'

calculate_check_digit=function(nhsnumber){

  #Step 1) Multiply each of the first nine digits by a weighting factor
  nhsnumber.df=data.frame(position = seq(1,9,1),nhsnumber.digits=as.numeric((stringr::str_split(string = nhsnumber, pattern = "")[[1]])[1:9]),weight = as.numeric(checkdigit_weights$weight))
  nhsnumber.df$products = nhsnumber.df$nhsnumber.digits*nhsnumber.df$weight

  #Step 2) Add the results of each multiplication together.
  sum_products = sum(nhsnumber.df$products)

  # Step 3) Divide the total by 11 and establish the remainder.
  remainder = sum_products %% 11

  # Step 4) Subtract the remainder from 11 to give the check digit.
  eleven_minus_remainder = 11 - remainder

  # If the result is 11 then a check digit of 0 is used. If the result is 10 then the NHS number is invalid.
  if(eleven_minus_remainder == 11){
    check_digit = 0
  } else {
    check_digit = eleven_minus_remainder
  }


  return(check_digit)
}


#' Generate NHS numbers in sequence
#'
#' Returns a predictable sequence of 10-digit NHS numbers within the specified range.
#'
#' The default ranges are the ones currently issued in England, Wales and the Isle of Man.  Numbers outside of this
#' range may be valid but could conflict with identifiers used in Northern Ireland and Scotland.
#' See https://en.wikipedia.org/wiki/NHS_number
#'
#' @param n number of nhs numbers to return
#' @param range Numeric range in which the sequence NHS numbers is derived.\cr
#' Input as a vector detailing the limits of the range (inclusive) e.g. \code{c(lower.bound, upper.bound)}. For multiple non-consecutive ranges
#' input a list of vectors. \cr
#' Ranges must not be more than 9 digits long. \cr
#' When \code{range} is \code{NULL} (default) the sequence is constructed from the ranges currently issued in England, Wales and the Isle of Man.
#' That is 400000000-499999999 and 600000000-708800001.
#' @return A vector of nhs number strings
#' @examples
#' deterministic_nhs_number(n=100)
#' deterministic_nhs_number(n=100, nhsnums.range=c(100000000,200000000))
#' deterministic_nhs_number(n=100, nhsnums.range=list(c(100000000,200000000),c(300000000,400000000)))

deterministic_nhs_number = function(n, nhsnums.range=NULL){

  if(typeof(nhsnums.range)!="list" & !is.null(nhsnums.range)){
    nhsnums.range = list(as.integer(nhsnums.range))
    nhsnums.range = lapply(nhsnums.range, nine_digitify)
  }


  if(is.null(nhsnums.range)){
    nhsnums.range = list(c(400000000, 499999999),c(600000000, 708800001))
    seed.value = as.integer(min(nhsnums.range[[1]]))
  } else{
    seed.value = as.integer(min(nhsnums.range[[1]]))
  }

  for(i in 1:length(nhsnums.range)){
    if(nhsnums.range[[i]][2]<=nhsnums.range[[i]][1]){stop("The high end of the range should not be lower than the low end.")}
  }

  range.list.incrementer=1

  nhsnumbers.list=list()

  while(length(nhsnumbers.list) < n) {

    candidate_number = as.character(as.integer(seed.value))
    check_digit = calculate_check_digit(candidate_number)

    if(check_digit!=10){
      nhsnumber = paste(seed.value,check_digit,"",sep="")
      nhsnumbers.list[[length(nhsnumbers.list)+1]]=nhsnumber

    }

    if(seed.value<=max(nhsnums.range[[range.list.incrementer]])){
      seed.value=as.integer(as.integer(seed.value)+1)
    } else {
      if(range.list.incrementer<length(nhsnums.range)){
        range.list.incrementer=range.list.incrementer+1
        seed.value = as.integer(min(nhsnums.range[[range.list.incrementer]]))
      } else {
        stop("There are not enough NHS numbers in the range specified. Try a wider range")
      }
    }


  }

  return(unlist(nhsnumbers.list))

}



#' Generate NHS numbers in an unpredictable sequence
#'
#' Returns a generator for an unpredictable sequence of 10-digit NHS numbers.
#'
#' \code{deterministic_nhs_number} produces
#' NHS numbers in an ascending sequence until n nhs numbers are reached. In contrast, \code{random_nhs_number}
#' generates nhsnumbers at random in the range specified until n is reached.
#'
#' The default ranges are the ones currently issued in England, Wales and the Isle of Man.  Numbers outside of this
#' range may be valid but could conflict with identifiers used in Northern Ireland and Scotland.
#' See https://en.wikipedia.org/wiki/NHS_number
#'
#' @param n number of nhs numbers to return
#' @param range Numeric range in which the sequence NHS numbers is derived.\cr
#' Input as a vector detailing the limits of the range (inclusive) e.g. \code{c(lower.bound, upper.bound)}. For multiple non-consecutive ranges
#' input a list of vectors. \cr
#' Ranges must not be more than 9 digits long. \cr
#' When \code{range} is \code{NULL} (default) the sequence is constructed from the ranges currently issued in England, Wales and the Isle of Man.
#' That is 400000000-499999999 and 600000000-708800001.
#' @return A vector of nhs number strings
#' @examples
#' random_nhs_number(10)
#' random_nhs_number(10, nhsnums.range = c(100000000,200000000))
#' random_nhs_number(10, nhsnums.range = list(c(100000000,200000000),c(300000000,400000000)))

random_nhs_number = function(n, nhsnums.range=NULL){

  if(typeof(nhsnums.range)!="list" & !is.null(nhsnums.range)){
    nhsnums.range = list(as.integer(nhsnums.range))
    nhsnums.range = lapply(nhsnums.range, nine_digitify)
    nhsnums.range = lapply(nhsnums.range, as.integer)
    #print(nhsnums.range)
  }


  if(is.null(nhsnums.range)){
    nhsnums.range = list(c(400000000, 499999999),c(600000000, 708800001))
  }

  for(i in 1:length(nhsnums.range)){
    if(nhsnums.range[[i]][2]<=nhsnums.range[[i]][1]){stop("The high end of the range should not be lower than the low end.")}
  }



  nhsnumbers.list=list()
  rejectednumbers.list = list()

  random_from_range = sample_from_ranges(nhsnums.range)

  total_range_width =sum(unlist(lapply(nhsnums.range,diff)) + length(nhsnums.range))



  while(length(nhsnumbers.list) < n) {

    candidate_number = as.character(as.integer(random_from_range))
    check_digit = calculate_check_digit(candidate_number)
    nhsnumber = paste(random_from_range,check_digit,"",sep="")

    if(check_digit!=10 & !(nhsnumber %in% nhsnumbers.list)){
      nhsnumbers.list[[length(nhsnumbers.list)+1]]=nhsnumber
    }

    if(check_digit==10){
      rejectednumbers.list[[length(rejectednumbers.list)+1]] = nhsnumber
    }

    random_from_range = sample_from_ranges(nhsnums.range)


    if((length(nhsnumbers.list)+length(rejectednumbers.list)) == total_range_width){
      stop("There are not enough NHS numbers in the range specified. Try a wider range")
    }

  }

  return(unlist(nhsnumbers.list))

}

#' Add seperators into an NHS number
#'
#' Adds seperator an NHS numer in the form 123 456 7890. Spaces are usually used to seperate digit-blocks
#' but other speators can be choosen
#'
#' @param nhsnumber nhsnumber as string
#' @param separator Character used to seperate digit-blocks. Default as space.
#' @return nhs number as string with seperators after 3 and 6 characters
#' @examples
#' add_separators("4000000004")
#' add_separators("4000000004",separator = "-")

add_separators = function(nhsnumber, separator =" "){

  if(nchar(nhsnumber) == 10){
    split_nhsnumber = stringr::str_split(nhsnumber,"")[[1]]
    nhsnumber_with_seps = paste(split_nhsnumber[1], split_nhsnumber[2],split_nhsnumber[3], separator,
                                split_nhsnumber[4],split_nhsnumber[5],split_nhsnumber[6], separator,
                                split_nhsnumber[7],split_nhsnumber[8],split_nhsnumber[9],split_nhsnumber[10],
                                sep="")
  } else{
    stop("Incorrect number of digits. NHS numbers need 10 digits.")
  }

  return(nhsnumber_with_seps)

}

#' Remove seperators from an NHS number
#'
#' Removes all characters from a potential NHS number that are not numbers (0-9).
#'
#' A warning is produced if the output has the wrong number of digits to be a valid
#' NHS number (10).
#'
#' @param nhsnumber nhsnumber as string
#' @param suppress_warnings Logical. Warnings of the incorrect digit numbers are or are not supressed. Default is TRUE
#' @return nhs number as string with seperators after 3 and 6 characters
#' @examples
#' remove_separators("400 000 0004")
#' remove_separators("400-000-0004")

remove_separators = function(nhsnumber, suppress_warnings = TRUE){

  split_nhsnumber = stringr::str_split(nhsnumber,"")[[1]]
  numeric_nhsnumber = split_nhsnumber[stringr::str_detect(split_nhsnumber, "[0-9]")]
  nhsnumber_without_seps = paste(numeric_nhsnumber, collapse="")

  if(nchar(nhsnumber_without_seps)!=10 & suppress_warnings == FALSE){
    warning("The input has the wrong number of digits for an NHS number")
  }

  return(nhsnumber_without_seps)

}









