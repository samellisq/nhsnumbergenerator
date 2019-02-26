#####################################################################################
## ADDITIONAL FUNCTIONS
# THESE FUNCTIONS ARE NECESSARRY FOR THE FUNCITONING OF THE MAIN AND HELPER FUNCTIONS BUT ARE NOT
#FOUND IN I STENSON'S PYTHON PACKAGE
#####################################################################################

#' Convert a string-number to have 9 digits
#'
#' NHS numbers must have nine digits. This function coverts numbers with fewer digits to having nine digits
#' by prefixing 0's. Numbers already with nine digits are unchanged. Numbers with more than nine digits give
#' an error.
#'
#' @param nhsnumber a potential NHS number or vector of potential NHS numbers.
#' @return Nine-digit potential NHS number as string
#' @examples
#' nine_digitify("01")
#' nine_digitify(c("01","02"))
#' nine_digitify("000000001") # returns unchanged


nine_digitify = function(nhsnumber){

  R=length(nhsnumber)
  output.nhsnumber=character(R)
  for(i in 1:R){

    nhsnum = as.character(nhsnumber[i])

    if(nchar(nhsnum)>9){
      stop("This number has too many digits and cannot be an NHS number")
    }

    if(nchar(nhsnum) == 9){
      output.nhsnumber[i]=nhsnum
    } else {
      missing.digits=paste(rep.int("0",9-nchar(nhsnum)),sep="", collapse = "")
      output.nhsnumber[i] = paste(missing.digits,nhsnum,sep="")
    }

  }

  return(output.nhsnumber)

}

#' Samples from two or more ranges
#'
#' Selects a single value at random from two or more ranges.
#'
#' @param listofranges Ranges from which a value should be selected. Each list item should be a vector of the
#' form \code{c(lowerbound,upperbound)}.
#' @return a numeric value selected at random from the input ranges
#' @examples
#' sample_from_ranges(list(c(400000000, 499999999),c(600000000, 708800001)))

sample_from_ranges = function(listofranges){

  for(i in 1:length(listofranges)){
    if(!is.numeric(listofranges[[i]][1])|!is.numeric(listofranges[[i]][2])){stop("Range bounds must be numeric")}
    if(listofranges[[i]][1]%%1 !=0 |listofranges[[i]][2]%%1 !=0 ){stop("Range bounds must be integers")}
    if(listofranges[[i]][2]<=listofranges[[i]][1]){stop("The high end of the range should not be lower than the low end.")}
  }


  range_width = unlist(lapply(listofranges,diff)) + length(listofranges)

  #random.index = sample(1:sum(range_width),1)
  random.index = round(runif(1,1,sum(range_width)),0)
  index.list  = min(which(cumsum(range_width)>=random.index))

  if(index.list==1){
    index_correction = 0
  } else {
    index_correction = cumsum(range_width)[index.list -1]
  }


  random.value = min(listofranges[[index.list]]) + (random.index - index_correction - 1)

  return(random.value)

}
