# _common_date.R contains common date functions

# isConvertibleToDate checks if a provided string can be converted to a date
isConvertibleToDate <- function(x) {
  return (!is.na(as.Date(as.character(x), origin='1970-01-01')))
}

# convertToDate converts a provided string to a date
# Note this returns a Date object not a string
# The date object is basically an integer number which corresponds to the day
# To display it can be cast to a string with convertToDateStr() or it does this automatically when adding to data.frame
convertToDate <- function(x) {
  if (isConvertibleToDate(x)) {
    r=as.Date(x,format=Sys.getenv("DATE_FORMAT"), origin='1970-01-01')
  } else {
    r=NA
  }
  return (r)
}                                                    

# This is redundant currently
convertToDateStr <- function(x) {
  return (as.character(convertToDate(x)))
}                                                    
