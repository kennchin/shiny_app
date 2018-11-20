convert_date <- function(x) {
  no_space = gsub('\\s+', '', x)
  formatted= dmy(no_space)
  return(as.character(formatted))
}