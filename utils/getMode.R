get_mode <- function(value){
  return(names(sort(table(value), decreasing = T, na.last = T)[1]))
}