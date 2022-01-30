f_add_leading_0s <- function(num){
  return(ifelse(num<10, paste0("0",as.character(num)), as.character(num)))
}