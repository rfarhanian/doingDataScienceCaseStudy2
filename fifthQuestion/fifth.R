
convert <- function (Temp_val, Convert_to) {
  if(Convert_to=='F') {
    return ((Temp_val*1.8)+ 32)
  } else if( Convert_to =='C') {
    return ((Temp_val-32)/1.8)
  }
  else {
    stop("Convert_to must be either 'F' or 'C'")
  }
  
}
