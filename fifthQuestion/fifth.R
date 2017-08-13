
convert <- function (Temp_val, Convert_to) {
  if (Convert_to != 'F' & Convert_to != 'C') {
    stop("Convert_to must be either 'F' or 'C'")
  }
  if ( Convert_to== 'F') {
    return ((Temp_val * 1.8) + 32)
  } else if (Convert_to == 'C') {
    return ((Temp_val - 32) / 1.8)
  }
}

tryCatch(expr = convert(Temp_val = 22, Convert_to = 'D') ,error= function(e) {print(paste("Expected Error: ", e))})

cat("convert(68, 'C'): ", convert(Temp_val = 68, Convert_to = 'C'), "\n")

cat("convert(20, 'F'): ", convert(Temp_val = 20, Convert_to = 'F'), "\n")

cat("convert(20, 22, 24, 26, 'F'): ", convert(Temp_val = c(20, 22, 24, 26), Convert_to = 'F'), "\n")

cat("convert(32, 68, 72, 100, 'C'): ", convert(Temp_val = c(32, 68, 72, 100), Convert_to = 'C'), "\n")


