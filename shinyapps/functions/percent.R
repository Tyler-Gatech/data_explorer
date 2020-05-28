#function to change decimal to data to percentage format
percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = "f", digits = 1), "%")
}
