write_and_return_path <- function(x) {
  path <- paste0("output/", deparse(substitute(x)), ".csv")
  write_csv(x, path)
  return(path)
}