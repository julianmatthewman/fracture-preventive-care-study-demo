write_lines_and_return_path <- function(x) {
	path <- paste0("output/", deparse(substitute(x)), ".txt")
	write_lines(x, path)
	return(path)
}
