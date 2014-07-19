complete <- function(directory, id = 1:332) {
	## 'directory' is a character vector of length 1 indicating
	## the location of the CSV files
	## 'id' is an integer vector indicating the monitor ID numbers
	## to be used
	## Return a data frame of the form:
	## id nobs
	## 1  117
	## 2  1041
	## ...
	## where 'id' is the monitor ID number and 'nobs' is the
	## number of complete cases
	all.filenames <- list.files(directory)
	mon.id <- function (fname) { as.integer(substr(fname, start=1, stop=3)) }
	is.file.in.range <- function (fname) {
		mon.id <- mon.id(fname)
		min(id) <= mon.id & mon.id <= max(id)
	}
	files.in.range <- all.filenames[is.file.in.range(all.filenames)]
	n.complete.cases <- data.frame(id=id, nobs=c(0))
	idx <- 0
	for (i in id) {
		fname <- paste(directory, "/", sprintf("%03d", i), ".csv", sep="")
		data <- read.csv(fname)
		idx <- idx + 1
		n.complete.cases[idx,"nobs"] <- sum(complete.cases(data))
	}
	n.complete.cases
}
