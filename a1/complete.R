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
