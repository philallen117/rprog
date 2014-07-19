pollutantmean <- function(directory, pollutant, id = 1:332) {
	## 'directory' is a character vector of length 1 indicating
	## the location of the CSV files
	## 'pollutant' is a character vector of length 1 indicating
	## the name of the pollutant for which we will calculate the
	## mean; either "sulfate" or "nitrate".
	## 'id' is an integer vector indicating the monitor ID numbers
	## to be used
	## Return the mean of the pollutant across all monitors list
	## in the 'id' vector (ignoring NA values)
	data.in.range <- data.frame(Date=as.Date(character()),
								sulfate=numeric(),
								nitrate=numeric(),
								ID=numeric(),
								stringsAsFactors=FALSE)
	for (i in id) {
		fname <- paste(directory, "/", sprintf("%03d", i), ".csv", sep="")
		data <- read.csv(fname)
		data.in.range <- rbind(data.in.range, data)
	}
	mean(data.in.range[,pollutant], na.rm=TRUE)
}
