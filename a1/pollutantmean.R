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
	all.filenames <- list.files(directory)
	is.file.in.range <- function (fname) {
		mon.id <- as.integer(substr(fname, start=1, stop=3))
		min(id) <= mon.id & mon.id <= max(id)
	}
	files.in.range <- all.filenames[is.file.in.range(all.filenames)]
	data.in.range <- data.frame(Date=as.Date(character()),
								sulfate=numeric(),
								nitrate=numeric(),
								ID=numeric(),
								stringsAsFactors=FALSE)
	for (file in files.in.range) {
		tmp <- read.csv(paste(directory, file, sep="/"))
		data.in.range <- rbind(data.in.range, tmp)
	}
	mean(data.in.range[,pollutant], na.rm=TRUE)
}
