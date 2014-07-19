corr <- function(directory, threshold = 0) {
	## 'directory' is a character vector of length 1 indicating
	## the location of the CSV files
	## 'threshold' is a numeric vector of length 1 indicating the
	## number of completely observed observations (on all
	## variables) required to compute the correlation between
	## nitrate and sulfate; the default is 0
	## Return a numeric vector of correlations
	cors = numeric(0)
	n.results <- 0
	for (fname in list.files(directory)) {
		data <- read.csv(paste(directory, fname, sep="/"))
		complete.data.flags <- complete.cases(data)
		if (sum(complete.data.flags) >= threshold) {
			n.results <- n.results + 1
			complete.rows <- data[complete.data.flags,]
			## Copies, eughh
			cors[n.results] <- cor(complete.rows$sulfate, complete.rows$nitrate)
		}
	}
	cors
}
