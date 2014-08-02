## rankhospital.R

rankhospital <- function(state, outcome, num="best") {
	## Read outcome data
	d <- read.csv(file="outcome-of-care-measures.csv", colClasses="character")

	## Check for valid state parameter
	states = d[,7]
	if (!any(states==state)) stop("invalid state")

	## Check for valid outcome parameter and select relevant rows and columns
	if (outcome=="heart attack") {
		d.outcome <- subset(d, State==state, select=c(2,11))
	} else if (outcome=="heart failure") {
		d.outcome <- subset(d, State==state, select=c(2,17))
	} else if (outcome=="pneumonia") {
		d.outcome <- subset(d, State==state, select=c(2,23))
	} else {
		stop("invalid outcome")
	}

	## Parse numeric column, suppress NA warnings
	names(d.outcome) <- c("Name","Mort")
	suppressWarnings(d.outcome$Mort <- as.numeric(d.outcome$Mort))

	## Find order by Mortality and then by Name, both ascending, suppressing NAs
	d.outcome.order <- order(d.outcome$Mort, d.outcome$Name, na.last=NA)

	## Reorder the data, also suppressing NAs
	d.outcome <- d.outcome[d.outcome.order,]

	## Get name of hospital or requested rank in that order
	## If num is bigger than filtered data, return NA
	if (num=="best") {
		d.outcome$Name[1]
	} else if (num=="worst") {
		d.outcome$Name[nrow(d.outcome)]
	} else {
		rank.wanted <- as.integer(num)
		if (!is.integer(rank.wanted)) stop("invalid num")
		d.outcome$Name[rank.wanted]
	}
}
