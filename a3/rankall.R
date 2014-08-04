## rankall.R

rankall <- function(outcome, num="best") {
	## Read outcome data
	d <- read.csv(file="outcome-of-care-measures.csv", colClasses="character")

	## Check for valid outcome parameter and select relevant columns
	if (outcome=="heart attack") {
		d.outcome <- d[,c(2,7,11)]
	} else if (outcome=="heart failure") {
		d.outcome <- d[,c(2,7,17)]
	} else if (outcome=="pneumonia") {
		d.outcome <- d[,c(2,7,23)]
	} else {
		stop("invalid outcome")
	}

	## Parse numeric column, suppress NA warnings
	names(d.outcome) <- c("hospital","state", "mort")
	suppressWarnings(d.outcome$mort <- as.numeric(d.outcome$mort))

	## Split by state (list by state of list of three vectors)
	state.lists <- split(d.outcome,d.outcome$state)

	## Get list of numth by state
	state.numths <- sapply(state.lists, numth, num, simplify = "array")
	states <- names(state.numths)

	data.frame(hospital=state.numths,state=states, row.names=states)

	## Build up data frame by column. Bit ugly
	## s <- as.data.frame(names(state.lists), stringsAsFactors = FALSE)
	## names(s) <- c("state")
	## h <- as.data.frame(state.numths, stringsAsFactors = FALSE)
	## names(h) <- c("hospital")
	## data.frame(h, s, row.names = s$state)
}

## Given res, a list of three equal size vectors named
## hospital (chr), state (chr), mort (num) and a
## num like the argument above,
## return the numth hospital

numth <- function(res, num) {
	ord <- order(res$mort, res$hospital, na.last = NA)
	rank.wanted <- convnum(num,length(ord))
	res$hospital[ord[rank.wanted]]
}

## Convert num into an integer
convnum <- function(num, bottom) {
	if (num=="best") 1
	else if (num=="worst") bottom
	else {
		rank.wanted <- as.integer(num)
		if (!is.integer(rank.wanted)) stop("invalid num")
		rank.wanted
	}
}
