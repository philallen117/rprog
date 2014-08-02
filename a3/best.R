## best.R

best <- function(state, outcome) {
	## Read outcome data
	d <- read.csv(file="outcome-of-care-measures.csv", colClasses="character")

	## Check for valid state parameters
	states = d[,7]
	if (!any(states==state)) stop("invalid state")

	## Check for valid outcome parameter and select relevant rows and columns
	if (outcome=="heart attack") {
		d.outcome <- subset(d, State==state, select=c(2,11))
	}
	else if (outcome=="heart failure") {
		d.outcome <- subset(d, State==state, select=c(2,17))
	}
	else if (outcome=="pneumonia") {
		d.outcome <- subset(d, State==state, select=c(2,23))
	}
	else stop("invalid outcome")

	## Parse numeric column, suppress NA warnings
	names(d.outcome) <- c("Name","Mort")
	suppressWarnings(d.outcome$Mort <- as.numeric(d.outcome$Mort))

	## Find best (least) mortality, and all rows achieving that
	min.mort <- min(d.outcome$Mort, na.rm=TRUE)
	ties <- subset(d.outcome, d.outcome$Mort == min.mort)

	# Result is alphabetically first of those hospitals
	min(ties$Name)
}
