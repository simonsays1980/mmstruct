"computeKokotLik" <- function(data, par, T, methodLik = c("precise", "approx")) {

	## Check if correct argument is given ##
	method <- match.arg(methodLik)
	
	## Transform parameters ##
	lalpha 		<- exp(par[1])/(1 + exp(par[1]))
	lepsilon 	<- par[2]
	lmu 		<- par[3]
	
	## Allocate containers to hold values ##
	tradeter1 	<- matrix(0, NROW(data), 1)
	tradeter2 	<- matrix(0, NROW(data), 1)
	
	## Compute Likelihood ##
	for (i in 1:NROW(data)) {
		tradeter1[i] <- data[i] * log(2 * lepsilon * T) - lgamma(data[i] + 1)
		tradeter2[i] <- data[i] * log((2 * lepsilon + lmu) * T) - lgamma(data[i] + 1)
	}
	
	epster1 	<- (-2 * lepsilon * T)
	epster2 	<- (-(2 * lepsilon + lmu) * T) 
	sum1 		<- (1 - lalpha) * exp(epster1 + tradeter1)
	sum2 		<- lalpha * exp(epster2 + tradeter2)

	likl <- sum(log((sum1 + sum2)))
	if(method == "approx") {
		likl[likl == -Inf] <- -1e+6
	}

	return(likl)
}

"computeKokotLikGrad" <- function(data, par, T, methodLik = c("precise", "approx")) {

	## Check if correct argument is given ##
	method <- match.arg(methodLik)
	
	grad 		<- c(0, 0, 0)
	tradeter1	<- matrix(0, NROW(data), 1)
	tradeter2 	<- matrix(0, NROW(data), 1)
	
	## Transform parameters ##
	lalpha 		<- exp(par[1])/(1 + exp(par[1]))
	lepsilon 	<- par[2]
	lmu 		<- par[3]
	
	## Compute Terms for likelihood and derivatives ##
	for(i in 1:NROW(data)) {
		tradeter1[i] 	<- data[i] * log(2 * lepsilon * T) - lgamma(data[i] + 1)
		tradeter2[i] 	<- data[i] * log((2 * lepsilon + lmu) * T) - lgamma(data[i] + 1) 
	}
	epster1 	<- (-2 * lepsilon * T)
	epster2 	<- (-(2 * lepsilon + lmu) * T)
	sum1		<- (1 - lalpha) * exp(epster1 + tradeter1)
	sum2 		<- lalpha * exp(epster2 + tradeter2)
	sum3 		<- data/(2 * lepsilon * T) - 1
	sum4		<- data/((2 * lepsilon + lmu) * T) - 1
	
	likl 		<- sum((sum1 + sum2))
	if(method == "approx") {
		likl[likl == -Inf] <- -1e+6
	}
	
	## Derivative towards lalpha ##
	grad[1] 	<- sum(sum2 - sum1)
	
	## Derivative towards lepsilon ##
	grad[2]		<- sum(sum1 * 2 * T * sum3 + sum2 * 2 * T * sum4)
	
	## Derivative towards lmu ##
	grad[3]		<- sum(sum2 * T * sum4)
	
	return(grad / likl)
}
