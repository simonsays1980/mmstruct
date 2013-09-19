"estModelKokotML" <- function(data, startpar, T = 390, methodLik = c("precise", "approx")) {
	
	if (missing(startpar)) {
		cat("Using default starting values...\n")
		tmp         <- mean(data, na.rm = TRUE)/T
		startpar 	<- c(0, tmp * 0.75/2, tmp * 0.25/2)
	}

	## optimization settings ##
	optim_fn 	    <- computeKokotLik
	optim_Method 	<- "L-BFGS-B"
	optim_lower 	<- c(-1e+6, 0, 0)
	optim_upper 	<- c(1e+6, 1e+6, 1e+6)
	optim_fnscale 	<- -1 
	optim_maxit 	<- 200
	optim_ctrl	    <- list(fnscale = optim_fnscale, maxit = optim_maxit)
	 
	optim_res <- optim(par = startpar, fn = optim_fn, data = data, T = T, 
				methodLik = methodLik, method = optim_Method, 
				lower = optim_lower, upper = optim_upper, 
				control = optim_ctrl, hessian  = TRUE)
	return(optim_res)	
}

"estModelEKOPML" <- function(data, startpar, T = 390, methodLik = c("precise", "approx"),
                             sim = FALSE, mis = FALSE, mis.prob, misInd) {
	
	if (missing(startpar)) {
		cat("Using default starting values...\n")
		tmp         <- mean(data[, 5], na.rm = TRUE)/T
		startpar 	<- c(0, tmp * 0.75/2, 0, tmp * 0.25/2)
	}

	## optimization settings ##
	optim_fn 	    <- computeEKOPLik
	optim_Method 	<- "L-BFGS-B"
	optim_lower 	<- c(-1e+6, 0, -1e+6, 0)
	optim_upper 	<- c(1e+6, 1e+6, 1e+6, 1e+6)
	optim_fnscale 	<- -1 
	optim_maxit 	<- 200
	optim_ctrl	    <- list(fnscale = optim_fnscale, maxit = optim_maxit, trace = 6)
	 
	optim_res <- optim(par = startpar, fn = optim_fn, data = data, T = T, 
				methodLik = methodLik, method = optim_Method, 
				lower = optim_lower, upper = optim_upper, 
				control = optim_ctrl, hessian  = TRUE)
	return(optim_res)	
}


