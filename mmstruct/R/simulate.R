"simulateEKOP" <- function(size = 1000, alpha = 0.2, epsilon = 1.3, delta = 0.5, mu = 0.02, T = 60 * 6.5) {
			sample <- .Call("simulateEKOP_cc", as.integer(size), as.double(alpha), as.double(epsilon), 
						as.double(delta), as.double(mu), as.double(T))
			colnames <- c("Mis", "Buy", "Sell", "Trades")
			sample <- data.frame(sample)
			colnames(sample) <- colnames

			return(sample)
}

"simulateEKOPMis" <- function(size = 1000, alpha = 0.2, epsilon = 1.3, delta = 0.5, mu = 0.02, T = 60 * 6.5, mis = 0.15) {
			sample <- .Call("simulateEKOPMis_cc", as.integer(size), as.double(alpha), as.double(epsilon),
					as.double(delta), as.double(mu), as.double(T), as.double(mis))
		
			colnames <- c("Mis", "Buy", "Sell", "Trades")
			colnames(sample) <- colnames

			return(sample)
}
