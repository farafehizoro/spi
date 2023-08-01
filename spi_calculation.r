# calculate Standardized precipitation index 
# Author: Farafehizoro
# Date : august 01st, 2023


spi_gamma <- function(rain, ref_dat) #calculate SPI based on a gamma distribtion
{
	#param <- fitdist(ref_dat,distr = "gamma")
	ref_dat2 <- ref_dat[which( !is.na(ref_dat))]
	pzero <- sum (ref_dat2 == 0) / length(ref_dat2)
	ref_dat2_nozero <- ref_dat2[ref_dat2 > 0]
	param <- tryCatch({
		fitdist(ref_dat2_nozero,distr = "gamma", method= "mle", lower= c(0,0))
		}, error = function(err){
		fitdist(ref_dat2_nozero,distr = "gamma", method= "mme", lower= c(0,0))
		})
	rr_shape <- param$estimate[1]
	rr_rate <- param$estimate[2]
	prob_rr <- pgamma(rain, rr_shape, rr_rate ) 
	spi_res <- qnorm(pzero + (1 - pzero) *prob_rr)
	return (spi_res)
}

