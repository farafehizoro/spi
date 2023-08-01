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


spi_month <- function (dat, start_year, end_year= NULL, start_ref = NULL, end_ref= NULL, return_last_value = FALSE)
{
	if (is.null(end_year)) end_year = length(dat) + start_year - 1
	if (is.null(start_ref)) start_ref = start_year
	if (is.null(end_ref)) end_ref = end_year
	#if (start_ref>= end_ref) print("error") #mbola ampidirina code mamoaka error sy manapaka ny script eto
	ref_year_name <- sapply(start_ref: end_ref, toString)
	ref_dat <- dat[ref_year_name]
	res <- spi_gamma(dat, ref_dat)
	if(return_last_value){
		return(tail(res,1))
	}
	else{
		return (res)
	}
}



spi_all <- function (dat, start_year, start_month = 1,time_scale = 1, start_ref=NULL, end_ref= NULL, asMatrix = TRUE )
{
	#prepare dataset 
		#fill month before and after
		fill_before <- rep(NA, (start_month - 1))
		dat2 <- c(fill_before, dat) # data starting from january
		left <- 12 - length(dat) %% 12
		fill_after <- rep(NA, left)
		dat3 <- c(dat2, fill_after)
		# rolling sum base on time_scale and adjust data
		dat4 <- rollsum(dat3, time_scale)
		# fill data after rolling sum
		fill_rollsum <- rep(NA, (time_scale - 1))
		dat_tot<- c(fill_rollsum, dat4)
		# format into matrix
		mat <- matrix(dat_tot, ncol=12, byrow= TRUE)
		end_year <- start_year + nrow(mat) - 1
		row.names(mat) <- start_year:end_year
		
	# calculate spi per month
	res <- mat
	res[] <- NA
	for (i in 1:12)
	{
		
		res[,i] <- spi_month(mat[,i], start_year, end_year, start_ref, end_ref)
	}
	if (asMatrix) return (res)
	else{
	res <- t(res)
	if (length(fill_before) > 0) 
	{	
			to_drop_bef <- -1 * (1: length(fill_before))
			res <- res[to_drop_bef]
		}
		if(length(fill_after)> 0){
			res <- res[1 : length(dat)]
		}	
		return (res)
	}
}


