# Standardized precipitation index
```
spi_res <- spi_all(data, start_year=1981, start_month=2, time_scale = 3, asMatrix = TRUE) # calculate spi for all data with result in matrix format
spi_res <- spi_all(data, start_year=1981, start_month=2, time_scale = 3, asMatrix = FALSE) # calculate spi for all data with result in vector format
spi_res <- spi_month(data, start_year=1981, start_ref = 1991, end_ref= 2020, return_last_value = FALSE) # calculate spi for a specific month, returning all value
spi_res <- spi_month(data, start_year=1981, start_ref = 1991, end_ref= 2020, return_last_value = TRUE) # calculate spi for a specific month, returning only the last value


```

Calculation of standardized precipitation index (SPI)

# About
This script calculate the standardized precipitation index for any time scale using gamma distribution. </br>
The data needed for the calculation is:
* rainfall of any time scale (monthly at least)

There are two type of computation:

* for all the time span (example: January 1991 to March 2023). For this option, the data must be monthly.
* for a specific month (example: only for June rainfall or October-November-December rainfall).For this option, the data can be in any time scale.

Reference period can be specified for the fitting of the precipitations data to gamma distribution. If not, all the data are used for the fitting. 