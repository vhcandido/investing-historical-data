require(httr) # POST, content and some internals
require(xts)
# Must have stringr installed (part of tidyverse)

#' @title Download data from Investing.com
#' 
#' @description Retrieve xts with Investing OHLC data for a given identifier and period
#'
#' @param curr_id Identifier of the instrument
#' @param start_date Starting date to download data
#' @param end_date Ending date
#'
#' @return xts with Investing OHLC data
#'
#' @examples
#' ohlc <- download.investing(2103, start_date = '2017-01-01')
#' 
#' # Plotting (optional)
#' require(dygraphs)
#' dygraph(ohlc) %>% dyCandlestick()
download.investing <- function(curr_id, start_date, end_date = Sys.Date()) {
	if(is.character(start_date)) { start_date <- as.Date(start_date) }
	if(is.character(end_date)) { end_date <- as.Date(end_date) }
	url  <- "https://www.investing.com/instruments/HistoricalDataAjax"

	# Thanks to hadley
	# https://stackoverflow.com/questions/24037411/retrieving-post-requests-from-javascript-and-using-them-with-curl-rcurl
	resp <- POST(url,
	  body = list(
	    action = "historical_data",
	    curr_id = curr_id,
	    st_date = format(start_date, '%m/%d/%Y'),
	    end_date = format(end_date, '%m/%d/%Y'),
	    interval_sec = "Daily"
	  ),
	  add_headers(
	    "user-agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_9_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/33.0.1750.117 Safari/537.36",
	    Origin = "http://www.investing.com",
	    "X-Requested-With" = "XMLHttpRequest"
	  ),
	  multipart = FALSE
	)

	if(attr(resp, 'class') != 'response') { stop("is.response(x) is not TRUE") }
	if(resp$status_code != 200) { stop("Status code != 200") }

	x <- strsplit(content(resp, 'text'), 'historicalTbl')[[1]][2]
	x <- unlist(lapply(
		strsplit(x, '<tr>')[[1]],
		function(r) {
			# stringr::str_match_all return a 2col matrix
			stringr::str_match_all(r, ".*?data-real-value=\\p{quotation mark}(\\d+\\.?\\d*)\\p{quotation mark}.*?")[[1]][,2]
		}))
	
	# Build the timestamp/OHLC matrix
	x <- matrix(as.numeric(x), ncol = 5, byrow = TRUE)
	
	# Put into chronological order
	x <- x[nrow(x):1,]
	
	# Transform timestamp into date
	dates <- as.Date(as.POSIXct(x[,1], origin = '1970-01-01', tz = 'GMT'))
	x <- xts(x[,c(3,4,5,2)], order.by = as.Date(dates))
	colnames(x) <- c('Open', 'High', 'Low', 'Close')
	
	return(x)
}
