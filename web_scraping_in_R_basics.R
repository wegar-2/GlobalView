library(xml2)
library(pingr)
library(RCurl)
library(rvest)
library(data.table)
library(tidyverse)
library(futile.logger)

flog.threshold(INFO)

# cUrl <- "https://www.treasury.gov/resource-center/data-chart-center/interest-rates/pages/TextView.aspx?data=yieldYear&year=2019"
objScrapeAllTablesAtUrl <- function(cUrl) {
  # 1. check if valid url
  if (is.null(cUrl)) {
    stop("Error inside the objScrapeAllTablesAtUrl function - the input", 
         " parameter cannot be NULL")
  }
  if (!RCurl::url.exists(url = cUrl)) {
    stop("Error inside the objScrapeAllTablesAtUrl function - the input", 
         " URL is not a valid URL!")
  }
  
  # 2. create web page object
  flog.trace(msg = "Inside objScrapeAllTablesAtUrl call - creating web page representation...")
  objWebPage <- xml2::read_html(x = cUrl)
  
  # 3. pull the tables from the webpage
  flog.trace(msg = "Inside objScrapeAllTablesAtUrl call - pulling tables from the indicated web page...")
  lTables <- rvest::html_table(x = objWebPage, fill = TRUE)
  flog.trace(msg = paste0("Inside objScrapeAllTablesAtUrl call - successfully pulled ", 
                          as.character(length(lTables)), " tables from the indicated URL"))
  return(lTables)
}
# res <- objScrapeAllTablesAtUrl(cUrl = cUrl)


# use the function to pull the data for years 2000-2019 (twenty years)
lYieldCurveData <- vector(mode = "list", length = 20L)
names(lYieldCurveData) <- as.character(seq(2000, 2019, 1))
for (iIterYear in 2000:2019) {
  # iIterYear <- 2000
  flog.info(msg = paste0("Fetching data for year: ", as.character(iIterYear)))
  cIterUrl <- paste0(
    "https://www.treasury.gov/resource-center/data-chart-center/interest-rates/pages/TextView.aspx?data=yieldYear&year=",
    as.character(iIterYear))
  lRes <- objScrapeAllTablesAtUrl(cUrl = cIterUrl)
  lYieldCurveData[[as.character(iIterYear)]] <- lRes[[2]]
}

lYieldCurveData <- lapply(X = lYieldCurveData, FUN = data.table::as.data.table)
dtYieldCurveData <- data.table::rbindlist(l = lYieldCurveData, use.names = TRUE, 
                                          idcol = "my_id_col", fill = TRUE)
write.csv(x = dtYieldCurveData, file = "UsaYieldCurveTimeSeries.csv",
          quote = TRUE, sep = ",", dec = ".", row.names = FALSE)
